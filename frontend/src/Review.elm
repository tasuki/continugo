module Review exposing (Model, add, annotate, atDepth, children, empty, fromTrees, line, select, selectSibling, siblings, switch)

{-| The first continuation is the game. Selection never reorders the tree.
Remembered paths let stepping back and forward retrace an explored line.
-}

import Rules.Position exposing (Move(..), Play)
import Sgf exposing (Tree(..))


type alias Model =
    { trees : List Tree
    , selected : List Int
    , remembered : List (List Int)
    }


empty : Model
empty =
    fromTrees []


fromTrees : List Tree -> Model
fromTrees trees =
    select [] { trees = trees, selected = [], remembered = [] }


children : List Int -> List Tree -> List Tree
children path trees =
    case path of
        [] ->
            trees

        index :: rest ->
            case List.drop index trees |> List.head of
                Just (Node _ _ next) ->
                    children rest next

                Nothing ->
                    []


siblings : Int -> Model -> List ( Int, Tree )
siblings depth model =
    if depth <= 0 then
        []

    else
        let
            current =
                List.drop (depth - 1) model.selected |> List.head
        in
        children (List.take (depth - 1) model.selected) model.trees
            |> List.indexedMap Tuple.pair
            |> List.filter (\( index, _ ) -> Just index /= current)


selectSibling : Int -> Int -> Model -> Model
selectSibling depth index model =
    if siblings depth model |> List.any (Tuple.first >> (==) index) then
        select (List.take (depth - 1) model.selected ++ [ index ]) model

    else
        model


complete : List Int -> List Tree -> List Int
complete path trees =
    case children path trees of
        [] ->
            path

        _ ->
            complete (path ++ [ 0 ]) trees


select : List Int -> Model -> Model
select requested model =
    let
        valid candidatePath trees =
            case candidatePath of
                [] ->
                    True

                index :: rest ->
                    if index < 0 then
                        False

                    else
                        case List.drop index trees |> List.head of
                            Just (Node _ _ next) ->
                                valid rest next

                            Nothing ->
                                False

        path =
            if valid requested model.trees then
                requested

            else
                []

        selected =
            model.remembered
                |> List.filter (\candidate -> List.take (List.length path) candidate == path)
                |> List.head
                |> Maybe.withDefault path
                |> (\candidate -> complete candidate model.trees)
    in
    { model | selected = selected, remembered = selected :: List.filter ((/=) selected) model.remembered }


line : List Sgf.Property -> Model -> Sgf.Record
line root model =
    let
        walk path trees =
            case path of
                index :: rest ->
                    case List.drop index trees |> List.head of
                        Just (Node play properties next) ->
                            let
                                record =
                                    walk rest next
                            in
                            { record | plays = play :: record.plays, moveProperties = properties :: record.moveProperties }

                        Nothing ->
                            { root = root, plays = [], moveProperties = [] }

                [] ->
                    { root = root, plays = [], moveProperties = [] }
    in
    walk model.selected model.trees


atDepth : Int -> List Tree -> List (List Int)
atDepth depth trees =
    if depth <= 0 then
        [ [] ]

    else
        trees
            |> List.indexedMap (\index (Node _ _ next) -> atDepth (depth - 1) next |> List.map ((::) index))
            |> List.concat


switch : Int -> Int -> Model -> Model
switch direction depth model =
    let
        paths =
            atDepth depth model.trees

        current =
            List.take depth model.selected

        find index remaining =
            case remaining of
                first :: rest ->
                    if first == current then
                        index

                    else
                        find (index + 1) rest

                [] ->
                    0

        target =
            clamp 0 (List.length paths - 1) (find 0 paths + direction)
    in
    List.drop target paths |> List.head |> Maybe.map (\path -> select path model) |> Maybe.withDefault model


{-| Replace supplied properties on the selected move, preserving other metadata.
-}
annotate : Int -> List Sgf.Property -> Model -> Model
annotate depth properties model =
    let
        update path trees =
            case path of
                index :: rest ->
                    List.indexedMap
                        (\i (Node play existing next) ->
                            if i /= index then
                                Node play existing next

                            else if List.isEmpty rest then
                                Node play (properties ++ List.filter (\( name, _ ) -> not (List.any (Tuple.first >> (==) name) properties)) existing) next

                            else
                                Node play existing (update rest next)
                        )
                        trees

                [] ->
                    trees
    in
    { model | trees = update (List.take depth model.selected) model.trees }


sameMove : Play -> Play -> Bool
sameMove a b =
    a.color
        == b.color
        && (case ( a.move, b.move ) of
                ( Pass, Pass ) ->
                    True

                ( Place first, Place second ) ->
                    first.spot == second.spot

                _ ->
                    False
           )


add : Int -> Play -> Model -> Model
add depth play model =
    let
        parent =
            List.take depth model.selected

        continuations =
            children parent model.trees

        find index remaining =
            case remaining of
                (Node existing _ _) :: rest ->
                    if sameMove existing play then
                        index

                    else
                        find (index + 1) rest

                [] ->
                    index

        target =
            find 0 continuations

        insert path trees =
            case path of
                [] ->
                    if target < List.length trees then
                        trees

                    else
                        trees ++ [ Node play [] [] ]

                first :: rest ->
                    List.indexedMap
                        (\i ((Node move properties next) as tree) ->
                            if i == first then
                                Node move properties (insert rest next)

                            else
                                tree
                        )
                        trees
    in
    select (parent ++ [ target ]) { model | trees = insert parent model.trees }
