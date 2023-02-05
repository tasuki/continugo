module Main exposing (..)

import Browser
import Browser.Dom as BD
import Browser.Events
import Dict
import Go exposing (..)
import Html as H
import Html.Attributes as HA
import Json.Decode as D
import Liberties exposing (findNearestPlayable)
import Maybe.Extra
import Play exposing (groupAndItsLiberties, playIfLegal, playStones)
import Process
import SamplePositions
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import Svg.Lazy exposing (..)
import Task


boardSize =
    13


preciseR =
    coordRange / (boardSize * 2)


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { stones : Stones
    , ghostStone : Maybe Stone
    , highlightedGroup : List Spot
    , highlightedLiberties : List Spot
    , justPlayed : Maybe Spot
    , justRemoved : List Stone
    , onMove : Player
    }


init : () -> ( Model, Cmd Msg )
init _ =
    update (PlayStones SamplePositions.empty)
        { stones = Dict.empty
        , ghostStone = Nothing
        , highlightedGroup = []
        , highlightedLiberties = []
        , justPlayed = Nothing
        , justRemoved = []
        , onMove = Black
        }



-- UPDATE


type Msg
    = Clicked Spot
    | MouseMoved Spot
    | PlayIfLegal Spot (Result BD.Error BD.Element)
    | Hover Spot (Result BD.Error BD.Element)
    | PlayStones (List Stone)
    | RemoveStones


handleHover : Model -> Spot -> Model
handleHover model hoverSpot =
    let
        nearby =
            nearbyStones model.stones hoverSpot

        maybeOver =
            nearby
                |> List.filter (\s -> distance hoverSpot s.spot < stoneR)
                |> List.head
    in
    case maybeOver of
        Just over ->
            -- hovering over a stone
            let
                overJustPlayed =
                    Maybe.Extra.filter ((==) over.spot) model.justPlayed
            in
            case overJustPlayed of
                Just _ ->
                    -- hovering over a stone which was just played: show nothing
                    model

                Nothing ->
                    -- hovering over a stone: show liberties
                    let
                        ( group, liberties ) =
                            groupAndItsLiberties model.stones over
                    in
                    { model
                        | highlightedGroup = group
                        , highlightedLiberties = liberties
                        , ghostStone = Nothing
                        , justPlayed = overJustPlayed
                    }

        Nothing ->
            -- hovering over an empty spot: show ghost move if possible
            let
                maybePlay : Spot -> Maybe Stone
                maybePlay spot =
                    playIfLegal (createStone model.onMove spot) model.stones
                        |> Maybe.andThen (Dict.get ( spot.x, spot.y ))
            in
            { model
                | ghostStone =
                    findNearestPlayable hoverSpot (List.map .spot nearby)
                        |> Maybe.andThen maybePlay
                , highlightedGroup = []
                , highlightedLiberties = []
                , justPlayed = Nothing
            }


handlePlay : Model -> Spot -> ( Model, Cmd Msg )
handlePlay model playSpot =
    let
        nearby =
            nearbyStones model.stones playSpot

        maybeStones =
            findNearestPlayable playSpot (List.map .spot nearby)
                |> Maybe.andThen (\np -> playIfLegal (createStone model.onMove np) model.stones)
    in
    case maybeStones of
        Just stones ->
            -- play!
            ( { model
                | stones = stones
                , onMove = otherPlayer model.onMove
                , justPlayed = Just playSpot
                , justRemoved = removedStones model.stones stones
              }
            , Process.sleep 500 |> Task.perform (\_ -> RemoveStones)
            )

        Nothing ->
            -- illegal move
            ( model, Cmd.none )


removedStones : Stones -> Stones -> List Stone
removedStones old new =
    let
        maybeMissingStone : Stone -> Maybe Stone
        maybeMissingStone oldStone =
            case Dict.get (stoneKey oldStone) new of
                Just _ ->
                    Nothing

                Nothing ->
                    Just oldStone
    in
    List.filterMap maybeMissingStone (stoneList old)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked clickedCoords ->
            ( model
            , BD.getElement "board" |> Task.attempt (PlayIfLegal clickedCoords)
            )

        PlayIfLegal clickedCoords (Ok element) ->
            handlePlay model (toBoardCoords clickedCoords element)

        MouseMoved hoverCoords ->
            ( model
            , BD.getElement "board" |> Task.attempt (Hover hoverCoords)
            )

        Hover hoverCoords (Ok element) ->
            ( handleHover model (toBoardCoords hoverCoords element)
            , Cmd.none
            )

        PlayStones stonesList ->
            ( { model | stones = playStones stonesList model.stones }
            , Cmd.none
            )

        RemoveStones ->
            ( { model | justRemoved = [] }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


toBoardCoords : Spot -> BD.Element -> Spot
toBoardCoords windowCoords element =
    { x =
        round <|
            (toFloat windowCoords.x - element.element.x)
                * (toFloat coordRange / element.element.width)
    , y =
        round <|
            (toFloat windowCoords.y - element.element.y)
                * (toFloat coordRange / element.element.height)
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onClick
            (D.map Clicked
                (D.map2 Spot
                    (D.field "pageX" D.int)
                    (D.field "pageY" D.int)
                )
            )
        , Browser.Events.onMouseMove
            (D.map MouseMoved
                (D.map2 Spot
                    (D.field "pageX" D.int)
                    (D.field "pageY" D.int)
                )
            )
        ]



-- VIEW


offset : Float -> Float
offset lines =
    lines * preciseR * 2 - preciseR


viewLines : List (Svg msg)
viewLines =
    let
        line x1 y1 x2 y2 =
            Svg.line
                [ SA.x1 <| String.fromFloat x1
                , SA.y1 <| String.fromFloat y1
                , SA.x2 <| String.fromFloat x2
                , SA.y2 <| String.fromFloat y2
                , SA.stroke "#993"
                , SA.strokeWidth "1.5"
                ]
                []

        ( start, end ) =
            ( preciseR, offset boardSize )

        offsets =
            List.range 1 boardSize |> List.map toFloat

        horizontal =
            List.map (\o -> line start (offset o) end (offset o)) offsets

        vertical =
            List.map (\o -> line (offset o) start (offset o) end) offsets
    in
    horizontal ++ vertical


viewStars : List (Svg msg)
viewStars =
    let
        ( a, z ) =
            ( 4, boardSize - 3 )

        star x y =
            Svg.circle
                [ SA.cx <| String.fromFloat (offset x)
                , SA.cy <| String.fromFloat (offset y)
                , SA.r <| String.fromInt 4
                , SA.fill "#993"
                ]
                []
    in
    [ star a a
    , star z a
    , star a z
    , star z z
    ]


hideLines : Stone -> Svg Msg
hideLines { spot } =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt <| stoneR * 7
        , SA.fill "#EEA"
        ]
        []


viewStone : String -> Stone -> Svg Msg
viewStone extraClass { player, spot } =
    let
        class =
            case player of
                Black ->
                    "black"

                White ->
                    "white"
    in
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        , SA.stroke "black"
        , SA.strokeWidth "5"
        , SA.class class
        , SA.class extraClass
        ]
        []


viewHighlight : Spot -> Svg Msg
viewHighlight spot =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        , SA.stroke "#900"
        , SA.strokeWidth "5"
        , SA.fill "transparent"
        ]
        []


viewLiberty : Spot -> Svg Msg
viewLiberty spot =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        , SA.stroke "#BB6"
        , SA.strokeWidth "5"
        , SA.fill "transparent"
        ]
        []


viewLink : ( Spot, Spot ) -> Svg Msg
viewLink ( s1, s2 ) =
    Svg.line
        [ SA.x1 <| String.fromInt s1.x
        , SA.y1 <| String.fromInt s1.y
        , SA.x2 <| String.fromInt s2.x
        , SA.y2 <| String.fromInt s2.y
        , SA.stroke "black"
        , SA.strokeWidth "5"
        ]
        []


viewGhostStone : Stone -> Svg Msg
viewGhostStone stone =
    Svg.g [ SA.opacity "0.4" ] [ viewStone "" stone ]


viewGhostLink : ( Spot, Spot ) -> Svg Msg
viewGhostLink ( ghost, existing ) =
    let
        shiftBy : Float
        shiftBy =
            stoneR / distance ghost existing

        ghostBorder : Spot
        ghostBorder =
            Liberties.findShift ghost existing
                |> Liberties.scaleShift shiftBy
                |> Liberties.shift ghost
    in
    Svg.g [ SA.opacity "0.4" ] [ viewLink ( ghostBorder, existing ) ]



-- Keyed


spotKeyStr : Spot -> String
spotKeyStr s =
    "-" ++ String.fromInt s.x ++ "-" ++ String.fromInt s.y


hideLinesKeyed : Stone -> ( String, Svg Msg )
hideLinesKeyed stone =
    ( "hide" ++ spotKeyStr stone.spot
    , lazy hideLines stone
    )


viewKeyedStone : String -> Stone -> ( String, Svg Msg )
viewKeyedStone extraClass stone =
    ( "stone" ++ spotKeyStr stone.spot
    , lazy2 viewStone extraClass stone
    )


viewKeyedLink : ( Spot, Spot ) -> ( String, Svg Msg )
viewKeyedLink ( s1, s2 ) =
    ( "link" ++ spotKeyStr s1 ++ spotKeyStr s2
    , lazy viewLink ( s1, s2 )
    )


classJustPlayed : Maybe Spot -> Stone -> String
classJustPlayed justPlayed stone =
    if justPlayed == Just stone.spot then
        "just-played"

    else
        ""



-- Main view


view : Model -> Browser.Document Msg
view model =
    let
        intsToStr ints =
            List.map String.fromInt ints |> String.join " "
    in
    { title = ""
    , body =
        [ H.div [ HA.id "board" ]
            [ Svg.svg
                [ SA.viewBox <| intsToStr [ 0, 0, coordRange, coordRange ] ]
                [ Svg.defs []
                    [ Svg.filter [ SA.id "blur-filter" ]
                        [ Svg.feGaussianBlur [ SA.in_ "SourceGraphic", SA.stdDeviation "70" ] [] ]
                    ]
                , lazy3 Svg.node "g" [ SA.id "lines" ] <| viewLines
                , lazy3 Svg.node "g" [ SA.id "viewStars" ] <| viewStars
                , lazy3 Svg.Keyed.node "g" [ SA.id "hideLines", SA.filter "url(#blur-filter)" ] <|
                    List.map hideLinesKeyed (stoneList model.stones)
                , lazy3 Svg.node "g" [ SA.id "ghostLinks" ] <|
                    List.map viewGhostLink (Maybe.map getStoneLinks model.ghostStone |> Maybe.withDefault [])
                , lazy3 Svg.node "g" [ SA.id "ghostStone" ] <|
                    List.filterMap identity [ Maybe.map viewGhostStone model.ghostStone ]
                , lazy3 Svg.Keyed.node "g" [ SA.id "links" ] <|
                    List.map viewKeyedLink (getUniqueLinks model.stones)
                , lazy3 Svg.Keyed.node "g" [ SA.id "stones" ] <|
                    (stoneList model.stones |> List.map (\s -> viewKeyedStone (classJustPlayed model.justPlayed s) s))
                , lazy3 Svg.node "g" [ SA.id "removedStones" ] <|
                    (model.justRemoved |> List.map (viewStone "removed"))
                , lazy3 Svg.node "g" [ SA.id "highlights" ] <|
                    (model.highlightedGroup |> List.map viewHighlight)
                , lazy3 Svg.node "g" [ SA.id "liberties" ] <|
                    (model.highlightedLiberties |> List.map viewLiberty)
                ]
            ]
        ]
    }



-- Links helpers


getStoneLinks : Stone -> List ( Spot, Spot )
getStoneLinks stone =
    List.map (\spots -> ( stone.spot, spots )) stone.adjacent


getUniqueLinks : Stones -> List ( Spot, Spot )
getUniqueLinks stones =
    let
        allLinks : List ( Spot, Spot )
        allLinks =
            stoneList stones
                |> List.concatMap (\s -> List.map (\a -> ( s.spot, a )) s.adjacent)

        -- filter out half of the duplicate links, so as to show each only once
        isChosen : ( Spot, Spot ) -> Bool
        isChosen ( s1, s2 ) =
            if s1.y < s2.y then
                True

            else
                s1.y == s2.y && s1.x < s2.x
    in
    List.filter isChosen allLinks
