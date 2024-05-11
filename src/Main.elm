module Main exposing (..)

import Board
import Browser
import Browser.Dom as BD
import Browser.Navigation as Nav
import Dict
import Go exposing (..)
import Help
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Maybe.Extra
import Play
import Process
import Regex
import Sgf
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import Svg.Lazy exposing (..)
import Task
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { record : List Play
    , stones : Stones
    , ghostStone : Maybe Stone
    , highlightedGroup : List Spot
    , highlightedLiberties : List Spot
    , justPlayed : Maybe Spot
    , justRemoved : List Stone
    , onMove : Player
    , startedTouching : Maybe Spot
    , stagedMove : Maybe Stone
    , showHelp : Bool
    , navKey : Nav.Key
    }


emptyModel : Nav.Key -> Model
emptyModel navKey =
    { record = []
    , stones = Dict.empty
    , ghostStone = Nothing
    , highlightedGroup = []
    , highlightedLiberties = []
    , justPlayed = Nothing
    , justRemoved = []
    , onMove = Black
    , startedTouching = Nothing
    , stagedMove = Nothing
    , showHelp = False
    , navKey = navKey
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo url (emptyModel navKey)



-- UPDATE


type alias WindowCoords =
    { x : Int, y : Int }


type Msg
    = Started WindowCoords
    | Moved WindowCoords
    | Finished WindowCoords
    | StartedBoard WindowCoords (Result BD.Error BD.Element)
    | MovedBoard WindowCoords (Result BD.Error BD.Element)
    | FinishedBoard WindowCoords (Result BD.Error BD.Element)
    | PlayPass
    | RemoveStones
    | Prev
    | Next
    | Help
    | New
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


handleHover : Model -> Spot -> Model
handleHover model hoverSpot =
    let
        maybeOverStone : Maybe Stone
        maybeOverStone =
            stoneList model.stones
                |> List.filter (\s -> distance hoverSpot s.spot < stoneR)
                |> List.head

        memberOfHighlighted : Stone -> Bool
        memberOfHighlighted stone =
            List.member stone.spot model.highlightedGroup

        overAlreadyShown : Bool
        overAlreadyShown =
            Maybe.map memberOfHighlighted maybeOverStone |> Maybe.withDefault False

        overJustPlayed : Bool
        overJustPlayed =
            Maybe.map .spot maybeOverStone == model.justPlayed
    in
    case ( maybeOverStone, overJustPlayed || overAlreadyShown ) of
        ( Just _, True ) ->
            -- hovering over the stone which was just played OR
            -- hovering over the stone whose liberties are already shown
            -- show nothing
            model

        ( Just over, False ) ->
            -- hovering over a stone: show liberties
            let
                ( group, liberties ) =
                    Play.groupAndItsLiberties model.stones over
            in
            { model
                | highlightedGroup = List.map .spot group
                , highlightedLiberties = liberties
                , ghostStone = Nothing
                , justPlayed = Nothing
            }

        ( Nothing, _ ) ->
            -- hovering over an empty spot: show ghost move if possible
            { model
                | ghostStone =
                    Play.playNearby model.onMove model.stones hoverSpot
                        |> Maybe.map Tuple.second
                , highlightedGroup = []
                , highlightedLiberties = []
                , justPlayed = Nothing
            }


handlePlay : Model -> Spot -> ( Model, Cmd Msg )
handlePlay model playSpot =
    case Play.playNearby model.onMove model.stones playSpot of
        Just ( stones, played ) ->
            -- play!
            let
                play =
                    { player = played.player, move = Place played }

                newModel =
                    { model
                        | record = play :: model.record
                        , stones = stones
                        , onMove = otherPlayer model.onMove
                        , justPlayed = Just played.spot
                        , justRemoved = removedStones model.stones stones
                    }
            in
            ( newModel
            , Cmd.batch
                [ pushUrl newModel.navKey newModel.record
                , Process.sleep 500 |> Task.perform (\_ -> RemoveStones)
                ]
            )

        _ ->
            -- illegal move
            ( model, Cmd.none )


updatePosition : Model -> ( Model, Cmd Msg )
updatePosition model =
    ( model
    , pushUrl model.navKey model.record
    )


ifNotFinished : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
ifNotFinished model inf =
    if isFinished model.record then
        ( model, Cmd.none )

    else
        inf


addBoard : Model -> (Result BD.Error BD.Element -> Msg) -> ( Model, Cmd Msg )
addBoard model msg =
    ifNotFinished model <|
        ( model
        , BD.getElement "board" |> Task.attempt msg
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Started windowCoords ->
            addBoard model (StartedBoard windowCoords)

        Moved windowCoords ->
            addBoard model (MovedBoard windowCoords)

        Finished windowCoords ->
            addBoard model (FinishedBoard windowCoords)

        StartedBoard windowCoords (Ok element) ->
            let
                coords =
                    toBoardCoords windowCoords element
            in
            Maybe.map .spot model.stagedMove
                |> Maybe.Extra.filter (isWithinStone coords)
                |> Maybe.map
                    (handlePlay
                        { model
                            | startedTouching = Just coords
                            , stagedMove = Nothing
                        }
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        MovedBoard windowCoords (Ok element) ->
            let
                coords =
                    toBoardCoords windowCoords element
            in
            if
                Maybe.map .spot model.stagedMove
                    |> Maybe.Extra.filter (isWithinStone coords)
                    |> Maybe.Extra.isJust
            then
                -- hovering over staged move
                ( model, Cmd.none )

            else
                ( handleHover { model | stagedMove = Nothing } coords
                , Cmd.none
                )

        FinishedBoard windowCoords (Ok element) ->
            let
                coords =
                    toBoardCoords windowCoords element
            in
            if model.startedTouching == Just coords then
                handlePlay { model | startedTouching = Nothing } coords

            else
                let
                    stagedMove : Maybe Stone
                    stagedMove =
                        Play.playNearby model.onMove model.stones coords
                            |> Maybe.map Tuple.second
                in
                ( { model
                    | startedTouching = Nothing
                    , stagedMove = stagedMove
                  }
                , Cmd.none
                )

        PlayPass ->
            let
                record =
                    { player = model.onMove, move = Pass } :: model.record
            in
            ifNotFinished model <|
                updatePosition
                    { model
                        | record = record
                        , onMove = otherPlayer model.onMove
                    }

        RemoveStones ->
            ( { model | justRemoved = [] }
            , Cmd.none
            )

        Prev ->
            ( model
            , Nav.back model.navKey 1
            )

        Next ->
            ( model
            , Nav.forward model.navKey 1
            )

        New ->
            updatePosition (emptyModel model.navKey)

        Help ->
            ( { model | showHelp = not model.showHelp }
            , Cmd.none
            )

        UrlChanged url ->
            changeRouteTo url model

        _ ->
            ( model, Cmd.none )


toBoardCoords : WindowCoords -> BD.Element -> Spot
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



-- ROUTING / URL


recordRegex : Regex.Regex
recordRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^record=(.*)$"


changeRouteTo : Url.Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    let
        maybeRecord : Maybe (List Play)
        maybeRecord =
            Maybe.map (Regex.find recordRegex) url.query
                |> Maybe.andThen List.head
                |> Maybe.map .submatches
                |> Maybe.andThen List.head
                |> Maybe.andThen identity
                |> Maybe.map (Sgf.decode >> List.reverse)

        createModel : List Play -> Model
        createModel record =
            { model
                | record = record
                , stones = Play.playStones (List.reverse record) Dict.empty
                , onMove =
                    List.head record
                        |> Maybe.map .player
                        |> Maybe.map otherPlayer
                        |> Maybe.withDefault Black
            }

        newModel =
            Maybe.map createModel maybeRecord
                |> Maybe.withDefault (emptyModel model.navKey)
    in
    ( newModel, Cmd.none )


pushUrl : Nav.Key -> List Play -> Cmd msg
pushUrl navKey record =
    let
        rec =
            if List.length record > 0 then
                -- Bravely go where no legal URL has gone before.
                -- I prefer URLs to look nice rather than be valid.
                "?record=" ++ (Sgf.encode <| List.reverse record)

            else
                "?"
    in
    Nav.pushUrl navKey rec



-- VIEW


viewSvg : Model -> List (Svg Msg)
viewSvg model =
    [ Svg.defs []
        [ Svg.filter [ SA.id "blur-filter" ]
            [ Svg.feGaussianBlur [ SA.in_ "SourceGraphic", SA.stdDeviation "70" ] [] ]
        ]
    , lazy3 Svg.node "g" [ SA.id "lines" ] <| Board.viewLines
    , lazy3 Svg.node "g" [ SA.id "stars" ] <| Board.viewStars
    , lazy3 Svg.Keyed.node "g" [ SA.id "hide-lines", SA.filter "url(#blur-filter)" ] <|
        List.map hideLinesKeyed (stoneList model.stones)
    , lazy3 Svg.node "g" [ SA.id "liberties" ] <|
        (model.highlightedLiberties |> List.map viewLiberty)
    , lazy3 Svg.node "g" [ SA.id "ghost-links" ] <|
        List.map viewGhostLink (Maybe.map getStoneLinks model.ghostStone |> Maybe.withDefault [])
    , lazy3 Svg.node "g" [ SA.id "ghost-stone" ] <|
        List.filterMap identity [ Maybe.map viewGhostStone model.ghostStone ]
    , lazy3 Svg.Keyed.node "g" [ SA.id "links" ] <|
        List.map viewKeyedLink (getUniqueLinks model.stones)
    , lazy3 Svg.Keyed.node "g" [ SA.id "stones" ] <|
        (stoneList model.stones |> List.map (\s -> viewKeyedStone (classJustPlayed model.justPlayed s) s))
    , lazy3 Svg.node "g" [ SA.id "staged-stone" ] <|
        List.filterMap identity [ Maybe.map viewStagedStone model.stagedMove ]
    , lazy3 Svg.node "g" [ SA.id "removed-stones" ] <|
        (model.justRemoved |> List.map (viewStone "removed"))
    , lazy3 Svg.node "g" [ SA.id "highlights" ] <|
        (model.highlightedGroup |> List.map viewHighlight)
    ]


menuLink : msg -> String -> String -> H.Html msg
menuLink action iconText tooltip =
    H.div [ HA.class "item" ]
        [ H.div [ HA.class "icon", HE.onClick action ]
            [ H.text iconText, H.span [ HA.class "tooltip" ] [ H.text <| " " ++ tooltip ] ]
        ]


decodeMouse : (Spot -> msg) -> D.Decoder msg
decodeMouse msg =
    D.map msg
        (D.map2 Spot
            (D.field "pageX" D.int)
            (D.field "pageY" D.int)
        )


decodeTouch : (Spot -> msg) -> D.Decoder msg
decodeTouch msg =
    D.map msg
        (D.map2 Spot
            (D.at [ "touches", "0" ] <| D.field "pageX" D.int)
            (D.at [ "touches", "0" ] <| D.field "pageY" D.int)
        )


decodeChangedTouch : (Spot -> msg) -> D.Decoder msg
decodeChangedTouch msg =
    D.map msg
        (D.map2 Spot
            (D.at [ "changedTouches", "0" ] <| D.field "pageX" D.int)
            (D.at [ "changedTouches", "0" ] <| D.field "pageY" D.int)
        )


view : Model -> Browser.Document Msg
view model =
    let
        intsToStr ints =
            List.map String.fromInt ints |> String.join " "

        title =
            case List.length model.record of
                0 ->
                    "ContinuGo: freed from the tyranny of the grid"

                n ->
                    "ContinuGo: #" ++ (String.fromInt <| n)

        score : H.Html msg
        score =
            if isFinished model.record then
                H.div [ HA.id "score" ] [ H.text <| resultString model.stones ]

            else
                H.div [] []

        content =
            if model.showHelp then
                H.div [ HA.id "help" ] [ Help.help ]

            else
                H.div
                    [ HA.id "board-container"
                    , HE.on "mousedown" <| decodeMouse Started
                    , HE.on "mousemove" <| decodeMouse Moved
                    , HE.on "mouseup" <| decodeMouse Finished
                    , HE.on "touchstart" <| decodeTouch Started
                    , HE.on "touchmove" <| decodeTouch Moved
                    , HE.on "touchend" <| decodeChangedTouch Finished
                    , HE.on "touchcancel" <| decodeChangedTouch Finished
                    ]
                    [ H.div
                        [ HA.id "board" ]
                        [ Svg.svg
                            [ SA.viewBox <| intsToStr [ 0, 0, coordRange, coordRange ] ]
                            (viewSvg model)
                        , score
                        ]
                    ]
    in
    { title = title
    , body =
        [ H.div [ HA.id "menu" ]
            [ menuLink New "!" "new"
            , menuLink Help "?" "help"
            , menuLink PlayPass "*" "pass"
            , menuLink Prev "‹" "undo"
            , menuLink Next "︎︎›︎" "redo"
            ]
        , content
        ]
    }
