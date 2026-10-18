module Main exposing (..)

import Board
import Browser
import Browser.Dom as BD
import Browser.Navigation as Nav
import DeadZones
import Dict
import DocumentCoords as DC exposing (DocumentCoords)
import DragFrom as DF exposing (DragFrom)
import Go exposing (..)
import Help
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List.Extra
import Maybe.Extra
import MovePreview
import Play
import Process
import Regex
import Sgf
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import Svg.Lazy exposing (..)
import Task
import Territory
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
    , onMove : Player
    , stones : Stones
    , ghostStone : Maybe MovePreview.Preview
    , highlightedGroup : List Spot
    , justPlayed : Maybe Spot
    , justRemoved : List Stone
    , dragFrom : DragFrom
    , stagedMove : Maybe MovePreview.Preview
    , lastPointer : Maybe Spot
    , showHelp : Bool
    , showScoring : Bool
    , navKey : Nav.Key
    }


emptyModel : Nav.Key -> Model
emptyModel navKey =
    { record = []
    , onMove = Black
    , stones = Dict.empty
    , ghostStone = Nothing
    , highlightedGroup = []
    , justPlayed = Nothing
    , justRemoved = []
    , dragFrom = DF.DragNone
    , stagedMove = Nothing
    , lastPointer = Nothing
    , showHelp = False
    , showScoring = False
    , navKey = navKey
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo url (emptyModel navKey)



-- UPDATE


type Msg
    = Started DocumentCoords
    | Moved DocumentCoords
    | Finished DocumentCoords
    | ClearTouch
    | StartedBoard (Result BD.Error Spot)
    | MovedBoard (Result BD.Error Spot)
    | FinishedBoard (Result BD.Error Spot)
    | PlayPass
    | RemoveStones
    | Prev
    | Next
    | Help
    | ToggleScoring
    | New
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


handleHover : Model -> Spot -> Model
handleHover model hoverSpot =
    let
        maybeOverStone : Maybe Stone
        maybeOverStone =
            stoneList model.stones
                |> List.Extra.find (\s -> isWithinStone hoverSpot s.spot)

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
                group =
                    Play.group model.stones over
            in
            { model
                | highlightedGroup = List.map .spot group
                , ghostStone = Nothing
                , justPlayed = Nothing
            }

        ( Nothing, _ ) ->
            -- hovering over an empty spot: show ghost move if possible
            { model
                | ghostStone = playStone hoverSpot model
                , highlightedGroup = []
                , justPlayed = Nothing
            }


handleDrag : Model -> Stone -> Spot -> Model
handleDrag model draggedFrom draggedTo =
    let
        candidateStone : Stone
        candidateStone =
            DF.findDragged model.stones model.onMove draggedFrom draggedTo

        ghostStone : Maybe MovePreview.Preview
        ghostStone =
            if Maybe.map (.stone >> .spot) model.ghostStone == Just candidateStone.spot then
                model.ghostStone

            else
                MovePreview.fromMoves model.stones (Just candidateStone) Nothing
    in
    { model
        | ghostStone = ghostStone
        , highlightedGroup = []
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
                        , highlightedGroup = []
                        , ghostStone = Nothing
                        , stagedMove = Nothing
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


playStone : Spot -> Model -> Maybe MovePreview.Preview
playStone coords model =
    case Play.playNearby model.onMove model.stones coords of
        Nothing ->
            Nothing

        Just result ->
            case model.ghostStone of
                Just previous ->
                    if previous.stone.spot == (Tuple.second result).spot then
                        -- Preserve the Maybe wrapper too: Svg.Lazy compares
                        -- argument references, not structural equality.
                        model.ghostStone

                    else
                        Just (MovePreview.fromResult model.stones result)

                Nothing ->
                    Just (MovePreview.fromResult model.stones result)


filterStaged : Spot -> Model -> Maybe MovePreview.Preview
filterStaged coords model =
    model.stagedMove
        |> Maybe.Extra.filter (\preview -> isWithinStone coords preview.stone.spot)


ifNotFinished : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
ifNotFinished model newModelCmd =
    if isFinished model.record then
        ( model, Cmd.none )

    else
        newModelCmd


ifNoMultiTouch : DocumentCoords -> Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
ifNoMultiTouch documentCoords model newModelCmd =
    if documentCoords.source == DC.Touch && model.dragFrom == DF.DragNone then
        ( model, Cmd.none )

    else
        newModelCmd


withBoardCoords : DocumentCoords -> Model -> (Result BD.Error Spot -> Msg) -> ( Model, Cmd Msg )
withBoardCoords documentCoords model msg =
    if isScoring model then
        ( model, Cmd.none )

    else
        ( model
        , Task.attempt msg <|
            Task.map (DC.toBoardSpot documentCoords) <|
                BD.getElement "board"
        )


handleMoved : Spot -> Model -> Model
handleMoved coords model =
    case ( filterStaged coords model, model.dragFrom ) of
        ( Just _, _ ) ->
            -- hovering over staged move
            model

        ( _, DF.DragStone draggedFrom ) ->
            handleDrag model draggedFrom coords

        _ ->
            -- handle hovering
            let
                gettingCloser : ( MovePreview.Preview, Spot ) -> Bool
                gettingCloser ( staged, lastPointer ) =
                    distanceSquared staged.stone.spot lastPointer > distanceSquared staged.stone.spot coords
            in
            case Maybe.map2 Tuple.pair model.stagedMove model.lastPointer |> Maybe.Extra.filter gettingCloser of
                Just _ ->
                    -- Keep the staged move without eagerly computing a hover.
                    model

                Nothing ->
                    handleHover { model | stagedMove = Nothing } coords


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Started documentCoords ->
            withBoardCoords documentCoords model StartedBoard

        Moved documentCoords ->
            ifNoMultiTouch documentCoords model <|
                withBoardCoords documentCoords model MovedBoard

        Finished documentCoords ->
            ifNoMultiTouch documentCoords model <|
                withBoardCoords documentCoords model FinishedBoard

        ClearTouch ->
            ( { model
                | dragFrom = DF.DragNone
                , ghostStone = Nothing
              }
            , Cmd.none
            )

        StartedBoard (Ok coords) ->
            ( { model
                | dragFrom = DF.getDragFrom model.stones model.onMove coords
                , stagedMove = filterStaged coords model
              }
            , Cmd.none
            )

        MovedBoard (Ok coords) ->
            let
                setLastPointer m =
                    { m | lastPointer = Just coords }
            in
            ( handleMoved coords model |> setLastPointer, Cmd.none )

        FinishedBoard (Ok coords) ->
            if model.dragFrom == DF.DragSpot coords then
                let
                    cleared =
                        { model
                            | dragFrom = DF.DragNone
                            , stagedMove = Nothing
                            , ghostStone = Nothing
                        }
                in
                case filterStaged coords model of
                    Just staged ->
                        handlePlay cleared staged.stone.spot

                    Nothing ->
                        handlePlay cleared coords

            else
                ( { model
                    | dragFrom = DF.DragNone
                    , stagedMove = model.ghostStone
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

        ToggleScoring ->
            ( { model
                | showScoring = not model.showScoring
                , ghostStone = Nothing
                , stagedMove = Nothing
                , highlightedGroup = []
                , dragFrom = DF.DragNone
                , lastPointer = Nothing
              }
            , Cmd.none
            )

        UrlChanged url ->
            changeRouteTo url model

        _ ->
            ( model, Cmd.none )



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
                |> Maybe.andThen (.submatches >> List.head)
                |> Maybe.andThen identity
                |> Maybe.map (Sgf.decode >> List.reverse)

        createModel : List Play -> Model
        createModel record =
            { model
                | record = record
                , stones = Play.playStones (List.reverse record) Dict.empty
                , highlightedGroup = []
                , ghostStone = Nothing
                , stagedMove = Nothing
                , dragFrom = DF.DragNone
                , lastPointer = Nothing
                , onMove =
                    List.head record
                        |> Maybe.map (.player >> otherPlayer)
                        |> Maybe.withDefault Black
            }

        empty =
            emptyModel model.navKey

        newModel =
            Maybe.map createModel maybeRecord
                |> Maybe.withDefault { empty | showScoring = model.showScoring }
    in
    ( newModel, Cmd.none )


updatePosition : Model -> ( Model, Cmd Msg )
updatePosition model =
    ( { model | ghostStone = Nothing, stagedMove = Nothing, highlightedGroup = [] }
    , pushUrl model.navKey model.record
    )


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


isScoring : Model -> Bool
isScoring model =
    model.showScoring || isFinished model.record


viewSvg : Model -> Svg Msg
viewSvg model =
    lazy7 viewPosition
        model.stones
        (Maybe.Extra.or model.stagedMove model.ghostStone)
        model.highlightedGroup
        model.justPlayed
        model.justRemoved
        (isScoring model)
        (Maybe.Extra.isJust model.stagedMove)


{-| Pointer coordinates and drag bookkeeping do not change the position. Keep
this whole subtree lazy so those updates cannot rebuild previews or SVG masks.
-}
viewPosition : Stones -> Maybe MovePreview.Preview -> List Spot -> Maybe Spot -> List Stone -> Bool -> Bool -> Svg Msg
viewPosition stones candidate group justPlayed justRemoved scoring staged =
    let
        preview =
            if scoring then
                Nothing

            else
                candidate

        previewStones =
            Maybe.map .stones preview |> Maybe.withDefault stones

        previewStone =
            Maybe.map .stone preview

        highlightedGroup =
            Maybe.map .group preview |> Maybe.withDefault group

        survivingLink ( a, b ) =
            Dict.member ( a.x, a.y ) previewStones && Dict.member ( b.x, b.y ) previewStones

        stoneClass stone =
            if Dict.member (stoneKey stone) previewStones then
                classJustPlayed justPlayed stone

            else
                "pending-capture"
    in
    Svg.svg [ SA.viewBox ("0 0 " ++ String.fromInt Board.coordRange ++ " " ++ String.fromInt Board.coordRange) ]
        [ Svg.defs []
            [ Svg.filter [ SA.id "blur-filter" ]
                [ Svg.feGaussianBlur [ SA.in_ "SourceGraphic", SA.stdDeviation "70" ] [] ]
            ]
        , lazy3 Svg.node "g" [ SA.id "lines" ] <| Board.viewLines
        , lazy3 Svg.node "g" [ SA.id "stars" ] <| Board.viewStars
        , lazy3 Svg.Keyed.node "g" [ SA.id "hide-lines", SA.filter "url(#blur-filter)" ] <|
            List.map hideLinesKeyed (stoneList stones)
        , if scoring then
            lazy Territory.view stones

          else
            lazy DeadZones.view previewStones
        , if scoring then
            Svg.g [] []

          else
            lazy2 DeadZones.viewLiberties previewStones highlightedGroup
        , lazy3 Svg.node "g" [ SA.id "ghost-links" ] <|
            List.map viewGhostLink (Maybe.map getStoneLinks previewStone |> Maybe.withDefault [])
        , lazy3 Svg.node "g" [ SA.id "ghost-stone" ] <|
            List.filterMap identity [ Maybe.map viewGhostStone previewStone ]
        , lazy3 Svg.Keyed.node "g" [ SA.id "links" ] <|
            List.map viewKeyedLink (List.filter survivingLink (getUniqueLinks stones))
        , lazy3 Svg.Keyed.node "g" [ SA.id "stones" ] <|
            (stoneList stones |> List.map (\s -> viewKeyedStone (stoneClass s) s))
        , lazy3 Svg.node "g" [ SA.id "staged-stone" ] <|
            if staged then
                List.filterMap identity [ Maybe.map viewStagedStone previewStone ]

            else
                []
        , lazy3 Svg.node "g" [ SA.id "capture-preview", SA.pointerEvents "none" ] <|
            List.map viewCapturePreview (Maybe.map .captured preview |> Maybe.withDefault [])
        , lazy3 Svg.node "g" [ SA.id "removed-stones" ] <|
            (justRemoved |> List.map (viewStone "removed"))
        , lazy3 Svg.node "g" [ SA.id "highlights" ] <|
            (highlightedGroup
                |> List.filter (\spot -> Just spot /= Maybe.map .spot previewStone)
                |> List.map viewHighlight
            )
        ]


viewCapturePreview : Stone -> Svg msg
viewCapturePreview { spot } =
    let
        offset =
            Board.stoneRadius // 2
    in
    Svg.g []
        [ viewLink ( Spot (spot.x - offset) (spot.y - offset), Spot (spot.x + offset) (spot.y + offset) )
        , viewLink ( Spot (spot.x - offset) (spot.y + offset), Spot (spot.x + offset) (spot.y - offset) )
        ]


menuLink : msg -> String -> String -> H.Html msg
menuLink action iconText tooltip =
    H.div [ HA.class "item" ]
        [ H.div [ HA.class "icon", HE.onClick action ]
            [ H.text iconText, H.span [ HA.class "tooltip" ] [ H.text <| " " ++ tooltip ] ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        title =
            case List.length model.record of
                0 ->
                    "ContinuGo: freed from the tyranny of the grid"

                n ->
                    "ContinuGo: #" ++ (String.fromInt <| n)

        score : H.Html msg
        score =
            if isScoring model then
                let
                    territory =
                        Territory.areas model.stones
                in
                H.div [ HA.id "score" ]
                    [ H.div [ HA.class "score-result" ] [ H.text (Territory.resultString territory) ]
                    , H.div [ HA.class "score-areas" ]
                        [ H.text ("Black " ++ Territory.formatArea territory.black ++ " · White " ++ Territory.formatArea territory.white) ]
                    ]

            else
                H.div [] []

        content =
            if model.showHelp then
                H.div [ HA.id "help" ] [ Help.help ]

            else
                H.div
                    [ HA.id "board-container"
                    , HE.on "mousedown" <| DC.decodeMouse Started
                    , HE.on "mousemove" <| DC.decodeMouse Moved
                    , HE.on "mouseup" <| DC.decodeMouse Finished
                    , HE.on "touchstart" <| DC.decodeTouch Started
                    , HE.preventDefaultOn "touchmove" <| DC.decodeSingleTouch ClearTouch Moved
                    , HE.on "touchend" <| DC.decodeChangedTouch Finished
                    , HE.on "touchcancel" <| DC.decodeChangedTouch Finished
                    ]
                    [ H.div
                        [ HA.id "board", HA.classList [ ( "scoring", isScoring model ) ] ]
                        [ viewSvg model
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
            , H.div [ HA.class "item" ]
                [ H.button
                    [ HA.id "score-toggle"
                    , HA.classList [ ( "icon", True ), ( "active", isScoring model ) ]
                    , HA.attribute "aria-label" "Toggle score estimator"
                    , HA.attribute "aria-pressed"
                        (if isScoring model then
                            "true"

                         else
                            "false"
                        )
                    , HA.disabled (isFinished model.record)
                    , HE.onClick ToggleScoring
                    ]
                    [ H.text "Σ", H.span [ HA.class "tooltip" ] [ H.text " score estimator" ] ]
                ]
            , menuLink Prev "‹" "undo"
            , menuLink Next "︎︎›︎" "redo"
            ]
        , content
        ]
    }
