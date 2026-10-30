module Main exposing (..)

import Analysis
import BoardView
import Browser
import Browser.Dom as BD
import Browser.Navigation as Nav
import Coverage
import Dict
import DocumentCoords as DC exposing (DocumentCoords)
import DragFrom as DF exposing (DragFrom)
import HelpContent
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import List.Extra
import Markdown
import Maybe.Extra
import MovePreview
import Placement
import PointerInput
import Process
import RecordUrl
import Rules.Board exposing (coordRange, stoneRadius)
import Rules.Game as Game
import Rules.Play as Play
import Rules.Position exposing (..)
import Rules.Territory as Territory
import Sgf
import StoneView exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import Svg.Lazy exposing (..)
import Task
import TerritoryView
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
    { game : Game.Game
    , sgfRoot : List Sgf.Property
    , ghostStone : Maybe MovePreview.Preview
    , highlightedGroup : List Spot
    , justPlayed : Maybe Spot
    , justRemoved : List Stone
    , dragFrom : DragFrom
    , pointer : PointerInput.State
    , stagedMove : Maybe MovePreview.Preview
    , lastPointer : Maybe Spot
    , showHelp : Bool
    , showScoring : Bool
    , showAnalysis : Bool
    , analysis : Analysis.Model
    , navKey : Nav.Key
    }


emptyModel : Nav.Key -> Model
emptyModel navKey =
    { game = Game.empty
    , sgfRoot = Sgf.empty.root
    , ghostStone = Nothing
    , highlightedGroup = []
    , justPlayed = Nothing
    , justRemoved = []
    , dragFrom = DF.DragNone
    , pointer = PointerInput.empty
    , stagedMove = Nothing
    , lastPointer = Nothing
    , showHelp = False
    , showScoring = False
    , showAnalysis = False
    , analysis = Analysis.empty
    , navKey = navKey
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    restoreFromUrl url (emptyModel navKey)



-- UPDATE


type Msg
    = PointerEvent PointerInput.Event
    | PointerAtBoard Int PointerInput.Action (Result BD.Error Spot)
    | ClearAnalysisHover
    | PassTurn
    | ClearCaptureAnimation
    | Undo
    | Redo
    | ToggleHelp
    | ToggleScoring
    | ToggleAnalysis
    | AnalysisRemovalExpired Int
    | NewGame
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


handleHover : Model -> Spot -> Model
handleHover model hoverSpot =
    let
        maybeOverStone : Maybe Stone
        maybeOverStone =
            stoneList model.game.stones
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
                    Play.group model.game.stones over
            in
            { model
                | highlightedGroup = List.map .spot group
                , ghostStone = Nothing
                , justPlayed = Nothing
            }

        ( Nothing, _ ) ->
            -- hovering over an empty spot: show ghost move if possible
            { model
                | ghostStone = previewNearbyMove hoverSpot model
                , highlightedGroup = []
                , justPlayed = Nothing
            }


handleDrag : Model -> Stone -> Spot -> Model
handleDrag model draggedFrom draggedTo =
    let
        candidateStone : Stone
        candidateStone =
            DF.findDragged model.game.stones model.game.onMove draggedFrom draggedTo

        ghostStone : Maybe MovePreview.Preview
        ghostStone =
            if Maybe.map (.stone >> .spot) model.ghostStone == Just candidateStone.spot then
                model.ghostStone

            else
                MovePreview.fromCandidates model.game.stones (Just candidateStone) Nothing
    in
    { model
        | ghostStone = ghostStone
        , highlightedGroup = []
        , justPlayed = Nothing
    }


handleAnalysisFinished : Spot -> Model -> ( Model, Cmd Msg )
handleAnalysisFinished coords model =
    let
        ( analysis, effect ) =
            Analysis.finish model.game.stones coords model.analysis

        updated =
            { model | analysis = analysis }
    in
    case effect of
        Analysis.NoEffect ->
            ( updated, Cmd.none )

        Analysis.ScheduleRemoval serial ->
            ( updated, Process.sleep 300 |> Task.perform (\_ -> AnalysisRemovalExpired serial) )

        Analysis.Propose spot ->
            case MovePreview.fromCandidates model.game.stones (Just (createStone model.game.onMove spot)) Nothing of
                Just preview ->
                    if Game.isFinished model.game then
                        ( { updated | analysis = { analysis | invalid = Just spot } }, Cmd.none )

                    else
                        ( { updated | showAnalysis = False, stagedMove = Just preview, lastPointer = Just spot }, Cmd.none )

                Nothing ->
                    ( { updated | analysis = { analysis | invalid = Just spot } }, Cmd.none )


handlePlay : Model -> Spot -> ( Model, Cmd Msg )
handlePlay model playSpot =
    case
        Placement.nearbySpot model.game.stones playSpot
            |> Maybe.andThen (\spot -> Game.play spot model.game |> Maybe.map (Tuple.pair spot))
    of
        Just ( spot, game ) ->
            let
                newModel =
                    { model
                        | game = game
                        , justPlayed = Just spot
                        , justRemoved = removedStones model.game.stones game.stones
                        , highlightedGroup = []
                        , ghostStone = Nothing
                        , stagedMove = Nothing
                    }
            in
            ( newModel
            , Cmd.batch
                [ pushRecordUrl newModel
                , Process.sleep 500 |> Task.perform (\_ -> ClearCaptureAnimation)
                ]
            )

        _ ->
            -- illegal move
            ( model, Cmd.none )


previewNearbyMove : Spot -> Model -> Maybe MovePreview.Preview
previewNearbyMove coords model =
    case Placement.playNearby model.game.onMove model.game.stones coords of
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
                        Just (MovePreview.fromResult model.game.stones result)

                Nothing ->
                    Just (MovePreview.fromResult model.game.stones result)


stagedMoveAt : Spot -> Model -> Maybe MovePreview.Preview
stagedMoveAt coords model =
    model.stagedMove
        |> Maybe.Extra.filter (\preview -> isWithinStone coords preview.stone.spot)


withBoardCoords : PointerInput.Action -> Model -> DocumentCoords -> ( Model, Cmd Msg )
withBoardCoords action model documentCoords =
    if isScoring model then
        ( model, Cmd.none )

    else
        ( model
        , Task.attempt (PointerAtBoard model.pointer.revision action) <|
            Task.map (DC.toBoardSpot documentCoords) <|
                BD.getElement "board"
        )


handleMoved : Spot -> Model -> Model
handleMoved coords model =
    case ( stagedMoveAt coords model, model.dragFrom ) of
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


cancelBoardGesture : Model -> Model
cancelBoardGesture model =
    { model
        | dragFrom = DF.DragNone
        , ghostStone = Nothing
        , highlightedGroup = []
        , lastPointer = Nothing
        , analysis = Analysis.cancel model.analysis
    }


handlePointerAction : PointerInput.Action -> Model -> ( Model, Cmd Msg )
handlePointerAction action model =
    let
        atBoard =
            withBoardCoords action model
    in
    case action of
        PointerInput.StartAt coords ->
            atBoard coords

        PointerInput.MoveAt coords ->
            atBoard coords

        PointerInput.FinishAt coords ->
            atBoard coords

        PointerInput.CancelGesture ->
            ( cancelBoardGesture model, Cmd.none )


handleBoardPointer : PointerInput.Action -> Spot -> Model -> ( Model, Cmd Msg )
handleBoardPointer action coords model =
    case action of
        PointerInput.StartAt _ ->
            if model.showAnalysis then
                ( { model
                    | analysis = Analysis.start model.game.stones coords model.analysis
                    , dragFrom = DF.DragSpot coords
                  }
                , Cmd.none
                )

            else
                ( { model
                    | dragFrom = DF.getDragFrom model.game.stones model.game.onMove coords
                    , stagedMove = stagedMoveAt coords model
                  }
                , Cmd.none
                )

        PointerInput.MoveAt _ ->
            let
                setLastPointer m =
                    { m | lastPointer = Just coords }
            in
            if model.showAnalysis then
                ( { model | analysis = Analysis.move model.game.stones coords model.analysis } |> setLastPointer, Cmd.none )

            else
                ( handleMoved coords model |> setLastPointer, Cmd.none )

        PointerInput.FinishAt _ ->
            if model.showAnalysis then
                handleAnalysisFinished coords { model | dragFrom = DF.DragNone }

            else if model.dragFrom == DF.DragSpot coords then
                let
                    cleared =
                        { model
                            | dragFrom = DF.DragNone
                            , stagedMove = Nothing
                            , ghostStone = Nothing
                        }
                in
                case stagedMoveAt coords model of
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

        PointerInput.CancelGesture ->
            ( cancelBoardGesture model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PointerEvent event ->
            let
                ( pointer, action ) =
                    PointerInput.update event model.pointer

                updated =
                    { model | pointer = pointer }
            in
            case action of
                Just next ->
                    handlePointerAction next updated

                Nothing ->
                    ( updated, Cmd.none )

        PointerAtBoard revision action (Ok coords) ->
            if revision == model.pointer.revision then
                handleBoardPointer action coords model

            else
                ( model, Cmd.none )

        ClearAnalysisHover ->
            ( { model | analysis = Analysis.clearHover model.analysis }, Cmd.none )

        PassTurn ->
            if model.showAnalysis then
                ( model, Cmd.none )

            else
                case Game.pass model.game of
                    Just game ->
                        updatePosition { model | game = game }

                    Nothing ->
                        ( model, Cmd.none )

        ClearCaptureAnimation ->
            ( { model | justRemoved = [] }
            , Cmd.none
            )

        Undo ->
            ( model
            , Nav.back model.navKey 1
            )

        Redo ->
            ( model
            , Nav.forward model.navKey 1
            )

        NewGame ->
            let
                fresh =
                    emptyModel model.navKey

                analysis =
                    fresh.analysis
            in
            updatePosition
                { fresh
                    | analysis = { analysis | serial = model.analysis.serial }
                    , pointer = PointerInput.cancel model.pointer
                }

        ToggleHelp ->
            ( { model
                | showHelp = not model.showHelp
                , pointer = PointerInput.cancel model.pointer
              }
                |> cancelBoardGesture
            , Cmd.none
            )

        ToggleScoring ->
            ( { model
                | showScoring = not model.showScoring
                , pointer = PointerInput.cancel model.pointer
                , showAnalysis = False
                , analysis = Analysis.cancel model.analysis
                , ghostStone = Nothing
                , stagedMove = Nothing
                , highlightedGroup = []
                , dragFrom = DF.DragNone
                , lastPointer = Nothing
              }
            , Cmd.none
            )

        ToggleAnalysis ->
            ( { model
                | showAnalysis = not model.showAnalysis
                , pointer = PointerInput.cancel model.pointer
                , showScoring = False
                , analysis = Analysis.reconcile model.game.stones model.analysis
                , ghostStone = Nothing
                , stagedMove = Nothing
                , highlightedGroup = []
                , justPlayed = Nothing
                , dragFrom = DF.DragNone
                , lastPointer = Nothing
              }
            , Cmd.none
            )

        AnalysisRemovalExpired serial ->
            ( { model | analysis = Analysis.expire serial model.analysis }, Cmd.none )

        UrlChanged url ->
            restoreFromUrl url model

        _ ->
            ( model, Cmd.none )



-- ROUTING / URL


restoreFromUrl : Url.Url -> Model -> ( Model, Cmd Msg )
restoreFromUrl url model =
    let
        maybeRecord : Maybe Sgf.Record
        maybeRecord =
            RecordUrl.decode url

        createModel : Sgf.Record -> Model
        createModel record =
            let
                game =
                    Game.fromRecord (List.reverse record.plays)
            in
            { model
                | game = game
                , sgfRoot = record.root
                , pointer = PointerInput.cancel model.pointer
                , analysis = Analysis.reconcile game.stones model.analysis
                , highlightedGroup = []
                , ghostStone = Nothing
                , stagedMove = Nothing
                , dragFrom = DF.DragNone
                , lastPointer = Nothing
            }

        empty =
            emptyModel model.navKey

        newModel =
            Maybe.map createModel maybeRecord
                |> Maybe.withDefault
                    { empty
                        | showScoring = model.showScoring
                        , pointer = PointerInput.cancel model.pointer
                        , showAnalysis = model.showAnalysis
                        , analysis = Analysis.reconcile Dict.empty model.analysis
                    }
    in
    ( newModel, Cmd.none )


updatePosition : Model -> ( Model, Cmd Msg )
updatePosition model =
    ( { model | ghostStone = Nothing, stagedMove = Nothing, highlightedGroup = [] }
    , pushRecordUrl model
    )


pushRecordUrl : Model -> Cmd msg
pushRecordUrl model =
    Nav.pushUrl model.navKey <|
        RecordUrl.encode { root = model.sgfRoot, plays = List.reverse model.game.record }



-- VIEW


isScoring : Model -> Bool
isScoring model =
    not model.showAnalysis && (model.showScoring || Game.isFinished model.game)


{-| Keep the unchanged blur in a separate SVG. Promoting individual SVG groups
can leave stale mask tiles after pinch zoom; the background needs no masks.
-}
viewBackground : Stones -> Svg Msg
viewBackground stones =
    Svg.svg [ SA.id "board-background", SA.viewBox boardViewBox ]
        [ Svg.defs []
            [ Svg.filter [ SA.id "blur-filter" ]
                [ Svg.feGaussianBlur [ SA.in_ "SourceGraphic", SA.stdDeviation "70" ] [] ]
            ]
        , lazy3 Svg.node "g" [ SA.id "lines" ] <| BoardView.viewLines
        , lazy3 Svg.node "g" [ SA.id "stars" ] <| BoardView.viewStars
        , lazy3 Svg.Keyed.node "g" [ SA.id "hide-lines", SA.filter "url(#blur-filter)" ] <|
            List.map viewKeyedLineCover (stoneList stones)
        ]


boardViewBox : String
boardViewBox =
    "0 0 " ++ String.fromInt coordRange ++ " " ++ String.fromInt coordRange


viewSvg : Model -> Svg Msg
viewSvg model =
    Svg.svg [ SA.id "board-position", SA.viewBox boardViewBox ]
        [ lazy8 viewPosition
            model.game.stones
            (Maybe.Extra.or model.stagedMove model.ghostStone)
            model.highlightedGroup
            model.justPlayed
            model.justRemoved
            (isScoring model)
            (Maybe.Extra.isJust model.stagedMove)
            (if model.showAnalysis then
                model.analysis

             else
                Analysis.empty
            )
        , if model.showAnalysis then
            lazy2 Analysis.view model.game.stones model.analysis

          else
            Svg.g [] []
        ]


{-| Pointer coordinates and drag bookkeeping do not change the position. Keep
this whole subtree lazy so those updates cannot rebuild previews or SVG masks.
-}
viewPosition : Stones -> Maybe MovePreview.Preview -> List Spot -> Maybe Spot -> List Stone -> Bool -> Bool -> Analysis.Model -> Svg Msg
viewPosition stones candidate group justPlayed justRemoved scoring staged analysis =
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
    Svg.g []
        [ if scoring then
            lazy TerritoryView.view stones

          else
            lazy3 Analysis.viewDeadZones previewStones analysis.circles analysis.preview
        , if scoring then
            Svg.g [] []

          else
            lazy2 Coverage.viewLiberties previewStones highlightedGroup
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
            stoneRadius // 2
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
            case List.length model.game.record of
                0 ->
                    "ContinuGo: freed from the tyranny of the grid"

                n ->
                    "ContinuGo: #" ++ (String.fromInt <| n)

        score : H.Html msg
        score =
            if isScoring model then
                let
                    territory =
                        Territory.areas model.game.stones
                in
                H.div [ HA.id "score" ]
                    [ H.div [ HA.class "score-result" ] [ H.text (TerritoryView.resultString territory) ]
                    , H.div [ HA.class "score-areas" ]
                        [ H.text ("Black " ++ TerritoryView.formatArea territory.black ++ " · White " ++ TerritoryView.formatArea territory.white) ]
                    ]

            else
                H.div [] []

        content =
            if model.showHelp then
                H.div [ HA.id "help" ] [ Markdown.toHtml [] HelpContent.markdown ]

            else
                H.div
                    (HA.id "board-container" :: PointerInput.events PointerEvent)
                    [ H.div
                        [ HA.id "board"
                        , HA.classList [ ( "scoring", isScoring model ), ( "analysis", model.showAnalysis ) ]
                        , HE.on "pointerleave" <| Decode.succeed ClearAnalysisHover
                        ]
                        [ lazy viewBackground model.game.stones
                        , viewSvg model
                        , score
                        ]
                    ]
    in
    { title = title
    , body =
        [ H.div [ HA.id "menu" ]
            [ menuLink NewGame "!" "new"
            , menuLink ToggleHelp "?" "help"
            , menuLink PassTurn "*" "pass"
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
                    , HA.disabled (Game.isFinished model.game)
                    , HE.onClick ToggleScoring
                    ]
                    [ H.text "%", H.span [ HA.class "tooltip" ] [ H.text " score" ] ]
                ]
            , H.div [ HA.class "item" ]
                [ H.button
                    [ HA.id "analysis-toggle"
                    , HA.classList [ ( "icon", True ), ( "active", model.showAnalysis ) ]
                    , HA.attribute "aria-label" "Toggle analysis mode"
                    , HA.attribute "aria-pressed"
                        (if model.showAnalysis then
                            "true"

                         else
                            "false"
                        )
                    , HE.onClick ToggleAnalysis
                    ]
                    [ H.text "o", H.span [ HA.class "tooltip" ] [ H.text " analysis" ] ]
                ]
            , menuLink Undo "‹" "undo"
            , menuLink Redo "︎︎›︎" "redo"
            ]
        , content
        ]
    }
