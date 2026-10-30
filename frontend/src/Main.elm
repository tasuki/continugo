port module Main exposing (..)

import Analysis
import BoardView
import Browser
import Browser.Dom as BD
import Browser.Events
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
import Rules.Board as Board exposing (Board, coordRange)
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


port saveSession : { sgf : String, move : Int } -> Cmd msg


port sgfLoaded : (String -> msg) -> Sub msg


port sgfLoadFailed : (String -> msg) -> Sub msg


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , update = updateAndSave
        , subscriptions =
            \model ->
                Sub.batch
                    [ sgfLoaded SgfLoaded
                    , sgfLoadFailed SgfLoadFailed
                    , if model.showMenu || model.showHelp then
                        Sub.none

                      else
                        Browser.Events.onKeyDown replayKey
                    ]
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


{-| record is the complete chronological game; game is its displayed prefix.
-}
type alias Model =
    { game : Game.Game
    , record : List Play
    , sgfRoot : List Sgf.Property
    , ghostStone : Maybe MovePreview.Preview
    , highlightedGroup : List Spot
    , justPlayed : Maybe Spot
    , justRemoved : List Stone
    , dragFrom : DragFrom
    , pointer : PointerInput.State
    , stagedMove : Maybe MovePreview.Preview
    , lastPointer : Maybe Spot
    , sgfError : Maybe String
    , showMenu : Bool
    , showHelp : Bool
    , showScoring : Bool
    , showAnalysis : Bool
    , analysis : Analysis.Model
    , navKey : Nav.Key
    }


emptyModel : Nav.Key -> Model
emptyModel navKey =
    { game = Game.empty
    , record = []
    , sgfRoot = Sgf.empty.root
    , ghostStone = Nothing
    , highlightedGroup = []
    , justPlayed = Nothing
    , justRemoved = []
    , dragFrom = DF.DragNone
    , pointer = PointerInput.empty
    , stagedMove = Nothing
    , lastPointer = Nothing
    , sgfError = Nothing
    , showMenu = False
    , showHelp = False
    , showScoring = False
    , showAnalysis = False
    , analysis = Analysis.empty
    , navKey = navKey
    }


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        fallback =
            restoreFromUrl url (emptyModel navKey) |> Tuple.first

        model =
            readSession flags
                |> Maybe.map
                    (\( record, game ) ->
                        { fallback | game = game, record = record.plays, sgfRoot = record.root }
                    )
                |> Maybe.withDefault fallback
    in
    ( model, persist model )


readSession : Decode.Value -> Maybe ( Sgf.Record, Game.Game )
readSession value =
    Decode.decodeValue
        (Decode.map2 Tuple.pair (Decode.field "sgf" Decode.string) (Decode.field "move" Decode.int))
        value
        |> Result.toMaybe
        |> Maybe.andThen
            (\( text, move ) ->
                Sgf.decodeRecord text
                    |> Result.toMaybe
                    |> Maybe.andThen
                        (\record ->
                            Sgf.boardFromRoot record.root
                                |> Maybe.map (\board -> ( record, gameAt board move record.plays ))
                        )
            )


persist : Model -> Cmd msg
persist model =
    saveSession
        { sgf = Sgf.encodeRecord { root = model.sgfRoot, plays = model.record }
        , move = List.length model.game.record
        }



-- UPDATE


type Msg
    = PointerEvent PointerInput.Event
    | PointerAtBoard Int PointerInput.Action (Result BD.Error Spot)
    | ClearAnalysisHover
    | PassTurn
    | ClearCaptureAnimation
    | Prev
    | Next
    | ToggleHelp
    | ToggleScoring
    | ToggleAnalysis
    | AnalysisRemovalExpired Int
    | OpenMenu
    | NewBoard Board
    | SgfLoaded String
    | SgfLoadFailed String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


replayKey : Decode.Decoder Msg
replayKey =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowLeft" ->
                        Decode.succeed Prev

                    "ArrowRight" ->
                        Decode.succeed Next

                    _ ->
                        Decode.fail "Not a replay key"
            )


handleHover : Model -> Spot -> Model
handleHover model hoverSpot =
    let
        maybeOverStone : Maybe Stone
        maybeOverStone =
            stoneList model.game.stones
                |> List.Extra.find (\s -> isWithinStoneOn model.game.board hoverSpot s.spot)

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
            DF.findDragged model.game.board model.game.stones model.game.onMove draggedFrom draggedTo

        ghostStone : Maybe MovePreview.Preview
        ghostStone =
            if Maybe.map (.stone >> .spot) model.ghostStone == Just candidateStone.spot then
                model.ghostStone

            else
                MovePreview.fromCandidates model.game.board model.game.stones (Just candidateStone) Nothing
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
            Analysis.finish model.game.board model.game.stones coords model.analysis

        updated =
            { model | analysis = analysis }
    in
    case effect of
        Analysis.NoEffect ->
            ( updated, Cmd.none )

        Analysis.ScheduleRemoval serial ->
            ( updated, Process.sleep 300 |> Task.perform (\_ -> AnalysisRemovalExpired serial) )

        Analysis.Propose spot ->
            case MovePreview.fromCandidates model.game.board model.game.stones (Just (createStone model.game.onMove spot)) Nothing of
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
        Placement.nearbySpot model.game.board model.game.stones playSpot
            |> Maybe.andThen (\spot -> Game.play spot model.game |> Maybe.map (Tuple.pair spot))
    of
        Just ( spot, game ) ->
            let
                newModel =
                    { model
                        | game = game
                        , record = List.reverse game.record
                        , justPlayed = Just spot
                        , justRemoved = removedStones model.game.stones game.stones
                        , highlightedGroup = []
                        , ghostStone = Nothing
                        , stagedMove = Nothing
                    }
            in
            ( newModel
            , Process.sleep 500 |> Task.perform (\_ -> ClearCaptureAnimation)
            )

        _ ->
            -- illegal move
            ( model, Cmd.none )


previewNearbyMove : Spot -> Model -> Maybe MovePreview.Preview
previewNearbyMove coords model =
    case Placement.playNearby model.game.board model.game.onMove model.game.stones coords of
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
        |> Maybe.Extra.filter (\preview -> isWithinStoneOn model.game.board coords preview.stone.spot)


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
                    | analysis = Analysis.start model.game.board model.game.stones coords model.analysis
                    , dragFrom = DF.DragSpot coords
                  }
                , Cmd.none
                )

            else
                ( { model
                    | dragFrom = DF.getDragFrom model.game.board model.game.stones model.game.onMove coords
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
                ( { model | analysis = Analysis.move model.game.board model.game.stones coords model.analysis } |> setLastPointer, Cmd.none )

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


updateAndSave : Msg -> Model -> ( Model, Cmd Msg )
updateAndSave msg model =
    let
        ( updated, command ) =
            update msg model
    in
    ( updated
    , if updated.record /= model.record || updated.sgfRoot /= model.sgfRoot || updated.game.record /= model.game.record then
        Cmd.batch [ command, persist updated ]

      else
        command
    )


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
                        updatePosition { model | game = game, record = List.reverse game.record }

                    Nothing ->
                        ( model, Cmd.none )

        ClearCaptureAnimation ->
            ( { model | justRemoved = [] }
            , Cmd.none
            )

        Prev ->
            showGame (Game.previous model.game) model

        Next ->
            case List.drop (List.length model.game.record) model.record of
                recorded :: _ ->
                    showGame (Game.replay recorded model.game) model

                [] ->
                    ( model, Cmd.none )

        OpenMenu ->
            ( { model | showMenu = True, sgfError = Nothing, pointer = PointerInput.cancel model.pointer }
                |> cancelBoardGesture
            , Cmd.none
            )

        NewBoard board ->
            ( newBoard board model, Cmd.none )

        SgfLoaded text ->
            case decodeImport text of
                Ok ( board, record ) ->
                    let
                        fresh =
                            newBoard board model
                    in
                    ( { fresh | record = record.plays, sgfRoot = record.root }, Cmd.none )

                Err error ->
                    ( { model | sgfError = Just error }, Cmd.none )

        SgfLoadFailed error ->
            ( { model | sgfError = Just error }, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.navKey (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

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
                , analysis = Analysis.reconcile model.game.board model.game.stones model.analysis
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


newBoard : Board -> Model -> Model
newBoard board model =
    let
        fresh =
            emptyModel model.navKey

        analysis =
            fresh.analysis
    in
    { fresh
        | game = Game.onBoard board
        , sgfRoot = (Sgf.onBoard board).root
        , pointer = PointerInput.cancel model.pointer
        , analysis = { analysis | serial = model.analysis.serial }
    }


decodeImport : String -> Result String ( Board, Sgf.Record )
decodeImport text =
    Sgf.decodeRecord text
        |> Result.andThen
            (\record ->
                Sgf.boardFromRoot record.root
                    |> Result.fromMaybe "Invalid SGF board size"
                    |> Result.map (\board -> ( board, record ))
            )


restoreFromUrl : Url.Url -> Model -> ( Model, Cmd Msg )
restoreFromUrl url model =
    let
        maybeRecord : Maybe ( Board, Sgf.Record )
        maybeRecord =
            RecordUrl.decode url
                |> Maybe.andThen
                    (\record ->
                        Sgf.boardFromRoot record.root
                            |> Maybe.map (\board -> ( board, record ))
                    )

        createModel : ( Board, Sgf.Record ) -> Model
        createModel ( board, record ) =
            let
                game =
                    Game.fromRecordOn board (List.reverse record.plays)
            in
            { model
                | game = game
                , record = record.plays
                , sgfRoot = record.root
                , pointer = PointerInput.cancel model.pointer
                , analysis = Analysis.reconcile game.board game.stones model.analysis
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
                        , analysis = Analysis.reconcile Board.default Dict.empty model.analysis
                    }
    in
    ( { newModel | showMenu = url.path == "/menu" }, Cmd.none )


updatePosition : Model -> ( Model, Cmd Msg )
updatePosition model =
    ( { model | ghostStone = Nothing, stagedMove = Nothing, highlightedGroup = [] }
    , Cmd.none
    )


gameAt : Board -> Int -> List Play -> Game.Game
gameAt board move record =
    Game.fromRecordOn board (List.reverse (List.take move record))


showGame : Game.Game -> Model -> ( Model, Cmd Msg )
showGame game model =
    updatePosition
        { model
            | game = game
            , justPlayed = Nothing
            , justRemoved = []
            , pointer = PointerInput.cancel model.pointer
            , analysis = Analysis.reconcile game.board game.stones model.analysis
        }
        |> Tuple.mapFirst cancelBoardGesture



-- VIEW


isScoring : Model -> Bool
isScoring model =
    not model.showAnalysis && (model.showScoring || Game.isFinished model.game)


{-| Keep the unchanged blur in a separate SVG. Promoting individual SVG groups
can leave stale mask tiles after pinch zoom; the background needs no masks.
-}
viewBackground : Board -> Stones -> Svg Msg
viewBackground board stones =
    Svg.svg [ SA.id "board-background", SA.viewBox boardViewBox ]
        [ Svg.defs []
            [ Svg.filter [ SA.id "blur-filter" ]
                [ Svg.feGaussianBlur [ SA.in_ "SourceGraphic", SA.stdDeviation "70" ] [] ]
            ]
        , lazy3 Svg.node "g" [ SA.id "lines" ] <| BoardView.viewLines board
        , lazy3 Svg.node "g" [ SA.id "stars" ] <| BoardView.viewStars board
        , lazy3 Svg.Keyed.node "g" [ SA.id "hide-lines", SA.filter "url(#blur-filter)" ] <|
            List.map (viewKeyedLineCover board) (stoneList stones)
        ]


boardViewBox : String
boardViewBox =
    "0 0 " ++ String.fromInt coordRange ++ " " ++ String.fromInt coordRange


type PositionMode
    = Normal
    | Staged
    | Scoring


viewSvg : Model -> Svg Msg
viewSvg model =
    Svg.svg [ SA.id "board-position", SA.viewBox boardViewBox ]
        [ lazy8 viewPosition
            model.game.board
            model.game.stones
            (Maybe.Extra.or model.stagedMove model.ghostStone)
            model.highlightedGroup
            model.justPlayed
            model.justRemoved
            (if isScoring model then
                Scoring

             else if Maybe.Extra.isJust model.stagedMove then
                Staged

             else
                Normal
            )
            (if model.showAnalysis then
                model.analysis

             else
                Analysis.empty
            )
        , if model.showAnalysis then
            lazy3 Analysis.view model.game.board model.game.stones model.analysis

          else
            Svg.g [] []
        ]


{-| Pointer coordinates and drag bookkeeping do not change the position. Keep
this whole subtree lazy so those updates cannot rebuild previews or SVG masks.
-}
viewPosition : Board -> Stones -> Maybe MovePreview.Preview -> List Spot -> Maybe Spot -> List Stone -> PositionMode -> Analysis.Model -> Svg Msg
viewPosition board stones candidate group justPlayed justRemoved mode analysis =
    let
        scoring =
            mode == Scoring

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
            lazy4 Analysis.viewDeadZones board previewStones analysis.circles analysis.preview
        , if scoring then
            Svg.g [] []

          else
            lazy3 Coverage.viewLiberties board previewStones highlightedGroup
        , lazy3 Svg.node "g" [ SA.id "ghost-links" ] <|
            List.map (viewGhostLink board) (Maybe.map getStoneLinks previewStone |> Maybe.withDefault [])
        , lazy3 Svg.node "g" [ SA.id "ghost-stone" ] <|
            List.filterMap identity [ Maybe.map (viewGhostStone board) previewStone ]
        , lazy3 Svg.Keyed.node "g" [ SA.id "links" ] <|
            List.map viewKeyedLink (List.filter survivingLink (getUniqueLinks stones))
        , lazy3 Svg.Keyed.node "g" [ SA.id "stones" ] <|
            (stoneList stones |> List.map (\s -> viewKeyedStone board (stoneClass s) s))
        , lazy3 Svg.node "g" [ SA.id "staged-stone" ] <|
            if mode == Staged then
                List.filterMap identity [ Maybe.map (viewStagedStone board) previewStone ]

            else
                []
        , lazy3 Svg.node "g" [ SA.id "capture-preview", SA.pointerEvents "none" ] <|
            List.map (viewCapturePreview board) (Maybe.map .captured preview |> Maybe.withDefault [])
        , lazy3 Svg.node "g" [ SA.id "removed-stones" ] <|
            (justRemoved |> List.map (viewStone board "removed"))
        , lazy3 Svg.node "g" [ SA.id "highlights" ] <|
            (highlightedGroup
                |> List.filter (\spot -> Just spot /= Maybe.map .spot previewStone)
                |> List.map (viewHighlight board)
            )
        ]


viewCapturePreview : Board -> Stone -> Svg msg
viewCapturePreview board { spot } =
    let
        offset =
            Board.radius board // 2
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


viewNewGames : Maybe String -> H.Html Msg
viewNewGames error =
    H.main_ []
        [ H.h1 [] [ H.text "ContinuGo" ]
        , H.p [] [ H.text "New game" ]
        , H.div []
            ([ 9, 13, 19 ]
                |> List.filterMap Board.fromSize
                |> List.map
                    (\board ->
                        H.button [ HE.onClick (NewBoard board) ]
                            [ H.text (String.fromInt (Board.size board) ++ "×" ++ String.fromInt (Board.size board)) ]
                    )
            )
        , H.div [ HA.id "sgf-dropzone" ] [ H.text "Drop an SGF file here" ]
        , case error of
            Just message ->
                H.p [ HA.attribute "role" "alert" ] [ H.text message ]

            Nothing ->
                H.text ""
        ]


viewMoveNumber : List Play -> H.Html msg
viewMoveNumber record =
    let
        number =
            String.fromInt (List.length record)

        coordinates =
            case List.head record |> Maybe.map .move of
                Just (Place stone) ->
                    Sgf.encodePlay { color = stone.color, move = Place stone }
                        |> String.slice 2 6

                Just Pass ->
                    "pass"

                Nothing ->
                    "No moves yet"
    in
    H.div [ HA.class "item" ]
        [ H.div
            [ HA.id "move-number"
            , HA.class "icon"
            , HA.attribute "aria-label" ("Move " ++ number ++ ": " ++ coordinates)
            ]
            [ H.text number, H.span [ HA.class "tooltip" ] [ H.text (" " ++ coordinates) ] ]
        ]


viewDownload : List Sgf.Property -> List Play -> H.Html msg
viewDownload root record =
    H.div [ HA.class "item" ]
        [ H.a
            [ HA.class "icon"
            , HA.href ("data:application/x-go-sgf;charset=utf-8," ++ Url.percentEncode (Sgf.encodeRecord { root = root, plays = record }))
            , HA.download "continugo.sgf"
            , HA.attribute "aria-label" "Download SGF"
            ]
            [ H.text "↓", H.span [ HA.class "tooltip" ] [ H.text " download SGF" ] ]
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
                    [ H.div [ HA.class "score-result" ] [ H.text (TerritoryView.resultString model.game.board territory) ]
                    , H.div [ HA.class "score-areas" ]
                        [ H.text ("Black " ++ TerritoryView.formatArea model.game.board territory.black ++ " · White " ++ TerritoryView.formatArea model.game.board territory.white) ]
                    ]

            else
                H.div [] []

        content =
            if model.showMenu then
                viewNewGames model.sgfError

            else if model.showHelp then
                H.div [ HA.id "help" ] [ Markdown.toHtml [] HelpContent.markdown ]

            else
                H.div
                    (HA.id "board-container" :: PointerInput.events PointerEvent)
                    [ H.div
                        [ HA.id "board"
                        , HA.classList [ ( "scoring", isScoring model ), ( "analysis", model.showAnalysis ) ]
                        , HE.on "pointerleave" <| Decode.succeed ClearAnalysisHover
                        ]
                        [ lazy2 viewBackground model.game.board model.game.stones
                        , viewSvg model
                        , score
                        ]
                    ]
    in
    { title = title
    , body =
        [ H.div [ HA.id "menu", HA.hidden model.showMenu ]
            [ menuLink OpenMenu "!" "menu"
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
            , lazy viewMoveNumber model.game.record
            , menuLink Prev "‹" "prev"
            , menuLink Next "›" "next"
            , lazy2 viewDownload model.sgfRoot model.record
            ]
        , content
        ]
    }
