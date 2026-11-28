port module Main exposing (..)

import Analysis
import BoardHover
import BoardScene exposing (PositionMode(..), boardViewBox, viewBackground, viewLastMove, viewPosition)
import Browser
import Browser.Dom as BD
import Browser.Events
import Browser.Navigation as Nav
import Dict
import DocumentCoords as DC exposing (DocumentCoords)
import DragFrom as DF exposing (DragFrom)
import HelpContent
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Markdown
import Maybe.Extra
import MovePreview
import Pill
import Placement
import PointerInput
import Process
import RecordUrl
import Review
import Rules.Board as Board exposing (Board)
import Rules.Game as Game
import Rules.Position exposing (..)
import Rules.Territory as Territory
import Sgf
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy exposing (..)
import Task
import TerritoryView
import Training
import Url exposing (Url)


port saveSession : { sgf : String, move : Int, path : List Int } -> Cmd msg


port botRequest : { id : Int, sgf : String, ply : Int, simulations : Int } -> Cmd msg


port botResponse : (Decode.Value -> msg) -> Sub msg


port botAvailability : ({ available : Bool, simulations : Int } -> msg) -> Sub msg


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
                    [ botAvailability BotAvailability
                    , botResponse BotResponse
                    , sgfLoaded SgfLoaded
                    , sgfLoadFailed SgfLoadFailed
                    , Browser.Events.onKeyDown (menuKey model)
                    ]
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


{-| record is the selected continuation; game is its displayed prefix.
-}
type alias PlaySetup =
    { board : Board, bot : Maybe Color, customSize : Maybe String, komi : String, simulations : Int }


defaultPlaySetup : PlaySetup
defaultPlaySetup =
    { board = Board.fromSize 9 |> Maybe.withDefault Board.default, bot = Nothing, customSize = Nothing, komi = "7", simulations = 128 }


setupBoard : PlaySetup -> Maybe Board
setupBoard setup =
    case setup.customSize of
        Just value ->
            String.toInt value
                |> Maybe.Extra.filter (\size -> size >= 7 && size <= 25)
                |> Maybe.andThen Board.fromSize

        Nothing ->
            Just setup.board


setupKomi : PlaySetup -> Maybe Float
setupKomi setup =
    String.toFloat setup.komi
        |> Maybe.Extra.filter (\value -> not (isNaN value || isInfinite value))


type alias LivePosition =
    { game : Game.Game, path : List Int }


type alias Model =
    { game : Game.Game
    , review : Review.Model
    , record : List Play
    , sgfRoot : List Sgf.Property
    , sgfMoves : List (List Sgf.Property)
    , showPolicy : Bool
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
    , botAvailable : Bool
    , setup : PlaySetup
    , bot : Maybe Color
    , botReview : Maybe LivePosition
    , botSimulations : Int
    , botSerial : Int
    , botPending : Maybe Int
    , botError : Maybe String
    , navKey : Nav.Key
    }


emptyModel : Nav.Key -> Model
emptyModel navKey =
    { game = Game.empty
    , review = Review.empty
    , record = []
    , sgfRoot = Sgf.empty.root
    , sgfMoves = []
    , showPolicy = True
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
    , botAvailable = False
    , setup = defaultPlaySetup
    , bot = Nothing
    , botReview = Nothing
    , botSimulations = 128
    , botSerial = 0
    , botPending = Nothing
    , botError = Nothing
    , navKey = navKey
    }


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        fallback =
            restoreFromUrl url (emptyModel navKey) |> Tuple.first

        model =
            readReviewSession flags
                |> Maybe.map
                    (\( review, record, game ) ->
                        withReview review { fallback | sgfRoot = record.root, game = game }
                    )
                |> Maybe.withDefault fallback
    in
    ( { model | showMenu = model.showMenu || (List.isEmpty model.game.record && RecordUrl.decode url == Nothing) }, persist model )


readSession : Decode.Value -> Maybe ( Sgf.Record, Game.Game )
readSession value =
    readReviewSession value |> Maybe.map (\( _, record, game ) -> ( record, game ))


readReviewSession : Decode.Value -> Maybe ( Review.Model, Sgf.Record, Game.Game )
readReviewSession value =
    Decode.decodeValue
        (Decode.map2 Tuple.pair (Decode.field "sgf" Decode.string) (Decode.field "move" Decode.int))
        value
        |> Result.toMaybe
        |> Maybe.andThen
            (\( text, move ) ->
                Sgf.decodeTree text
                    |> Result.toMaybe
                    |> Maybe.andThen
                        (\( root, trees ) ->
                            let
                                path =
                                    Decode.decodeValue (Decode.field "path" (Decode.list Decode.int)) value |> Result.withDefault []

                                review =
                                    Review.select path (Review.fromTrees trees)

                                record =
                                    Review.line root review
                            in
                            Sgf.boardFromRoot root
                                |> Maybe.map (\board -> ( review, record, gameAt board move record.plays ))
                        )
            )


persist : Model -> Cmd msg
persist model =
    saveSession
        { sgf = Sgf.encodeTree model.sgfRoot model.review.trees
        , path = model.review.selected
        , move = List.length model.game.record
        }


sgfRecord : Model -> Sgf.Record
sgfRecord model =
    { root = model.sgfRoot, plays = model.record, moveProperties = model.sgfMoves }



-- UPDATE


type Msg
    = PointerEvent PointerInput.Event
    | PointerAtBoard Int PointerInput.Action (Result BD.Error Spot)
    | ClearAnalysisHover
    | PassTurn
    | SetBot (Maybe Color)
    | SetBoard Board
    | SetCustomSize String
    | SetKomi String
    | SetSimulations Int
    | StartGame
    | BotAvailability { available : Bool, simulations : Int }
    | BotResponse Decode.Value
    | ClearCaptureAnimation
    | Prev
    | BackToGame
    | Next
    | SwitchVariation Int
    | SelectSibling Int
    | CancelBoardInput
    | ToggleHelp
    | ToggleScoring
    | ToggleAnalysis
    | TogglePolicy
    | AnalysisRemovalExpired Int
    | OpenMenu
    | ClosePanels
    | SgfLoaded String
    | SgfLoadFailed String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


menuKey : { a | showMenu : Bool, showHelp : Bool } -> Decode.Decoder Msg
menuKey model =
    let
        flag path =
            Decode.oneOf [ Decode.at path Decode.bool, Decode.succeed False ]

        blocked key modified repeating editable tag =
            ( key
            , modified
                || editable
                || List.member tag [ "INPUT", "TEXTAREA", "SELECT" ]
                || (repeating && not (List.member key [ "ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown" ]))
                || ((model.showMenu || model.showHelp)
                        && not (key == "Escape" || key == "!" || (key == "?" && model.showHelp))
                   )
            )
    in
    Decode.map5 blocked
        (Decode.field "key" Decode.string)
        (Decode.map3 (\ctrl meta alt -> ctrl || meta || alt)
            (flag [ "ctrlKey" ])
            (flag [ "metaKey" ])
            (flag [ "altKey" ])
        )
        (flag [ "repeat" ])
        (flag [ "target", "isContentEditable" ])
        (Decode.oneOf [ Decode.at [ "target", "tagName" ] Decode.string, Decode.succeed "" ])
        |> Decode.andThen
            (\( key, ignored ) ->
                if ignored then
                    Decode.fail "Not a menu shortcut"

                else
                    case key of
                        "!" ->
                            Decode.succeed OpenMenu

                        "?" ->
                            Decode.succeed ToggleHelp

                        "*" ->
                            Decode.succeed PassTurn

                        "%" ->
                            Decode.succeed ToggleScoring

                        "o" ->
                            Decode.succeed ToggleAnalysis

                        "p" ->
                            Decode.succeed TogglePolicy

                        "ArrowLeft" ->
                            Decode.succeed Prev

                        "←" ->
                            Decode.succeed Prev

                        "ArrowRight" ->
                            Decode.succeed Next

                        "→" ->
                            Decode.succeed Next

                        "ArrowUp" ->
                            Decode.succeed (SwitchVariation -1)

                        "ArrowDown" ->
                            Decode.succeed (SwitchVariation 1)

                        "Escape" ->
                            Decode.succeed ClosePanels

                        _ ->
                            Decode.fail "Not a menu shortcut"
            )


handleDrag : Model -> Maybe Stone -> Model
handleDrag model candidateStone =
    let
        ghostStone : Maybe MovePreview.Preview
        ghostStone =
            if Maybe.map (.stone >> .spot) model.ghostStone == Maybe.map .spot candidateStone then
                model.ghostStone

            else
                MovePreview.forGame model.game candidateStone Nothing
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
            case MovePreview.forGame model.game (Just (createStone model.game.onMove spot)) Nothing of
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
                        , justPlayed = Just spot
                        , justRemoved = removedStones model.game.stones game.stones
                        , highlightedGroup = []
                        , ghostStone = Nothing
                        , stagedMove = Nothing
                    }
            in
            ( commitMove newModel
            , Process.sleep 500 |> Task.perform (\_ -> ClearCaptureAnimation)
            )

        _ ->
            -- illegal move
            ( model, Cmd.none )


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
            handleDrag model (Just (DF.findDragged model.game.board model.game.stones model.game.onMove draggedFrom coords))

        ( _, DF.DragBorders _ borders ) ->
            DF.findBorderDraggedSpot model.game.board (List.map .spot (stoneList model.game.stones)) borders coords
                |> Maybe.map (createStone model.game.onMove)
                |> handleDrag model

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
                    BoardHover.at { model | stagedMove = Nothing } coords


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

            else if DF.isTap coords model.dragFrom then
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
        ( changed, command ) =
            update msg model

        ( updated, botCommand ) =
            requestBot model changed
    in
    ( updated
    , if updated.review /= model.review || updated.sgfRoot /= model.sgfRoot || updated.game.record /= model.game.record then
        Cmd.batch [ command, botCommand, persist updated ]

      else
        Cmd.batch [ command, botCommand ]
    )


botTurn : Model -> Bool
botTurn model =
    model.botReview == Nothing && model.bot == Just model.game.onMove && not (Game.isFinished model.game)


botSgf : Model -> String
botSgf model =
    Sgf.encodeRecord { root = model.sgfRoot, plays = List.reverse model.game.record, moveProperties = [] }


requestBot : Model -> Model -> ( Model, Cmd Msg )
requestBot before model =
    let
        changed =
            before.game.record /= model.game.record || before.sgfRoot /= model.sgfRoot || before.bot /= model.bot || before.botReview /= model.botReview

        current =
            if changed then
                { model | botPending = Nothing, botError = Nothing }

            else
                model
    in
    if botTurn current && current.botPending == Nothing && current.botError == Nothing then
        let
            id =
                current.botSerial + 1
        in
        ( { current | botSerial = id, botPending = Just id }
        , botRequest { id = id, sgf = botSgf current, ply = List.length current.game.record, simulations = current.botSimulations }
        )

    else
        ( current, Cmd.none )


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
            if revision == model.pointer.revision && model.botReview == Nothing && not (botTurn model) then
                handleBoardPointer action coords model

            else
                ( model, Cmd.none )

        ClearAnalysisHover ->
            ( { model | analysis = Analysis.clearHover model.analysis }, Cmd.none )

        BotAvailability config ->
            let
                setup =
                    model.setup
            in
            ( { model | botAvailable = config.available, setup = { setup | simulations = clamp 1 1024 config.simulations } }, Cmd.none )

        SetBot color ->
            let
                setup =
                    model.setup
            in
            ( { model | setup = { setup | bot = color } }, Cmd.none )

        SetBoard board ->
            let
                setup =
                    model.setup
            in
            ( { model | setup = { setup | board = board, customSize = Nothing } }, Cmd.none )

        SetCustomSize value ->
            let
                setup =
                    model.setup
            in
            ( { model | setup = { setup | customSize = Just value } }, Cmd.none )

        SetSimulations value ->
            let
                setup =
                    model.setup
            in
            ( { model | setup = { setup | simulations = clamp 1 1024 value } }, Cmd.none )

        SetKomi value ->
            let
                setup =
                    model.setup
            in
            ( { model | setup = { setup | komi = value } }, Cmd.none )

        StartGame ->
            case ( setupBoard model.setup, setupKomi model.setup ) of
                ( Just board, Just komi ) ->
                    let
                        fresh =
                            newBoard board model
                    in
                    ( { fresh
                        | bot =
                            if model.botAvailable then
                                model.setup.bot

                            else
                                Nothing
                        , botSimulations = model.setup.simulations
                        , sgfRoot = ( "KM", [ String.fromFloat komi ] ) :: fresh.sgfRoot
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        BotResponse value ->
            let
                decoder =
                    Decode.map4 (\id move error comment -> ( id, move, ( error, comment ) ))
                        (Decode.field "id" Decode.int)
                        (Decode.oneOf [ Decode.field "move" (Decode.nullable (Decode.map2 Spot (Decode.field "x" Decode.int) (Decode.field "y" Decode.int))), Decode.field "error" Decode.string |> Decode.map (always Nothing) ])
                        (Decode.maybe (Decode.field "error" Decode.string))
                        (Decode.maybe (Decode.field "comment" Decode.string))
            in
            case Decode.decodeValue decoder value of
                Ok ( id, move, ( error, comment ) ) ->
                    if model.botPending /= Just id then
                        ( model, Cmd.none )

                    else
                        case error of
                            Just text ->
                                ( { model | botPending = Nothing, botError = Just text }, Cmd.none )

                            Nothing ->
                                case Maybe.map (\spot -> Game.play spot model.game) move |> Maybe.withDefault (Game.pass model.game) of
                                    Just game ->
                                        let
                                            committed =
                                                commitMove { model | game = game, botPending = Nothing }

                                            annotated =
                                                case comment of
                                                    Just text ->
                                                        withReview (Review.annotate (List.length game.record) [ ( "C", [ text ] ) ] committed.review) committed

                                                    Nothing ->
                                                        committed
                                        in
                                        updatePosition annotated

                                    Nothing ->
                                        ( { model | botPending = Nothing, botError = Just "Bot returned an illegal move" }, Cmd.none )

                Err _ ->
                    ( { model | botPending = Nothing, botError = Just "Invalid bot response" }, Cmd.none )

        PassTurn ->
            if model.showAnalysis || model.botReview /= Nothing || botTurn model then
                ( model, Cmd.none )

            else
                case Game.pass model.game of
                    Just game ->
                        updatePosition (commitMove { model | game = game })

                    Nothing ->
                        ( model, Cmd.none )

        ClearCaptureAnimation ->
            ( { model | justRemoved = [] }
            , Cmd.none
            )

        Prev ->
            showGame (Game.previous model.game) (pauseBotReview model)

        BackToGame ->
            case model.botReview of
                Just live ->
                    showGame live.game
                        (withReview (Review.select live.path model.review) { model | botReview = Nothing })

                Nothing ->
                    ( model, Cmd.none )

        Next ->
            case List.drop (List.length model.game.record) model.record of
                recorded :: _ ->
                    showGame (Game.replay recorded model.game) (pauseBotReview model)

                [] ->
                    ( model, Cmd.none )

        SwitchVariation direction ->
            let
                depth =
                    List.length model.game.record

                selected =
                    withReview (Review.switch direction depth model.review) (pauseBotReview model)
            in
            showGame (gameAt model.game.board depth selected.record) selected

        SelectSibling index ->
            let
                depth =
                    List.length model.game.record

                selected =
                    withReview (Review.selectSibling depth index model.review) (pauseBotReview model)
            in
            showGame (gameAt model.game.board depth selected.record) selected

        CancelBoardInput ->
            ( cancelBoardGesture { model | pointer = PointerInput.cancel model.pointer }, Cmd.none )

        OpenMenu ->
            ( { model | showMenu = True, sgfError = Nothing, pointer = PointerInput.cancel model.pointer }
                |> cancelBoardGesture
            , Cmd.none
            )

        ClosePanels ->
            ( { model | showMenu = False, showHelp = False }, Cmd.none )

        SgfLoaded text ->
            case decodeTreeImport text of
                Ok ( board, root, trees ) ->
                    let
                        fresh =
                            newBoard board model
                    in
                    ( withReview (Review.fromTrees trees) { fresh | sgfRoot = root }, Cmd.none )

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

        TogglePolicy ->
            ( { model | showPolicy = not model.showPolicy }, Cmd.none )

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
        , botAvailable = model.botAvailable
        , setup = model.setup
        , botSerial = model.botSerial
        , sgfRoot = (Sgf.onBoard board).root
        , pointer = PointerInput.cancel model.pointer
        , analysis = { analysis | serial = model.analysis.serial }
    }


decodeImport : String -> Result String ( Board, Sgf.Record )
decodeImport text =
    decodeTreeImport text |> Result.map (\( board, root, trees ) -> ( board, Sgf.mainRecord root trees ))


decodeTreeImport : String -> Result String ( Board, List Sgf.Property, List Sgf.Tree )
decodeTreeImport text =
    Sgf.decodeTree text
        |> Result.andThen
            (\( root, trees ) ->
                Sgf.boardFromRoot root
                    |> Result.fromMaybe "Invalid SGF board size"
                    |> Result.map (\board -> ( board, root, trees ))
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
                , sgfMoves = record.moveProperties
                , review = Review.fromTrees (Sgf.fromRecord record)
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
    ( { newModel | showMenu = url.path == "/menu", botAvailable = model.botAvailable, setup = model.setup, botSerial = model.botSerial, bot = Nothing, botReview = Nothing, botPending = Nothing }, Cmd.none )


updatePosition : Model -> ( Model, Cmd Msg )
updatePosition model =
    ( { model | ghostStone = Nothing, stagedMove = Nothing, highlightedGroup = [] }
    , Cmd.none
    )


gameAt : Board -> Int -> List Play -> Game.Game
gameAt board move record =
    Game.fromRecordOn board (List.reverse (List.take move record))


pauseBotReview : { a | bot : Maybe Color, botReview : Maybe LivePosition, botPending : Maybe Int, game : Game.Game, review : Review.Model } -> { a | bot : Maybe Color, botReview : Maybe LivePosition, botPending : Maybe Int, game : Game.Game, review : Review.Model }
pauseBotReview model =
    if model.bot /= Nothing && model.botReview == Nothing then
        { model | botReview = Just { game = model.game, path = model.review.selected }, botPending = Nothing }

    else
        model


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


withReview : Review.Model -> Model -> Model
withReview review model =
    let
        record =
            Review.line model.sgfRoot review
    in
    { model | review = review, record = record.plays, sgfMoves = record.moveProperties }


commitMove : Model -> Model
commitMove model =
    case List.head model.game.record of
        Just play ->
            withReview (Review.add (List.length model.game.record - 1) play model.review) model

        Nothing ->
            model



-- VIEW


viewTreeDownload : List Sgf.Property -> List Sgf.Tree -> H.Html msg
viewTreeDownload root trees =
    viewSgfDownload (Sgf.encodeTree root trees)


viewSgfDownload : String -> H.Html msg
viewSgfDownload sgf =
    H.div [ HA.class "item" ]
        [ Pill.view "a"
            [ HA.id "sgf-download"
            , HA.href ("data:application/x-go-sgf;charset=utf-8," ++ Url.percentEncode sgf)
            , HA.download "continugo.sgf"
            , HA.attribute "aria-label" "Download SGF"
            ]
            "SGF"
            "download"
        ]


viewVariations : Model -> Svg Msg
viewVariations model =
    let
        markers =
            Review.siblings (List.length model.game.record) model.review

        marker ( index, Sgf.Node play _ _ ) =
            case play.move of
                Pass ->
                    Svg.g [] []

                Place stone ->
                    Svg.circle
                        [ SA.cx (String.fromInt stone.spot.x)
                        , SA.cy (String.fromInt stone.spot.y)
                        , SA.r (String.fromInt (Board.radius model.game.board))
                        , SA.pointerEvents "all"
                        , HA.tabindex 0
                        , HA.attribute "role" "button"
                        , HA.attribute "aria-label" ("Review alternative " ++ Sgf.encodePlay play)
                        , HE.stopPropagationOn "pointerdown" (Decode.succeed ( CancelBoardInput, True ))
                        , HE.stopPropagationOn "pointermove" (Decode.succeed ( CancelBoardInput, True ))
                        , HE.stopPropagationOn "pointerup" (Decode.succeed ( CancelBoardInput, True ))
                        , HE.onClick (SelectSibling index)
                        , HE.preventDefaultOn "keydown"
                            (Decode.field "key" Decode.string
                                |> Decode.andThen
                                    (\key ->
                                        if key == "Enter" || key == " " then
                                            Decode.succeed ( SelectSibling index, True )

                                        else
                                            Decode.fail "Not an activation key"
                                    )
                            )
                        ]
                        []
    in
    Svg.g [ SA.id "variations", SA.pointerEvents "none" ] (List.map marker markers)


viewVariationPasses : Model -> H.Html Msg
viewVariationPasses model =
    let
        pass ( index, Sgf.Node play _ _ ) =
            case play.move of
                Pass ->
                    Just (menuLink (SelectSibling index) "pass" "review alternative pass")

                Place _ ->
                    Nothing

        buttons =
            Review.siblings (List.length model.game.record) model.review
                |> List.filterMap pass
    in
    if List.isEmpty buttons || (model.showPolicy && not (List.isEmpty (nextTraining model).policy)) then
        H.text ""

    else
        H.div [ HA.id "variation-passes" ]
            (H.div [ HA.class "menu-gap", HA.attribute "aria-hidden" "true" ] [] :: buttons)


isScoring : Model -> Bool
isScoring model =
    not model.showAnalysis && (model.showScoring || Game.isFinished model.game)


viewSvg : Model -> Svg Msg
viewSvg model =
    Svg.svg
        [ SA.id "board-position"
        , SA.viewBox boardViewBox
        , SA.style ("--stone-stroke-width: " ++ String.fromFloat (toFloat (Board.radius model.game.board) / 10) ++ "px")
        ]
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
        , if model.showPolicy && not (List.isEmpty (nextTraining model).policy) then
            lazy3 Training.viewPolicy model.game.board model.game.onMove (nextTraining model).policy

          else
            viewVariations model
        , if model.showAnalysis then
            lazy3 Analysis.view model.game.board model.game.stones model.analysis

          else
            Svg.g [] []
        , lazy2 viewLastMove model.game.board model.game.record
        ]


menuLink : msg -> String -> String -> H.Html msg
menuLink action iconText tooltip =
    H.div [ HA.class "item" ]
        [ Pill.view "button" [ HE.onClick action ] iconText tooltip ]


viewBotStatus : Maybe Int -> Maybe String -> H.Html msg
viewBotStatus pending error =
    case error of
        Just message ->
            Pill.view "div" [ HA.id "bot-error", HA.attribute "role" "alert" ] "!" message

        Nothing ->
            if pending /= Nothing then
                Pill.view "div" [ HA.id "bot-thinking", HA.attribute "role" "status" ] "" "Bot thinking"

            else
                H.text ""


viewNewGames : { a | setup : PlaySetup, botAvailable : Bool, sgfError : Maybe String } -> H.Html Msg
viewNewGames model =
    let
        radio group value checked disabled message label =
            H.label []
                [ H.input
                    [ HA.type_ "radio"
                    , HA.name group
                    , HA.id (group ++ "-" ++ value)
                    , HA.value value
                    , HA.checked checked
                    , HA.disabled disabled
                    , HE.onClick message
                    ]
                    []
                , H.text (label ++ " ")
                ]
    in
    H.main_ []
        [ H.h1 [] [ H.text "ContinuGo" ]
        , H.section [ HA.id "play-setup" ]
            [ H.h2 [] [ H.text "Play" ]
            , H.fieldset [ HA.id "play-opponent" ]
                [ H.legend [] [ H.text "Opponent" ]
                , radio "play-opponent" "human" (model.setup.bot == Nothing) False (SetBot Nothing) "Human (you provide both moves)"
                , radio "play-opponent" "bot" (model.setup.bot /= Nothing) (not model.botAvailable) (SetBot (Just White)) "Bot"
                ]
            , if model.setup.bot /= Nothing then
                H.fieldset [ HA.id "play-colour" ]
                    [ H.legend [] [ H.text "Your colour" ]
                    , radio "play-colour" "black" (model.setup.bot == Just White) False (SetBot (Just White)) "Black (first move)"
                    , radio "play-colour" "white" (model.setup.bot == Just Black) False (SetBot (Just Black)) "White"
                    ]

              else
                H.text ""
            , H.fieldset [ HA.id "play-size" ]
                (H.legend [] [ H.text "Board size" ]
                    :: (List.filterMap
                            (\size ->
                                Board.fromSize size
                                    |> Maybe.map (\board -> radio "play-size" (String.fromInt size) (model.setup.customSize == Nothing && Board.size model.setup.board == size) False (SetBoard board) (String.fromInt size ++ "×" ++ String.fromInt size))
                            )
                            [ 9, 13, 19 ]
                            ++ [ radio "play-size" "custom" (model.setup.customSize /= Nothing) False (SetCustomSize (Maybe.withDefault "" model.setup.customSize)) "Custom"
                               , H.input
                                    [ HA.id "play-custom-size"
                                    , HA.type_ "number"
                                    , HA.min "7"
                                    , HA.max "25"
                                    , HA.step "1"
                                    , HA.attribute "aria-label" "Custom board size"
                                    , HA.value (Maybe.withDefault "" model.setup.customSize)
                                    , HA.disabled (model.setup.customSize == Nothing)
                                    , HE.onInput SetCustomSize
                                    ]
                                    []
                               ]
                       )
                )
            , if model.setup.bot /= Nothing then
                H.p []
                    [ H.label []
                        [ H.text ("Simulations per move: " ++ String.fromInt model.setup.simulations ++ " ")
                        , H.input
                            [ HA.id "play-simulations"
                            , HA.type_ "range"
                            , HA.min "1"
                            , HA.max "1024"
                            , HA.step "1"
                            , HA.value (String.fromInt model.setup.simulations)
                            , HE.onInput (String.toInt >> Maybe.withDefault model.setup.simulations >> SetSimulations)
                            ]
                            []
                        ]
                    ]

              else
                H.text ""
            , if setupBoard model.setup == Nothing then
                H.p [ HA.attribute "role" "alert" ] [ H.text "Board size must be a whole number from 7 to 25." ]

              else
                H.text ""
            , H.p []
                [ H.label []
                    [ H.text "Komi: "
                    , H.input [ HA.id "play-komi", HA.type_ "number", HA.step "any", HA.value model.setup.komi, HE.onInput SetKomi ] []
                    ]
                ]
            , if setupKomi model.setup == Nothing then
                H.p [ HA.attribute "role" "alert" ] [ H.text "Enter a valid komi." ]

              else
                H.text ""
            , H.button [ HA.id "play-start", HA.disabled (setupBoard model.setup == Nothing || setupKomi model.setup == Nothing), HE.onClick StartGame ] [ H.text "Start" ]
            ]
        , H.section [ HA.id "replay-setup" ]
            [ H.h2 [] [ H.text "Replay" ]
            , H.div [ HA.id "sgf-import" ]
                [ H.button [ HA.id "sgf-dropzone", HA.type_ "button" ] [ H.text "Open or drop an SGF file here" ]
                , H.textarea
                    [ HA.id "sgf-paste"
                    , HA.placeholder "Paste SGF here"
                    , HA.attribute "aria-label" "Paste SGF"
                    , HA.spellcheck False
                    , HE.onInput SgfLoaded
                    ]
                    []
                , H.input [ HA.id "sgf-file", HA.type_ "file", HA.accept ".sgf", HA.hidden True ] []
                ]
            , case model.sgfError of
                Just message ->
                    H.p [ HA.attribute "role" "alert" ] [ H.text message ]

                Nothing ->
                    H.text ""
            ]
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
        [ Pill.view "div"
            [ HA.id "move-number"
            , HA.attribute "aria-label" ("Move " ++ number ++ ": " ++ coordinates)
            ]
            number
            coordinates
        ]


viewDownload : List Sgf.Property -> List Play -> List (List Sgf.Property) -> H.Html msg
viewDownload root plays moveProperties =
    viewSgfDownload (Sgf.encodeRecord { root = root, plays = plays, moveProperties = moveProperties })


viewPlayers : List Sgf.Property -> H.Html msg
viewPlayers root =
    viewPlayerMenu root Nothing Nothing


viewPlayerMenu : List Sgf.Property -> Maybe Int -> Maybe String -> H.Html msg
viewPlayerMenu root pending error =
    let
        player property color =
            root
                |> List.filter (Tuple.first >> (==) property)
                |> List.concatMap Tuple.second
                |> List.map String.trim
                |> List.filter (not << String.isEmpty)
                |> List.head
                |> Maybe.map
                    (\name ->
                        Pill.view "div"
                            [ HA.class ("player-" ++ String.toLower color ++ " pill-" ++ String.toLower color)
                            , HA.attribute "aria-label" (color ++ ": " ++ name)
                            ]
                            (String.left 1 name)
                            name
                    )

        komi =
            Sgf.komiFromRoot root
                |> Maybe.map
                    (\value ->
                        Pill.view "div"
                            [ HA.id "komi"
                            , HA.class
                                (if value < 0 then
                                    "pill-black"

                                 else
                                    "pill-white"
                                )
                            ]
                            (String.fromFloat (abs value))
                            ("Komi: " ++ String.fromFloat value)
                    )
    in
    H.div [ HA.id "players", HA.class "pills" ]
        (List.filterMap identity [ player "PB" "Black", player "PW" "White", komi ]
            ++ (if pending /= Nothing || error /= Nothing then
                    [ viewBotStatus pending error ]

                else
                    []
               )
        )


trainingAt : Int -> Model -> Training.Annotation
trainingAt index model =
    if index < 0 then
        Training.empty

    else
        model.sgfMoves
            |> List.drop index
            |> List.head
            |> Maybe.map Training.annotation
            |> Maybe.withDefault Training.empty


nextTraining : Model -> Training.Annotation
nextTraining model =
    trainingAt (List.length model.game.record) model


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

                    komi =
                        Sgf.komiFromRoot model.sgfRoot |> Maybe.withDefault 0
                in
                H.div [ HA.id "score" ]
                    [ H.div [ HA.class "score-result" ] [ H.text (TerritoryView.resultString model.game.board komi territory) ]
                    , H.div [ HA.class "score-areas" ]
                        [ H.text ("Black " ++ TerritoryView.formatArea model.game.board territory.black ++ " · White " ++ TerritoryView.formatArea model.game.board (territory.white + komi * TerritoryView.pointArea model.game.board)) ]
                    ]

            else
                H.div [] []

        content =
            if model.showMenu then
                viewNewGames model

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
        [ H.div [ HA.id "menu", HA.class "pills", HA.hidden model.showMenu ]
            [ menuLink OpenMenu "!" "menu"
            , menuLink ToggleHelp "?" "help"
            , lazy2 viewTreeDownload model.sgfRoot model.review.trees
            , H.div [ HA.class "menu-gap", HA.attribute "aria-hidden" "true" ] []
            , menuLink Prev "←" "prev"
            , menuLink Next "→" "next"
            , menuLink (SwitchVariation -1) "↑" "previous line"
            , menuLink (SwitchVariation 1) "↓" "next line"
            , lazy viewMoveNumber model.game.record
            , H.div [ HA.class "menu-gap", HA.attribute "aria-hidden" "true" ] []
            , if model.botReview /= Nothing then
                H.div [ HA.class "item" ]
                    [ Pill.view "button" [ HA.id "back-to-game", HE.onClick BackToGame ] "↩" "Back to game" ]

              else
                H.text ""
            , menuLink PassTurn "*" "pass"
            , H.div [ HA.class "item" ]
                [ Pill.view "button"
                    [ HA.id "score-toggle"
                    , HA.classList [ ( "active", isScoring model ) ]
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
                    "%"
                    "score"
                ]
            , H.div [ HA.class "item" ]
                [ Pill.view "button"
                    [ HA.id "analysis-toggle"
                    , HA.classList [ ( "active", model.showAnalysis ) ]
                    , HA.attribute "aria-label" "Toggle analysis mode"
                    , HA.attribute "aria-pressed"
                        (if model.showAnalysis then
                            "true"

                         else
                            "false"
                        )
                    , HE.onClick ToggleAnalysis
                    ]
                    "o"
                    "analysis"
                ]
            , Training.viewMenu TogglePolicy
                model.showPolicy
                model.game.onMove
                (trainingAt (List.length model.game.record - 1) model)
                (nextTraining model).policy
            , viewVariationPasses model
            ]
        , if model.showMenu then
            H.text ""

          else
            lazy3 viewPlayerMenu model.sgfRoot model.botPending model.botError
        , content
        ]
    }
