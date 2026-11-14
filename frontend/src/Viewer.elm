port module Viewer exposing (Config, Model, Msg(..), init, main, update)

import Analysis
import BoardHover
import BoardScene
import Browser
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import MovePreview
import Rules.Board as Board
import Rules.Game as Game
import Rules.Position exposing (Spot)
import Sgf
import Svg
import Svg.Attributes as SA
import Svg.Lazy exposing (lazy2, lazy3, lazy8)
import Training


port configure : (Config -> msg) -> Sub msg


port pointer : (Maybe { x : Float, y : Float } -> msg) -> Sub msg


type alias Config =
    { sgf : String
    , move : Maybe Int
    , showPolicy : Bool
    , hover : Bool
    }


type alias Model =
    { config : Config
    , record : Sgf.Record
    , game : Game.Game
    , positions : Dict Int Game.Game
    , ghostStone : Maybe MovePreview.Preview
    , highlightedGroup : List Spot
    , justPlayed : Maybe Spot
    , error : Maybe String
    }


type Msg
    = Configure Config
    | Pointer (Maybe { x : Float, y : Float })


main : Program Config Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.batch [ configure Configure, pointer Pointer ]
        , view = view
        }


init : Config -> ( Model, Cmd Msg )
init config =
    ( load config
        { config = config
        , record = Sgf.empty
        , game = Game.empty
        , positions = Dict.empty
        , ghostStone = Nothing
        , highlightedGroup = []
        , justPlayed = Nothing
        , error = Nothing
        }
    , Cmd.none
    )


load : Config -> Model -> Model
load config model =
    let
        decoded =
            if config.sgf == "" then
                Ok Sgf.empty

            else
                Sgf.decodeRecord config.sgf

        result =
            decoded
                |> Result.andThen
                    (\record ->
                        Sgf.boardFromRoot record.root
                            |> Result.fromMaybe "Unsupported board size."
                            |> Result.map (\board -> ( record, board ))
                    )
    in
    case result of
        Err error ->
            { model | config = config, error = Just error, ghostStone = Nothing, highlightedGroup = [] }

        Ok ( record, board ) ->
            seek config
                { model
                    | record = record
                    , positions = Dict.singleton 0 (Game.onBoard board)
                    , error = Nothing
                }


{-| Cache immutable positions, and only apply moves not replayed before.
Changing display options or revisiting a ply never reruns the rules engine.
-}
seek : Config -> Model -> Model
seek config model =
    let
        move =
            config.move |> Maybe.withDefault (List.length model.record.plays) |> clamp 0 (List.length model.record.plays)

        ( start, game ) =
            model.positions
                |> Dict.toList
                |> List.filter (Tuple.first >> (>=) move)
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ( 0, Game.empty )

        ( position, positions ) =
            model.record.plays
                |> List.drop start
                |> List.take (move - start)
                |> List.indexedMap (\index play -> ( start + index + 1, play ))
                |> List.foldl
                    (\( ply, play ) ( previous, cache ) ->
                        let
                            next =
                                Game.replay play previous
                        in
                        ( next, Dict.insert ply next cache )
                    )
                    ( game, model.positions )
    in
    { model
        | config = config
        , game = position
        , positions = positions
        , ghostStone = Nothing
        , highlightedGroup = []
        , justPlayed = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Configure config ->
            ( if config.sgf == model.config.sgf && model.error == Nothing then
                seek config model

              else
                load config model
            , Cmd.none
            )

        Pointer point ->
            case point of
                Just position ->
                    if model.config.hover && model.error == Nothing then
                        ( BoardHover.at model
                            { x = round (position.x * toFloat Board.coordRange)
                            , y = round (position.y * toFloat Board.coordRange)
                            }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( { model | ghostStone = Nothing, highlightedGroup = [] }, Cmd.none )


view : Model -> H.Html Msg
view model =
    case model.error of
        Just error ->
            H.div [ HA.attribute "role" "alert" ] [ H.text error ]

        Nothing ->
            H.div [ HA.id "board", HA.attribute "aria-label" "ContinuGo position" ]
                [ lazy2 BoardScene.viewBackground model.game.board model.game.stones
                , Svg.svg
                    [ SA.id "board-position"
                    , SA.viewBox BoardScene.boardViewBox
                    , SA.style ("--stone-stroke-width: " ++ String.fromFloat (toFloat (Board.radius model.game.board) / 10) ++ "px")
                    ]
                    [ lazy8 BoardScene.viewPosition model.game.board model.game.stones model.ghostStone model.highlightedGroup Nothing [] BoardScene.Normal Analysis.empty
                    , if model.config.showPolicy then
                        lazy3 Training.viewPolicy
                            model.game.board
                            model.game.onMove
                            (model.record.moveProperties
                                |> List.drop (List.length model.game.record)
                                |> List.head
                                |> Maybe.map Training.annotation
                                |> Maybe.withDefault Training.empty
                                |> .policy
                            )

                      else
                        Svg.g [] []
                    , lazy2 BoardScene.viewLastMove model.game.board model.game.record
                    ]
                ]
