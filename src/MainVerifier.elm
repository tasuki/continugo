port module MainVerifier exposing (..)

import Dict
import Go
import Maybe.Extra
import Play
import Sgf


type alias InputType =
    { game : String
    , move : String
    }


type alias OutputType =
    Bool



-- Ceremony


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


port get : (InputType -> msg) -> Sub msg


port put : OutputType -> Cmd msg


type alias Model =
    ()


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


type Msg
    = GotData InputType


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotData value) _ =
    ( (), put <| processInput value )


subscriptions : Model -> Sub Msg
subscriptions _ =
    get GotData



-- Actual code


processInput : InputType -> OutputType
processInput input =
    let
        record : List Go.Play
        record =
            Sgf.decode input.game

        stones : Go.Stones
        stones =
            Play.playStones record Dict.empty
    in
    case Sgf.decodePlay input.move of
        Nothing ->
            False

        Just play ->
            case play.move of
                Go.Pass ->
                    True

                Go.Place s ->
                    Play.playIfLegal stones s
                        |> Maybe.Extra.isJust
