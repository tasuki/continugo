module Main exposing (..)

import Browser
import Browser.Dom as BD
import Browser.Events
import Html as H
import Html.Attributes as HA
import Json.Decode as D
import Svg exposing (Svg)
import Svg.Attributes as SA
import Task


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Coords =
    { x : Int, y : Int }


type alias Model =
    { clicked : Maybe Coords
    , element : Maybe BD.Element
    , message : String
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { clicked = Nothing
      , element = Nothing
      , message = "hi there"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Clicked Coords
    | CheckAgainstBoard (Result BD.Error BD.Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked coords ->
            ( { model | clicked = Just coords }
            , BD.getElement "board" |> Task.attempt CheckAgainstBoard
            )

        CheckAgainstBoard (Ok element) ->
            ( { model | element = Just element }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onClick
        (D.map Clicked
            (D.map2 Coords
                (D.field "pageX" D.int)
                (D.field "pageY" D.int)
            )
        )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ H.div [ HA.id "board" ]
            [ Svg.svg
                [ SA.viewBox "0 0 1000 1000" ]
                [ Svg.text_ [ SA.x "100", SA.y "100" ] [ Svg.text <| Debug.toString model.clicked ]
                , Svg.text_ [ SA.x "100", SA.y "200" ] [ Svg.text <| Debug.toString (Maybe.map .element model.element) ]
                ]
            ]
        ]
    }
