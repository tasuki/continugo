module Main exposing (..)

import Browser
import Browser.Dom as BD
import Browser.Events
import Go exposing (..)
import Html as H
import Html.Attributes as HA
import Json.Decode as D
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import Svg.Lazy exposing (..)
import Task


boardSize =
    13


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
    , links : Links
    , ghostStone : Maybe Stone
    , ghostLinks : Links
    , onMove : Player
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { stones = []
      , links = []
      , ghostStone = Nothing
      , ghostLinks = []
      , onMove = Black
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Clicked Coords
    | MouseMoved Coords
    | CheckAgainstBoard Coords (Result BD.Error BD.Element)
    | ShowGhost Coords (Result BD.Error BD.Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked clickedCoords ->
            ( model, BD.getElement "board" |> Task.attempt (CheckAgainstBoard clickedCoords) )

        CheckAgainstBoard clickedCoords (Ok element) ->
            let
                stone =
                    ( model.onMove, toBoardCoords clickedCoords element )
            in
            case playIfLegal stone model.stones of
                Just s ->
                    ( { model
                        | stones = s
                        , links = model.links ++ newLinks stone model.stones
                        , onMove = otherPlayer model.onMove
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        MouseMoved hoverCoords ->
            ( model, BD.getElement "board" |> Task.attempt (ShowGhost hoverCoords) )

        ShowGhost hoverCoords (Ok element) ->
            let
                stone =
                    ( model.onMove, toBoardCoords hoverCoords element )
            in
            case playIfLegal stone model.stones of
                Just s ->
                    ( { model
                        | ghostStone = Just stone
                        , ghostLinks = newLinks stone model.stones
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | ghostStone = Nothing
                        , ghostLinks = []
                      }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


toBoardCoords : Coords -> BD.Element -> Coords
toBoardCoords clickedCoords element =
    { x =
        round <|
            (toFloat clickedCoords.x - element.element.x)
                * (toFloat coordRange / element.element.width)
    , y =
        round <|
            (toFloat clickedCoords.y - element.element.y)
                * (toFloat coordRange / element.element.height)
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onClick
            (D.map Clicked
                (D.map2 Coords
                    (D.field "pageX" D.int)
                    (D.field "pageY" D.int)
                )
            )
        , Browser.Events.onMouseMove
            (D.map MouseMoved
                (D.map2 Coords
                    (D.field "pageX" D.int)
                    (D.field "pageY" D.int)
                )
            )
        ]



-- VIEW


viewStone : Stone -> Svg Msg
viewStone ( player, coords ) =
    let
        color =
            case player of
                Black ->
                    "#000"

                White ->
                    "#FFF"
    in
    Svg.circle
        [ SA.cx <| String.fromInt coords.x
        , SA.cy <| String.fromInt coords.y
        , SA.r <| String.fromInt stoneR
        , SA.stroke "black"
        , SA.strokeWidth "5"
        , SA.fill color
        ]
        []


viewLink : ( Stone, Stone ) -> Svg Msg
viewLink ( ( _, c1 ), ( _, c2 ) ) =
    Svg.line
        [ SA.x1 <| String.fromInt c1.x
        , SA.y1 <| String.fromInt c1.y
        , SA.x2 <| String.fromInt c2.x
        , SA.y2 <| String.fromInt c2.y
        , SA.stroke "black"
        , SA.strokeWidth "5"
        ]
        []


coordsKeyStr : Coords -> String
coordsKeyStr c =
    "-" ++ String.fromInt c.x ++ "-" ++ String.fromInt c.y


viewKeyedStone : Stone -> ( String, Svg Msg )
viewKeyedStone ( player, coords ) =
    ( "stone" ++ coordsKeyStr coords
    , lazy viewStone ( player, coords )
    )


viewKeyedLink : ( Stone, Stone ) -> ( String, Svg Msg )
viewKeyedLink link =
    let
        ( ( _, c1 ), ( _, c2 ) ) =
            link
    in
    ( "link" ++ coordsKeyStr c1 ++ coordsKeyStr c2
    , lazy viewLink link
    )


viewGhostMove : Stone -> Svg Msg
viewGhostMove stone =
    Svg.g [ SA.opacity "0.5" ] [ viewStone stone ]


viewGhostLink : ( Stone, Stone ) -> Svg Msg
viewGhostLink link =
    Svg.g [ SA.opacity "0.5" ] [ viewLink link ]


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ H.div [ HA.id "board" ]
            [ Svg.svg
                [ SA.viewBox <| intsToStr [ 0, 0, coordRange, coordRange ] ]
                [ lazy3 Svg.node "g" [ SA.id "ghostLinks" ] <| List.map viewGhostLink model.ghostLinks
                , lazy3 Svg.node "g" [ SA.id "ghostStone" ] <| List.filterMap identity [ Maybe.map viewGhostMove model.ghostStone ]
                , lazy3 Svg.Keyed.node "g" [ SA.id "links" ] <| List.map viewKeyedLink model.links
                , lazy3 Svg.Keyed.node "g" [ SA.id "stones" ] <| List.map viewKeyedStone model.stones
                ]
            ]
        ]
    }


intsToStr : List Int -> String
intsToStr ints =
    List.map String.fromInt ints |> String.join " "
