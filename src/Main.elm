module Main exposing (..)

import Browser
import Browser.Dom as BD
import Browser.Events
import Dict
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
    , ghostStone : Maybe Stone
    , onMove : Player
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { stones = Dict.empty
      , ghostStone = Nothing
      , onMove = Black
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Clicked Spot
    | MouseMoved Spot
    | CheckAgainstBoard Spot (Result BD.Error BD.Element)
    | ShowGhost Spot (Result BD.Error BD.Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked clickedCoords ->
            ( model, BD.getElement "board" |> Task.attempt (CheckAgainstBoard clickedCoords) )

        CheckAgainstBoard clickedCoords (Ok element) ->
            let
                stone =
                    createStone model.onMove (toBoardCoords clickedCoords element)
            in
            case playIfLegal stone model.stones of
                Just s ->
                    ( { model
                        | stones = s
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
                    createStone model.onMove (toBoardCoords hoverCoords element)
            in
            ( { model
                | ghostStone =
                    playIfLegal stone model.stones
                        |> Maybe.andThen (Dict.get (stoneKey stone))
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


toBoardCoords : Spot -> BD.Element -> Spot
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


viewStone : Stone -> Svg Msg
viewStone { player, spot } =
    let
        color =
            case player of
                Black ->
                    "#000"

                White ->
                    "#FFF"
    in
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        , SA.stroke "black"
        , SA.strokeWidth "5"
        , SA.fill color
        ]
        []


viewGhostStone : Stone -> Svg Msg
viewGhostStone stone =
    Svg.g [ SA.opacity "0.5" ] [ viewStone stone ]


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


viewGhostLink : ( Spot, Spot ) -> Svg Msg
viewGhostLink link =
    Svg.g [ SA.opacity "0.5" ] [ viewLink link ]



-- Keyed


spotKeyStr : Spot -> String
spotKeyStr s =
    "-" ++ String.fromInt s.x ++ "-" ++ String.fromInt s.y


viewKeyedStone : Stone -> ( String, Svg Msg )
viewKeyedStone stone =
    ( "stone" ++ spotKeyStr stone.spot
    , lazy viewStone stone
    )


viewKeyedLink : ( Spot, Spot ) -> ( String, Svg Msg )
viewKeyedLink ( s1, s2 ) =
    ( "link" ++ spotKeyStr s1 ++ spotKeyStr s2
    , lazy viewLink ( s1, s2 )
    )



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
                [ lazy3 Svg.node "g" [ SA.id "ghostLinks" ] <|
                    List.map viewGhostLink (Maybe.map getStoneLinks model.ghostStone |> Maybe.withDefault [])
                , lazy3 Svg.node "g" [ SA.id "ghostStone" ] <|
                    List.filterMap identity [ Maybe.map viewGhostStone model.ghostStone ]
                , lazy3 Svg.Keyed.node "g" [ SA.id "links" ] <|
                    List.map viewKeyedLink (getUniqueLinks model.stones)
                , lazy3 Svg.Keyed.node "g" [ SA.id "stones" ] <|
                    (stoneList model.stones |> List.map viewKeyedStone)
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
