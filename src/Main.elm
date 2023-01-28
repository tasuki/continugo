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


coordRange =
    1000


boardSize =
    13


stoneR =
    -- 9x9: 55.6, 13x13: 38.5, 19x19: 26.3
    38


connectedDistance =
    1.4142


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


type alias Stones =
    List Stone


type alias Links =
    List ( Stone, Stone )


type alias Stone =
    ( Player, Coords )


type Player
    = Black
    | White


type alias Coords =
    { x : Int, y : Int }


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


otherPlayer : Player -> Player
otherPlayer p =
    case p of
        White ->
            Black

        Black ->
            White


canPlay : Stone -> Stones -> Bool
canPlay stone stones =
    True


withinBoard : Coords -> Bool
withinBoard coords =
    let
        isWithin : Int -> Bool
        isWithin coord =
            coord > stoneR && coord < coordRange - stoneR
    in
    isWithin coords.x && isWithin coords.y


distance : Stone -> Stone -> Float
distance ( _, c1 ) ( _, c2 ) =
    sqrt <| toFloat ((c1.x - c2.x) ^ 2 + (c1.y - c2.y) ^ 2)


overlaps : Stone -> Stones -> Bool
overlaps stone =
    List.any (\s -> distance stone s < 2 * stoneR)


newLinks : Stone -> Stones -> Links
newLinks stone =
    let
        maybeLink s1 s2 =
            if distance s1 s2 < 2 * stoneR * connectedDistance then
                Just ( s1, s2 )

            else
                Nothing
    in
    List.filterMap (maybeLink stone)


playIfLegal : Stone -> Stones -> Maybe Stones
playIfLegal ( player, coords ) stones =
    if withinBoard coords then
        if not <| overlaps ( player, coords ) stones then
            Just <| ( player, coords ) :: stones

        else
            Nothing

    else
        Nothing



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


coordsWithinBoard : Coords -> Bool
coordsWithinBoard coords =
    coords.x >= 0 && coords.x <= coordRange && coords.y >= 0 && coords.y <= coordRange


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
                ([]
                    ++ List.map viewGhostLink model.ghostLinks
                    ++ List.filterMap identity [ Maybe.map viewGhostMove model.ghostStone ]
                    ++ List.map viewLink model.links
                    ++ List.map viewStone model.stones
                )
            ]
        ]
    }


intsToStr : List Int -> String
intsToStr ints =
    List.map String.fromInt ints |> String.join " "
