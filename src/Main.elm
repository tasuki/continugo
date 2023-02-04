module Main exposing (..)

import Browser
import Browser.Dom as BD
import Browser.Events
import Dict
import Go exposing (..)
import Html as H
import Html.Attributes as HA
import Json.Decode as D
import Liberties exposing (findNearestPlayable)
import Play exposing (groupAndItsLiberties, playIfLegal)
import SamplePositions
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
    , highlightedGroup : List Spot
    , highlightedLiberties : List Spot
    , onMove : Player
    }


init : () -> ( Model, Cmd Msg )
init _ =
    update (PlayStones SamplePositions.empty)
        { stones = Dict.empty
        , ghostStone = Nothing
        , highlightedGroup = []
        , highlightedLiberties = []
        , onMove = Black
        }



-- UPDATE


type Msg
    = Clicked Spot
    | MouseMoved Spot
    | PlayIfLegal Spot (Result BD.Error BD.Element)
    | Hover Spot (Result BD.Error BD.Element)
    | PlayStones (List Stone)


handleHover : Model -> Spot -> Model
handleHover model hoverSpot =
    let
        nearby =
            nearbyStones model.stones hoverSpot

        maybeOver =
            nearby
                |> List.filter (\s -> distance hoverSpot s.spot < stoneR)
                |> List.head
    in
    case maybeOver of
        Just over ->
            let
                ( group, liberties ) =
                    groupAndItsLiberties model.stones over
            in
            { model
                | highlightedGroup = group
                , highlightedLiberties = liberties
                , ghostStone = Nothing
            }

        Nothing ->
            let
                maybePlay : Spot -> Maybe Stone
                maybePlay spot =
                    playIfLegal (createStone model.onMove spot) model.stones
                        |> Maybe.andThen (Dict.get ( spot.x, spot.y ))
            in
            { model
                | ghostStone =
                    findNearestPlayable hoverSpot (List.map .spot nearby)
                        |> Maybe.andThen maybePlay
                , highlightedGroup = []
                , highlightedLiberties = []
            }


handlePlay : Model -> Spot -> Model
handlePlay model playSpot =
    let
        nearby =
            nearbyStones model.stones playSpot

        maybeStones =
            findNearestPlayable playSpot (List.map .spot nearby)
                |> Maybe.andThen (\np -> playIfLegal (createStone model.onMove np) model.stones)
    in
    case maybeStones of
        Just s ->
            { model
                | stones = s
                , onMove = otherPlayer model.onMove
            }

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked clickedCoords ->
            ( model, BD.getElement "board" |> Task.attempt (PlayIfLegal clickedCoords) )

        PlayIfLegal clickedCoords (Ok element) ->
            ( handlePlay model (toBoardCoords clickedCoords element)
            , Cmd.none
            )

        MouseMoved hoverCoords ->
            ( model, BD.getElement "board" |> Task.attempt (Hover hoverCoords) )

        Hover hoverCoords (Ok element) ->
            ( handleHover model (toBoardCoords hoverCoords element)
            , Cmd.none
            )

        PlayStones stonesList ->
            let
                playStone stone acc =
                    playIfLegal stone acc |> Maybe.withDefault acc

                newStones =
                    List.foldl playStone model.stones stonesList
            in
            ( { model | stones = newStones }, Cmd.none )

        _ ->
            ( model, Cmd.none )


toBoardCoords : Spot -> BD.Element -> Spot
toBoardCoords windowCoords element =
    { x =
        round <|
            (toFloat windowCoords.x - element.element.x)
                * (toFloat coordRange / element.element.width)
    , y =
        round <|
            (toFloat windowCoords.y - element.element.y)
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


viewHighlight : Spot -> Svg Msg
viewHighlight spot =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        , SA.stroke "#900"
        , SA.strokeWidth "5"
        , SA.fill "transparent"
        ]
        []


viewLiberty : Spot -> Svg Msg
viewLiberty spot =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        , SA.stroke "#BB9"
        , SA.strokeWidth "5"
        , SA.fill "transparent"
        ]
        []


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
                , lazy3 Svg.node "g" [ SA.id "highlights" ] <|
                    (model.highlightedGroup |> List.map viewHighlight)
                , lazy3 Svg.node "g" [ SA.id "liberties" ] <|
                    (model.highlightedLiberties |> List.map viewLiberty)
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
