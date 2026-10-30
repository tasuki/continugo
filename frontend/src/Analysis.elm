module Analysis exposing (..)

import Coverage
import DragFrom
import List.Extra
import Placement
import Rules.Board as Board exposing (Board)
import Rules.Position exposing (..)
import StoneView exposing (spotBorderNearestTo, viewLink)
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias Gesture =
    { start : Spot
    , origin : Maybe Spot
    , dragged : Bool
    , repeated : Bool
    }


type alias Model =
    { circles : List Spot
    , gesture : Maybe Gesture
    , preview : Maybe Spot
    , pendingRemoval : Maybe ( Int, Spot )
    , serial : Int
    , invalid : Maybe Spot
    }


empty : Model
empty =
    { circles = [], gesture = Nothing, preview = Nothing, pendingRemoval = Nothing, serial = 0, invalid = Nothing }


type Effect
    = NoEffect
    | ScheduleRemoval Int
    | Propose Spot


{-| Leave the scratch layer intact, but invalidate outstanding gestures/timers.
-}
cancel : Model -> Model
cancel model =
    { model | gesture = Nothing, preview = Nothing, pendingRemoval = Nothing, invalid = Nothing }


occupied : Stones -> Model -> List Spot
occupied stones model =
    List.map .spot (stoneList stones) ++ model.circles


fits : Board -> List Spot -> Spot -> Bool
fits board blockers spot =
    isWithinBoardOn board spot && not (overlapsAnyOn board spot blockers)


{-| Retain annotations across toggles/history, dropping circles occupied by a
newly played stone. A proposal alone never removes any circles.
-}
reconcile : Board -> Stones -> Model -> Model
reconcile board stones model =
    let
        cleared =
            cancel model
    in
    { cleared | circles = List.filter (fits board (List.map .spot (stoneList stones))) model.circles }


start : Board -> Stones -> Spot -> Model -> Model
start board stones coords model =
    let
        origin =
            List.Extra.find (isWithinStoneOn board coords) (occupied stones model)

        repeated =
            Maybe.map Tuple.second model.pendingRemoval == origin && origin /= Nothing

        circles =
            case model.pendingRemoval of
                Just ( _, spot ) ->
                    if repeated then
                        model.circles

                    else
                        List.filter ((/=) spot) model.circles

                Nothing ->
                    model.circles
    in
    { model
        | circles = circles
        , gesture = Just { start = coords, origin = origin, dragged = False, repeated = repeated }
        , pendingRemoval = Nothing
        , preview = Nothing
        , invalid = Nothing
    }


clearHover : Model -> Model
clearHover model =
    case model.gesture of
        Nothing ->
            { model | preview = Nothing }

        Just _ ->
            model


move : Board -> Stones -> Spot -> Model -> Model
move board stones coords model =
    case model.gesture of
        Nothing ->
            let
                blockers =
                    occupied stones model

                preview =
                    if
                        coords.x
                            < 0
                            || coords.x
                            > Board.coordRange
                            || coords.y
                            < 0
                            || coords.y
                            > Board.coordRange
                            || List.any (isWithinStoneOn board coords) blockers
                    then
                        Nothing

                    else
                        Placement.nearestFreeSpot board coords blockers
            in
            if preview == model.preview then
                model

            else
                { model | preview = preview }

        Just gesture ->
            let
                dragged =
                    gesture.dragged || distanceSquared gesture.start coords >= 8 * 8

                preview =
                    if not dragged then
                        Nothing

                    else
                        case gesture.origin of
                            Just from ->
                                DragFrom.findDraggedSpot board (occupied stones model) from coords

                            Nothing ->
                                Placement.nearestFreeSpot board coords (occupied stones model)
            in
            { model | gesture = Just { gesture | dragged = dragged }, preview = preview }


finish : Board -> Stones -> Spot -> Model -> ( Model, Effect )
finish board stones coords previous =
    let
        model =
            move board stones coords previous

        cleared =
            { model | gesture = Nothing, preview = Nothing }
    in
    case model.gesture of
        Nothing ->
            ( cleared, NoEffect )

        Just gesture ->
            if gesture.dragged then
                ( { cleared | circles = model.circles ++ Maybe.withDefault [] (Maybe.map List.singleton model.preview) }, NoEffect )

            else
                case gesture.origin of
                    Just origin ->
                        if not (List.member origin model.circles) then
                            ( cleared, NoEffect )

                        else if gesture.repeated then
                            ( cleared, Propose origin )

                        else
                            let
                                serial =
                                    model.serial + 1
                            in
                            ( { cleared | serial = serial, pendingRemoval = Just ( serial, origin ) }, ScheduleRemoval serial )

                    Nothing ->
                        case Placement.nearestFreeSpot board coords (occupied stones model) of
                            Just spot ->
                                ( { cleared | circles = model.circles ++ [ spot ] }, NoEffect )

                            Nothing ->
                                ( cleared, NoEffect )


expire : Int -> Model -> Model
expire serial model =
    case model.pendingRemoval of
        Just ( token, spot ) ->
            if token == serial then
                { model | circles = List.filter ((/=) spot) model.circles, pendingRemoval = Nothing }

            else
                model

        Nothing ->
            model


links : Board -> Stones -> List Spot -> List ( Spot, Spot )
links board stones circles =
    let
        withRest remaining =
            case remaining of
                [] ->
                    []

                circle :: rest ->
                    List.filter (adjacentOn board circle) (List.map .spot (stoneList stones) ++ rest)
                        |> List.map (Tuple.pair circle)
                        |> (++) (withRest rest)
    in
    withRest circles


visibleCircles : List Spot -> Maybe Spot -> List Spot
visibleCircles circles preview =
    circles ++ Maybe.withDefault [] (Maybe.map List.singleton preview)


viewDeadZones : Board -> Stones -> List Spot -> Maybe Spot -> Svg msg
viewDeadZones board stones circles preview =
    Coverage.viewSpots board (List.map .spot (stoneList stones) ++ visibleCircles circles preview)


view : Board -> Stones -> Model -> Svg msg
view board stones model =
    let
        circles =
            visibleCircles model.circles model.preview

        circle spot =
            Svg.circle
                [ SA.cx (String.fromInt spot.x)
                , SA.cy (String.fromInt spot.y)
                , SA.r (String.fromInt (Board.radius board))
                , SA.class
                    ((if Just spot == model.preview then
                        "preview "

                      else
                        ""
                     )
                        ++ (if Just spot == model.invalid then
                                "invalid"

                            else
                                ""
                           )
                    )
                ]
                []

        link ( a, b ) =
            viewLink ( spotBorderNearestTo board b a, spotBorderNearestTo board a b )
    in
    Svg.g [ SA.id "analysis-layer", SA.pointerEvents "none" ]
        [ Svg.g [ SA.id "analysis-links" ] (List.map link (links board stones circles))
        , Svg.g [ SA.id "analysis-circles" ] (List.map circle circles)
        ]
