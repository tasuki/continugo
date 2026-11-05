module DragFrom exposing (..)

import BoardGeometry exposing (Border(..), adjacentDistance, borderDistance, borderReach, connectedBorders, distance)
import List.Extra
import Placement
import Rules.Board as Board exposing (Board)
import Rules.Position exposing (..)


type DragFrom
    = DragNone
    | DragSpot Spot
    | DragStone Stone
    | DragBorders Spot (List Border)


getDragFrom : Board -> Stones -> Color -> Spot -> DragFrom
getDragFrom board stones onMove coords =
    let
        isOverOwn : Stone -> Bool
        isOverOwn s =
            s.color == onMove && isWithinStoneOn board s.spot coords

        maybeOverStone : Maybe Stone
        maybeOverStone =
            stoneList stones |> List.Extra.find isOverOwn
    in
    case maybeOverStone of
        Just stone ->
            DragStone stone

        Nothing ->
            case Placement.nearbySpot board stones coords |> Maybe.map (connectedBorders board) of
                Just ((_ :: _) as borders) ->
                    DragBorders coords borders

                _ ->
                    DragSpot coords


isTap : Spot -> DragFrom -> Bool
isTap coords origin =
    case origin of
        DragSpot start ->
            start == coords

        DragBorders start _ ->
            start == coords

        _ ->
            False


desiredCenter : Board -> Spot -> Spot -> Spot
desiredCenter board from to =
    let
        reach =
            adjacentDistance board

        dragDistance =
            distance to from
    in
    if dragDistance < reach then
        to

    else
        let
            ratio : Float
            ratio =
                reach / dragDistance

            dx =
                toFloat (to.x - from.x)

            dy =
                toFloat (to.y - from.y)
        in
        { x = from.x + floor (ratio * dx)
        , y = from.y + floor (ratio * dy)
        }


findDragged : Board -> Stones -> Color -> Stone -> Spot -> Stone
findDragged board stones onMove draggedFrom draggedTo =
    findDraggedSpot board (List.map .spot (stoneList stones)) draggedFrom.spot draggedTo
        |> Maybe.withDefault (desiredCenter board draggedFrom.spot draggedTo)
        |> createStone onMove


{-| Geometry only: analysis circles obstruct placement just like real stones.
Unlike a stone, a circle has no color or move-legality constraints.
-}
findDraggedSpot : Board -> List Spot -> Spot -> Spot -> Maybe Spot
findDraggedSpot board spotList from draggedTo =
    findNearestSpot board spotList (desiredCenter board from draggedTo) draggedTo (\candidate -> adjacentOn board candidate from)


findBorderDraggedSpot : Board -> List Spot -> List Border -> Spot -> Maybe Spot
findBorderDraggedSpot board spotList borders draggedTo =
    let
        r =
            Board.radius board

        reach =
            borderReach board

        constrain border spot =
            case border of
                Left ->
                    { spot | x = clamp r reach spot.x }

                Right ->
                    { spot | x = clamp (Board.coordRange - reach) (Board.coordRange - r) spot.x }

                Top ->
                    { spot | y = clamp r reach spot.y }

                Bottom ->
                    { spot | y = clamp (Board.coordRange - reach) (Board.coordRange - r) spot.y }

        connected candidate =
            List.all (\border -> borderDistance border candidate <= reach) borders
    in
    findNearestSpot board spotList (List.foldl constrain draggedTo borders) draggedTo connected


findNearestSpot : Board -> List Spot -> Spot -> Spot -> (Spot -> Bool) -> Maybe Spot
findNearestSpot board spotList desiredSpot draggedTo allowed =
    let
        findNearest : Spot -> Maybe ( Spot, Float ) -> Maybe ( Spot, Float )
        findNearest zeroCandidate current =
            let
                currentDistance =
                    Maybe.map Tuple.second current |> Maybe.withDefault 1000000
            in
            findCandidate zeroCandidate current currentDistance

        findCandidate zeroCandidate current currentDistance =
            let
                candidate =
                    translate desiredSpot zeroCandidate

                candidateDistance =
                    distance candidate draggedTo
            in
            if
                (candidateDistance < currentDistance)
                    && isWithinBoardOn board candidate
                    && allowed candidate
                    -- the not overlapsAny here is the hot path
                    && (not <| overlapsAnyOn board candidate spotList)
            then
                Just ( candidate, candidateDistance )

            else
                current
    in
    List.foldl findNearest Nothing (Placement.candidateOffsets board)
        |> Maybe.map Tuple.first
