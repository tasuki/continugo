module DragFrom exposing (..)

import BoardGeometry exposing (adjacentDistance, distance)
import List.Extra
import Placement
import Rules.Board exposing (Board)
import Rules.Position exposing (..)


type DragFrom
    = DragNone
    | DragSpot Spot
    | DragStone Stone


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
            DragSpot coords


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
    let
        desiredSpot : Spot
        desiredSpot =
            desiredCenter board from draggedTo

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
                    && adjacentOn board candidate from
                    -- the not overlapsAny here is the hot path
                    && (not <| overlapsAnyOn board candidate spotList)
            then
                Just ( candidate, candidateDistance )

            else
                current
    in
    List.foldl findNearest Nothing (Placement.candidateOffsets board)
        |> Maybe.map Tuple.first
