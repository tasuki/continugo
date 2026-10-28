module DragFrom exposing (..)

import List.Extra
import Placement
import Rules.Position exposing (..)


type DragFrom
    = DragNone
    | DragSpot Spot
    | DragStone Stone


getDragFrom : Stones -> Color -> Spot -> DragFrom
getDragFrom stones onMove coords =
    let
        isOverOwn : Stone -> Bool
        isOverOwn s =
            s.color == onMove && isWithinStone s.spot coords

        maybeOverStone : Maybe Stone
        maybeOverStone =
            stoneList stones |> List.Extra.find isOverOwn
    in
    case maybeOverStone of
        Just stone ->
            DragStone stone

        Nothing ->
            DragSpot coords


desiredCenter : Spot -> Spot -> Spot
desiredCenter from to =
    let
        dragDistance =
            distance to from
    in
    if dragDistance < adjacentDistance then
        to

    else
        let
            ratio : Float
            ratio =
                adjacentDistance / dragDistance

            dx =
                toFloat (to.x - from.x)

            dy =
                toFloat (to.y - from.y)
        in
        { x = from.x + floor (ratio * dx)
        , y = from.y + floor (ratio * dy)
        }


findDragged : Stones -> Color -> Stone -> Spot -> Stone
findDragged stones onMove draggedFrom draggedTo =
    findDraggedSpot (List.map .spot (stoneList stones)) draggedFrom.spot draggedTo
        |> Maybe.withDefault (desiredCenter draggedFrom.spot draggedTo)
        |> createStone onMove


{-| Geometry only: analysis circles obstruct placement just like real stones.
Unlike a stone, a circle has no color or move-legality constraints.
-}
findDraggedSpot : List Spot -> Spot -> Spot -> Maybe Spot
findDraggedSpot spotList from draggedTo =
    let
        desiredSpot : Spot
        desiredSpot =
            desiredCenter from draggedTo

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
                    && isWithinBoard candidate
                    && adjacent candidate from
                    -- the not overlapsAny here is the hot path
                    && (not <| overlapsAny candidate spotList)
            then
                Just ( candidate, candidateDistance )

            else
                current
    in
    List.foldl findNearest Nothing Placement.candidateOffsets
        |> Maybe.map Tuple.first
