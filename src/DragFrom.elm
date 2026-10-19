module DragFrom exposing (..)

import Go exposing (..)
import Liberties
import List.Extra


type DragFrom
    = DragNone
    | DragSpot Spot
    | DragStone Stone


getDragFrom : Stones -> Player -> Spot -> DragFrom
getDragFrom stones onMove coords =
    let
        isOverOwn : Stone -> Bool
        isOverOwn s =
            s.player == onMove && isWithinStone s.spot coords

        maybeOverStone : Maybe Stone
        maybeOverStone =
            stoneList stones |> List.Extra.find isOverOwn
    in
    case maybeOverStone of
        Just stone ->
            DragStone stone

        Nothing ->
            DragSpot coords


desiredCentre : Spot -> Spot -> Spot
desiredCentre from to =
    let
        dstnc =
            distance to from
    in
    if dstnc < adjacentDistance then
        to

    else
        let
            ratio : Float
            ratio =
                adjacentDistance / dstnc

            dx =
                toFloat (to.x - from.x)

            dy =
                toFloat (to.y - from.y)
        in
        { x = from.x + floor (ratio * dx)
        , y = from.y + floor (ratio * dy)
        }


findDragged : Stones -> Player -> Stone -> Spot -> Stone
findDragged stones onMove draggedFrom draggedTo =
    findDraggedSpot (List.map .spot (stoneList stones)) draggedFrom.spot draggedTo
        |> Maybe.withDefault (desiredCentre draggedFrom.spot draggedTo)
        |> createStone onMove


{-| Geometry only: analysis circles obstruct placement just like real stones.
Unlike a stone, a circle has no player or move-legality constraints.
-}
findDraggedSpot : List Spot -> Spot -> Spot -> Maybe Spot
findDraggedSpot spotList from draggedTo =
    let
        desiredSpot : Spot
        desiredSpot =
            desiredCentre from draggedTo

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
                    Liberties.normalize desiredSpot zeroCandidate

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
    List.foldl findNearest Nothing Liberties.zeroNpCandidates
        |> Maybe.map Tuple.first
