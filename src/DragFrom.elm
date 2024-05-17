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
    let
        desiredSpot : Spot
        desiredSpot =
            desiredCentre draggedFrom.spot draggedTo

        spotList : List Spot
        spotList =
            stoneList stones |> List.map .spot

        findNearest : Spot -> ( Spot, Float ) -> ( Spot, Float )
        findNearest zeroCandidate ( currentSpot, currentDistance ) =
            let
                candidate =
                    Liberties.normalize desiredSpot zeroCandidate

                candidateDistance =
                    distance candidate draggedTo
            in
            if
                (candidateDistance < currentDistance)
                    && isWithinBoard candidate
                    && adjacent candidate draggedFrom.spot
                    -- the not overlapsAny here is the hot path
                    && (not <| overlapsAny candidate spotList)
            then
                ( candidate, candidateDistance )

            else
                ( currentSpot, currentDistance )
    in
    createStone onMove <|
        Tuple.first <|
            List.foldl findNearest
                ( desiredSpot, 1000000 )
                Liberties.zeroNpCandidates
