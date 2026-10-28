module Placement exposing (candidateOffsets, nearbySpot, nearestFreeSpot, playNearby)

{-| Pointer placement conveniences, not game rules. Resolve an intended coordinate
before asking the rules to validate the exact placement.
-}

import Dict
import Rules.Board exposing (stoneRadius)
import Rules.Play as Play
import Rules.Position exposing (..)


candidateOffsets : List Spot
candidateOffsets =
    let
        range =
            List.range -stoneRadius stoneRadius
    in
    List.concatMap (\x -> List.map (Spot x) range) range
        |> List.filter (\s -> distance s (Spot 0 0) < toFloat stoneRadius)
        |> List.sortBy (distance (Spot 0 0))


firstFreeSpot : Spot -> List Spot -> List Spot -> Maybe Spot
firstFreeSpot origin nearbySpots offsets =
    case offsets of
        [] ->
            Nothing

        offset :: tail ->
            let
                candidate =
                    translate origin offset
            in
            if isWithinBoard candidate && not (overlapsAny candidate nearbySpots) then
                Just candidate

            else
                firstFreeSpot origin nearbySpots tail


nearestFreeSpot : Spot -> List Spot -> Maybe Spot
nearestFreeSpot origin nearbySpots =
    firstFreeSpot origin nearbySpots candidateOffsets


nearbySpot : Stones -> Spot -> Maybe Spot
nearbySpot stones spot =
    nearestFreeSpot spot (List.map .spot (nearbyStones stones spot))


playNearby : Color -> Stones -> Spot -> Maybe ( Stones, Stone )
playNearby color stones spot =
    nearbySpot stones spot
        |> Maybe.andThen
            (\target ->
                Play.playIfLegal stones (createStone color target)
                    |> Maybe.andThen
                        (\result ->
                            Dict.get ( target.x, target.y ) result
                                |> Maybe.map (Tuple.pair result)
                        )
            )
