module Placement exposing (candidateOffsets, nearbySpot, nearestFreeSpot, playNearby)

{-| Pointer placement conveniences, not game rules. Resolve an intended coordinate
before asking the rules to validate the exact placement.
-}

import Dict
import Rules.Board as Board exposing (Board)
import Rules.Play as Play
import Rules.Position exposing (..)


candidateOffsets : Board -> List Spot
candidateOffsets board =
    case Dict.get (Board.size board) cachedOffsets of
        Just offsets ->
            offsets

        Nothing ->
            buildOffsets board


cachedOffsets : Dict.Dict Int (List Spot)
cachedOffsets =
    [ 9, 13, 19 ]
        |> List.filterMap Board.fromSize
        |> List.map (\board -> ( Board.size board, buildOffsets board ))
        |> Dict.fromList


buildOffsets : Board -> List Spot
buildOffsets board =
    let
        stoneRadius =
            Board.radius board

        range =
            List.range -stoneRadius stoneRadius
    in
    List.concatMap (\x -> List.map (Spot x) range) range
        |> List.filter (\s -> distanceSquared s (Spot 0 0) < stoneRadius * stoneRadius)
        |> List.sortBy (distanceSquared (Spot 0 0))


firstFreeSpot : Board -> Spot -> List Spot -> List Spot -> Maybe Spot
firstFreeSpot board origin nearbySpots offsets =
    case offsets of
        [] ->
            Nothing

        offset :: tail ->
            let
                candidate =
                    translate origin offset
            in
            if isWithinBoardOn board candidate && not (overlapsAnyOn board candidate nearbySpots) then
                Just candidate

            else
                firstFreeSpot board origin nearbySpots tail


nearestFreeSpot : Board -> Spot -> List Spot -> Maybe Spot
nearestFreeSpot board origin nearbySpots =
    firstFreeSpot board origin nearbySpots (candidateOffsets board)


nearbySpot : Board -> Stones -> Spot -> Maybe Spot
nearbySpot board stones spot =
    nearestFreeSpot board spot (List.map .spot (nearbyStonesOn board stones spot))


playNearby : Board -> Color -> Stones -> Spot -> Maybe ( Stones, Stone )
playNearby board color stones spot =
    nearbySpot board stones spot
        |> Maybe.andThen
            (\target ->
                Play.playIfLegalOn board stones (createStone color target)
                    |> Maybe.andThen
                        (\result ->
                            Dict.get ( target.x, target.y ) result
                                |> Maybe.map (Tuple.pair result)
                        )
            )
