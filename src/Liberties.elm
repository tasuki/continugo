module Liberties exposing
    ( addPotential
    , findLiberties
    , findNearestPlayable
    , hasLiberties
    , potentialLiberties
    , recalculate
    , uniqueLiberties
    )

import Dict
import Go exposing (..)



-- settings and hardcoded things


zero : Spot
zero =
    Spot 0 0


normalize spot zeroSuspect =
    Spot (spot.x + zeroSuspect.x) (spot.y + zeroSuspect.y)



-- filtering and sorting liberties


takeNonOverlapping : List Spot -> List Spot -> List Spot
takeNonOverlapping acc queue =
    -- take unique liberty spots
    case queue of
        [] ->
            acc

        head :: tail ->
            if overlapsAny head acc then
                takeNonOverlapping acc tail

            else
                takeNonOverlapping (head :: acc) tail


uniqueLiberties : List Spot -> List Spot
uniqueLiberties =
    takeNonOverlapping []



-- finding liberties


zeroLibCandidates : List Spot
zeroLibCandidates =
    -- precompute ring of candidates around zero: 18k spots
    let
        range =
            List.range -(ceiling adjacentDistance) (ceiling adjacentDistance)
    in
    List.concatMap (\x -> List.map (Spot x) range) range
        |> List.filter (\s -> not <| overlapsAny s [ zero ])
        |> List.filter (adjacent zero)
        |> List.sortBy (distance zero)


potentialLiberties : Spot -> List Spot
potentialLiberties spot =
    List.map (normalize spot) zeroLibCandidates
        |> List.filter isWithinBoard


libertiesToCheck : Stone -> List Spot
libertiesToCheck stone =
    case stone.potentialLiberties of
        Just potentialLibs ->
            potentialLibs

        Nothing ->
            potentialLiberties stone.spot


isLiberty : List Spot -> Spot -> Bool
isLiberty nearbySpots suspect =
    not <| overlapsAny suspect nearbySpots


findLiberties : Stone -> List Spot -> List Spot
findLiberties stone nearbySpots =
    libertiesToCheck stone
        |> List.filter (isLiberty nearbySpots)
        |> uniqueLiberties


hasLiberties : Stone -> List Spot -> Bool
hasLiberties stone nearbySpots =
    let
        isLibertyWithinBoard : Spot -> Bool
        isLibertyWithinBoard spot =
            isWithinBoard spot && isLiberty nearbySpots spot
    in
    case stone.potentialLiberties of
        Just potentialLibs ->
            List.any (isLiberty nearbySpots) potentialLibs

        Nothing ->
            List.any (normalize stone.spot >> isLibertyWithinBoard) zeroLibCandidates



-- nearest playable


zeroNpCandidates : List Spot
zeroNpCandidates =
    -- precompute circle of candidates around zero: 4.5k spots
    let
        range =
            List.range -stoneR stoneR
    in
    List.concatMap (\x -> List.map (Spot x) range) range
        |> List.filter (\s -> distance s zero < stoneR)
        |> List.sortBy (distance zero)


nearestPlayable : Spot -> List Spot -> List Spot -> Maybe Spot
nearestPlayable origin nearbySpots zeroSuspects =
    case zeroSuspects of
        [] ->
            Nothing

        zeroSuspect :: tail ->
            let
                suspect =
                    normalize origin zeroSuspect
            in
            if isWithinBoard suspect && not (overlapsAny suspect nearbySpots) then
                Just suspect

            else
                nearestPlayable origin nearbySpots tail


findNearestPlayable : Spot -> List Spot -> Maybe Spot
findNearestPlayable origin nearbySpots =
    nearestPlayable origin nearbySpots zeroNpCandidates



-- recalc


addPotential : Stone -> Stones -> Stones
addPotential stone =
    let
        addPotentialLibs : Stone -> Stone
        addPotentialLibs s =
            { s | potentialLiberties = Just <| potentialLiberties s.spot }
    in
    Dict.update (stoneKey stone) (Maybe.map addPotentialLibs)


recalculate : List Spot -> Stones -> Stones
recalculate spots stones =
    let
        helper : Spot -> Stones -> Stones
        helper spot acc =
            acc
    in
    List.foldl helper stones spots
