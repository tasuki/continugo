module Liberties exposing
    ( findLiberties
    , findNearestPlayable
    , hasLiberties
    , uniqueLiberties
    )

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


isLiberty : List Spot -> Spot -> Bool
isLiberty nearbySpots suspect =
    let
        overlapsNearby : Bool
        overlapsNearby =
            overlapsAny suspect nearbySpots
    in
    isWithinBoard suspect && not overlapsNearby


findLiberties : Stone -> List Spot -> List Spot
findLiberties { spot } nearbySpots =
    List.map (normalize spot) zeroLibCandidates
        |> List.filter (isLiberty nearbySpots)
        |> uniqueLiberties


hasLiberties : Stone -> List Spot -> Bool
hasLiberties { spot } nearbySpots =
    let
        isLib : Spot -> Bool
        isLib zeroLC =
            isLiberty nearbySpots (normalize spot zeroLC)
    in
    List.any isLib zeroLibCandidates



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
