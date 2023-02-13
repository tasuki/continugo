module Liberties exposing
    ( findLiberties
    , findNearestPlayable
    , uniqueLiberties
    )

import Dict exposing (Dict)
import Go exposing (..)
import Set exposing (Set)



-- settings and hardcoded things


zero : Spot
zero =
    Spot 0 0


normalize spot zeroSuspect =
    Spot (spot.x + zeroSuspect.x) (spot.y + zeroSuspect.y)



-- filtering and sorting liberties


peel : List Spot -> List Spot -> Set ( Int, Int ) -> List Spot
peel acc liberties set =
    let
        allNeighbors : Spot -> Bool
        allNeighbors l =
            List.all (\i -> Set.member i set)
                [ ( l.x, l.y - 1 )
                , ( l.x, l.y + 1 )
                , ( l.x - 1, l.y )
                , ( l.x + 1, l.y )
                ]

        ( inside, outside ) =
            List.partition allNeighbors liberties

        newSet =
            Set.diff set (Set.fromList <| List.map (\l -> ( l.x, l.y )) outside)

        newAcc =
            outside ++ acc
    in
    if List.length inside == 0 then
        newAcc

    else
        peel newAcc inside newSet


sortByMostOverlap : List Spot -> List Spot
sortByMostOverlap liberties =
    -- we want to find the spots that take away as much liberty as possible
    let
        libertiesSet : Set ( Int, Int )
        libertiesSet =
            Set.fromList <| List.map (\l -> ( l.x, l.y )) liberties
    in
    peel [] liberties libertiesSet


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
uniqueLiberties liberties =
    sortByMostOverlap liberties |> takeNonOverlapping []



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
