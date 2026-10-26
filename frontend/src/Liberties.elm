module Liberties exposing (..)

import Board exposing (stoneRadius)
import Go exposing (..)



-- settings and hardcoded things


zero : Spot
zero =
    Spot 0 0


normalize : Spot -> Spot -> Spot
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
        |> List.filter (\s -> not <| overlaps s zero)
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


libertyProbes : List Spot
libertyProbes =
    [ Spot -diameter 0, Spot 0 -diameter, Spot 0 diameter, Spot diameter 0 ]


hasLiberties : Stone -> List Spot -> Bool
hasLiberties { spot } nearbySpots =
    -- Try the four closest candidates first: open groups usually stop here.
    -- For enclosed groups, search whole integer intervals rather than testing
    -- all ~18,000 points in the adjacency ring individually.
    List.any (normalize spot >> isLiberty nearbySpots) libertyProbes
        || hasLibertyInRows spot
            nearbySpots
            (max boardMin (spot.y - floor adjacentDistance))
            (min boardMax (spot.y + floor adjacentDistance))


hasLibertyInRows : Spot -> List Spot -> Int -> Int -> Bool
hasLibertyInRows spot nearbySpots y lastY =
    if y > lastY then
        False

    else
        let
            dy =
                y - spot.y

            -- Adjacency is strict; overlap is strict too. All coordinates are
            -- integers, so subtracting one preserves both boundary rules.
            outerSquared =
                2 * diameter * diameter - 1 - dy * dy

            innerSquared =
                diameter * diameter - dy * dy

            outer =
                floor (sqrt (toFloat outerSquared))

            inner =
                ceiling (sqrt (toFloat (max 0 innerSquared)))

            blockedInterval other =
                let
                    deltaY =
                        y - other.y

                    squared =
                        diameter * diameter - 1 - deltaY * deltaY
                in
                if squared < 0 then
                    Nothing

                else
                    let
                        halfWidth =
                            floor (sqrt (toFloat squared))
                    in
                    Just ( other.x - halfWidth, other.x + halfWidth )

            blocked =
                List.filterMap blockedInterval nearbySpots |> List.sortBy Tuple.first

            openInterval left right =
                intervalHasGap (max boardMin left) (min boardMax right) blocked
        in
        (outerSquared
            >= 0
            && (openInterval (spot.x - outer) (spot.x - inner)
                    || openInterval (spot.x + inner) (spot.x + outer)
               )
        )
            || hasLibertyInRows spot nearbySpots (y + 1) lastY


intervalHasGap : Int -> Int -> List ( Int, Int ) -> Bool
intervalHasGap left right blocked =
    if left > right then
        False

    else
        case blocked of
            [] ->
                True

            ( start, end ) :: rest ->
                if start > left then
                    True

                else
                    intervalHasGap (max left (end + 1)) right rest



-- nearest playable


zeroNpCandidates : List Spot
zeroNpCandidates =
    -- precompute circle of candidates around zero: 4.5k spots
    let
        range =
            List.range -stoneRadius stoneRadius
    in
    List.concatMap (\x -> List.map (Spot x) range) range
        |> List.filter (\s -> distance s zero < toFloat stoneRadius)
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
