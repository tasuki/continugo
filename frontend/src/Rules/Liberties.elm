module Rules.Liberties exposing (candidateOffsets, hasLiberties, sampleLiberties)

import Array exposing (Array)
import Rules.Position exposing (..)



-- settings and hardcoded things


zero : Spot
zero =
    Spot 0 0



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


nonOverlappingLiberties : List Spot -> List Spot
nonOverlappingLiberties =
    takeNonOverlapping []



-- finding liberties


{-| Walk each integer disc boundary once, storing its half-width at each row.
widthBound must be at least the radius. The squared radii subtract one because
adjacency and overlap are strict.
-}
discRows : Int -> Int -> Array Int
discRows widthBound squaredRadius =
    let
        build y x rows =
            if y * y > squaredRadius then
                Array.fromList (List.reverse rows)

            else if x * x + y * y > squaredRadius then
                build y (x - 1) rows

            else
                build (y + 1) x (x :: rows)
    in
    build 0 widthBound []


adjacencyRows : Array Int
adjacencyRows =
    discRows (2 * diameter) (2 * diameter * diameter - 1)


overlapRows : Array Int
overlapRows =
    discRows diameter (diameter * diameter - 1)


maxRowOffset : Int
maxRowOffset =
    Array.length adjacencyRows - 1


{-| -1 denotes an empty row outside the disc.
-}
rowHalfWidth : Array Int -> Int -> Int
rowHalfWidth rows offset =
    Array.get (abs offset) rows |> Maybe.withDefault -1


candidateOffsets : List Spot
candidateOffsets =
    -- precompute ring of candidates around zero: 18k spots
    let
        range =
            List.range -maxRowOffset maxRowOffset
    in
    List.concatMap (\x -> List.map (Spot x) range) range
        |> List.filter (\s -> not <| overlaps s zero)
        |> List.filter (adjacent zero)
        |> List.sortBy (distanceSquared zero)


isLiberty : List Spot -> Spot -> Bool
isLiberty nearbySpots suspect =
    let
        overlapsNearby : Bool
        overlapsNearby =
            overlapsAny suspect nearbySpots
    in
    isWithinBoard suspect && not overlapsNearby


{-| A greedy, non-overlapping sample, not the complete set of liberties.
-}
sampleLiberties : Stone -> List Spot -> List Spot
sampleLiberties { spot } nearbySpots =
    List.map (translate spot) candidateOffsets
        |> List.filter (isLiberty nearbySpots)
        |> nonOverlappingLiberties


libertyProbes : List Spot
libertyProbes =
    [ Spot -diameter 0, Spot 0 -diameter, Spot 0 diameter, Spot diameter 0 ]


hasLiberties : Stone -> List Spot -> Bool
hasLiberties { spot } nearbySpots =
    -- Try the four closest candidates first: open groups usually stop here.
    -- For enclosed groups, search whole integer intervals rather than testing
    -- all ~18,000 points in the adjacency ring individually.
    List.any (translate spot >> isLiberty nearbySpots) libertyProbes
        || hasLibertyInRows spot
            nearbySpots
            (max boardMin (spot.y - maxRowOffset))
            (min boardMax (spot.y + maxRowOffset))


hasLibertyInRows : Spot -> List Spot -> Int -> Int -> Bool
hasLibertyInRows spot nearbySpots y lastY =
    if y > lastY then
        False

    else
        let
            dy =
                y - spot.y

            outer =
                rowHalfWidth adjacencyRows dy

            inner =
                -- The first integer outside the overlap disc permits exact touching.
                rowHalfWidth overlapRows dy + 1

            blockedInterval other =
                let
                    halfWidth =
                        rowHalfWidth overlapRows (y - other.y)
                in
                if halfWidth < 0 then
                    Nothing

                else
                    Just ( other.x - halfWidth, other.x + halfWidth )

            blocked =
                List.filterMap blockedInterval nearbySpots |> List.sortBy Tuple.first

            openInterval left right =
                intervalHasGap (max boardMin left) (min boardMax right) blocked
        in
        (outer
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
