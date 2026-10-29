module Rules.Liberties exposing (candidateOffsets, candidateOffsetsOn, hasLiberties, hasLibertiesOn, sampleLiberties, sampleLibertiesOn)

import Array exposing (Array)
import Rules.Board as Board exposing (Board)
import Rules.Position exposing (..)


takeNonOverlapping : Board -> List Spot -> List Spot -> List Spot
takeNonOverlapping board acc queue =
    case queue of
        [] ->
            acc

        head :: tail ->
            if overlapsAnyOn board head acc then
                takeNonOverlapping board acc tail

            else
                takeNonOverlapping board (head :: acc) tail


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


type alias Search =
    { board : Board
    , adjacencyRows : Array Int
    , overlapRows : Array Int
    , maxRowOffset : Int
    }


buildSearch : Board -> Search
buildSearch board =
    let
        d =
            Board.diameter board

        rows =
            discRows (2 * d) (2 * d * d - 1)
    in
    { board = board
    , adjacencyRows = rows
    , overlapRows = discRows d (d * d - 1)
    , maxRowOffset = Array.length rows - 1
    }


defaultSearch : Search
defaultSearch =
    buildSearch Board.default


searchOn : Board -> Search
searchOn board =
    if board == Board.default then
        defaultSearch

    else
        buildSearch board


{-| -1 denotes an empty row outside the disc.
-}
rowHalfWidth : Array Int -> Int -> Int
rowHalfWidth rows offset =
    Array.get (abs offset) rows |> Maybe.withDefault -1


candidateOffsets : List Spot
candidateOffsets =
    candidateOffsetsOn Board.default


candidateOffsetsOn : Board -> List Spot
candidateOffsetsOn board =
    let
        maxOffset =
            (searchOn board).maxRowOffset

        range =
            List.range -maxOffset maxOffset

        zero =
            Spot 0 0
    in
    List.concatMap (\x -> List.map (Spot x) range) range
        |> List.filter (\s -> not <| overlapsOn board s zero)
        |> List.filter (adjacentOn board zero)
        |> List.sortBy (distanceSquared zero)


isLiberty : Board -> List Spot -> Spot -> Bool
isLiberty board nearbySpots suspect =
    isWithinBoardOn board suspect && not (overlapsAnyOn board suspect nearbySpots)


{-| A greedy, non-overlapping sample, not the complete set of liberties.
-}
sampleLiberties : Stone -> List Spot -> List Spot
sampleLiberties =
    sampleLibertiesOn Board.default


sampleLibertiesOn : Board -> Stone -> List Spot -> List Spot
sampleLibertiesOn board { spot } nearbySpots =
    let
        offsets =
            if board == Board.default then
                candidateOffsets

            else
                candidateOffsetsOn board
    in
    List.map (translate spot) offsets
        |> List.filter (isLiberty board nearbySpots)
        |> takeNonOverlapping board []


hasLiberties : Stone -> List Spot -> Bool
hasLiberties =
    hasLibertiesOn Board.default


hasLibertiesOn : Board -> Stone -> List Spot -> Bool
hasLibertiesOn board { spot } nearbySpots =
    let
        d =
            Board.diameter board

        r =
            Board.radius board

        search =
            searchOn board

        probes =
            [ Spot -d 0, Spot 0 -d, Spot 0 d, Spot d 0 ]
    in
    -- Try the four closest candidates before searching whole integer intervals.
    List.any (translate spot >> isLiberty board nearbySpots) probes
        || hasLibertyInRows search
            spot
            nearbySpots
            (max r (spot.y - search.maxRowOffset))
            (min (Board.coordRange - r) (spot.y + search.maxRowOffset))


hasLibertyInRows : Search -> Spot -> List Spot -> Int -> Int -> Bool
hasLibertyInRows search spot nearbySpots y lastY =
    if y > lastY then
        False

    else
        let
            r =
                Board.radius search.board

            dy =
                y - spot.y

            outer =
                rowHalfWidth search.adjacencyRows dy

            inner =
                -- The first integer outside the overlap disc permits exact touching.
                rowHalfWidth search.overlapRows dy + 1

            blockedInterval other =
                let
                    halfWidth =
                        rowHalfWidth search.overlapRows (y - other.y)
                in
                if halfWidth < 0 then
                    Nothing

                else
                    Just ( other.x - halfWidth, other.x + halfWidth )

            blocked =
                List.filterMap blockedInterval nearbySpots |> List.sortBy Tuple.first

            openInterval left right =
                intervalHasGap (max r left) (min (Board.coordRange - r) right) blocked
        in
        (outer
            >= 0
            && (openInterval (spot.x - outer) (spot.x - inner)
                    || openInterval (spot.x + inner) (spot.x + outer)
               )
        )
            || hasLibertyInRows search spot nearbySpots (y + 1) lastY


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
