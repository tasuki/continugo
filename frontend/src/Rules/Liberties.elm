module Rules.Liberties exposing (Span, candidateOffsets, candidateOffsetsOn, hasLiberties, hasLibertiesOn, openRowsOn, sampleLiberties, sampleLibertiesOn)

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

            blocked =
                blockedIntervals search y nearbySpots

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


blockedIntervals : Search -> Int -> List Spot -> List ( Int, Int )
blockedIntervals search y spots =
    spots
        |> List.filterMap
            (\other ->
                let
                    halfWidth =
                        rowHalfWidth search.overlapRows (y - other.y)
                in
                if halfWidth < 0 then
                    Nothing

                else
                    Just ( other.x - halfWidth, other.x + halfWidth )
            )
        |> List.sortBy Tuple.first


type alias Span =
    { y : Int, left : Int, right : Int }


{-| All physically free integer centres, optionally restricted to a group's
adjacency discs. Each span includes both endpoints. No suicide/ko checking.
-}
openRowsOn : Board -> List Spot -> Maybe (List Spot) -> List Span
openRowsOn board spots group =
    let
        search =
            searchOn board

        low =
            Board.radius board

        high =
            Board.coordRange - low

        row y =
            let
                reachable =
                    case group of
                        Nothing ->
                            [ ( low, high ) ]

                        Just members ->
                            members
                                |> List.filterMap
                                    (\member ->
                                        let
                                            width =
                                                rowHalfWidth search.adjacencyRows (y - member.y)
                                        in
                                        if width < 0 then
                                            Nothing

                                        else
                                            Just ( max low (member.x - width), min high (member.x + width) )
                                    )
                                |> List.sortBy Tuple.first
                                |> mergeIntervals

                blocked =
                    blockedIntervals search y spots
            in
            List.concatMap (\( left, right ) -> openIntervals left right blocked) reachable
                |> List.map (\( left, right ) -> Span y left right)

        firstY =
            group
                |> Maybe.map (List.map (.y >> (\y -> y - search.maxRowOffset)) >> List.minimum >> Maybe.withDefault high >> max low)
                |> Maybe.withDefault low

        lastY =
            group
                |> Maybe.map (List.map (.y >> (\y -> y + search.maxRowOffset)) >> List.maximum >> Maybe.withDefault low >> min high)
                |> Maybe.withDefault high
    in
    List.range firstY lastY |> List.concatMap row


mergeIntervals : List ( Int, Int ) -> List ( Int, Int )
mergeIntervals intervals =
    case intervals of
        ( a, b ) :: ( c, d ) :: rest ->
            if c <= b + 1 then
                mergeIntervals (( a, max b d ) :: rest)

            else
                ( a, b ) :: mergeIntervals (( c, d ) :: rest)

        _ ->
            intervals


openIntervals : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
openIntervals left right blocked =
    if left > right then
        []

    else
        case blocked of
            [] ->
                [ ( left, right ) ]

            ( start, end ) :: rest ->
                if start > right then
                    [ ( left, right ) ]

                else if start > left then
                    ( left, start - 1 ) :: openIntervals (max left (end + 1)) right rest

                else
                    openIntervals (max left (end + 1)) right rest


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
