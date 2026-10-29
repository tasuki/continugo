module Rules.Position exposing (..)

{-| Unqualified geometry helpers use the default board; `On` variants take a board.
Use the same board for all operations on a Stones value: neighbour caches depend
on its geometry. Rebuild from the record when changing boards.
-}

import Dict exposing (Dict)
import Rules.Board as Board exposing (Board, coordRange, stoneRadius)



-- Config and constants


diameter : Int
diameter =
    Board.diameter Board.default



-- Spot


type alias Spot =
    -- a spot has two coords
    { x : Int, y : Int }


translate : Spot -> Spot -> Spot
translate spot offset =
    Spot (spot.x + offset.x) (spot.y + offset.y)


distanceSquared : Spot -> Spot -> Int
distanceSquared s1 s2 =
    let
        dx =
            s1.x - s2.x

        dy =
            s1.y - s2.y
    in
    dx * dx + dy * dy


isWithinStone : Spot -> Spot -> Bool
isWithinStone =
    isWithinStoneOn Board.default


isWithinStoneOn : Board -> Spot -> Spot -> Bool
isWithinStoneOn board s1 s2 =
    let
        r =
            Board.radius board
    in
    distanceSquared s1 s2 < r * r


overlaps : Spot -> Spot -> Bool
overlaps =
    overlapsOn Board.default


overlapsOn : Board -> Spot -> Spot -> Bool
overlapsOn board s1 s2 =
    let
        d =
            Board.diameter board
    in
    distanceSquared s1 s2 < d * d


overlapsAny : Spot -> List Spot -> Bool
overlapsAny =
    overlapsAnyOn Board.default


overlapsAnyOn : Board -> Spot -> List Spot -> Bool
overlapsAnyOn board spot =
    List.any (overlapsOn board spot)


adjacent : Spot -> Spot -> Bool
adjacent =
    adjacentOn Board.default


adjacentOn : Board -> Spot -> Spot -> Bool
adjacentOn board s1 s2 =
    let
        d =
            Board.diameter board
    in
    distanceSquared s1 s2 < 2 * d * d


{-| For squared distance q and diameter d, nearby means q < (6 + 4√2)d².
Subtract 6d², then square only a nonnegative remainder.
-}
isNearby : Spot -> Spot -> Bool
isNearby =
    isNearbyOn Board.default


isNearbyOn : Board -> Spot -> Spot -> Bool
isNearbyOn board a b =
    let
        d =
            Board.diameter board

        diameterSquared =
            d * d

        excess =
            distanceSquared a b - 6 * diameterSquared
    in
    excess < 0 || excess * excess < 32 * diameterSquared * diameterSquared


boardMin : Int
boardMin =
    stoneRadius


boardMax : Int
boardMax =
    coordRange - stoneRadius


isWithinBoard : Spot -> Bool
isWithinBoard =
    isWithinBoardOn Board.default


isWithinBoardOn : Board -> Spot -> Bool
isWithinBoardOn board spot =
    let
        r =
            Board.radius board

        isWithin : Int -> Bool
        isWithin coord =
            coord >= r && coord <= coordRange - r
    in
    isWithin spot.x && isWithin spot.y



-- Colors


type Color
    = Black
    | White


otherColor : Color -> Color
otherColor color =
    case color of
        White ->
            Black

        Black ->
            White



-- Stone


type alias Stone =
    { color : Color
    , spot : Spot
    , nearby : List Spot
    , adjacent : List Spot
    }


createStone : Color -> Spot -> Stone
createStone color spot =
    { color = color
    , spot = spot
    , nearby = []
    , adjacent = []
    }


addNearby : Stone -> Stone -> Stone
addNearby toAdd orig =
    { orig | nearby = toAdd.spot :: orig.nearby }


removeNearby : Stone -> Stone -> Stone
removeNearby toRemove orig =
    { orig | nearby = List.filter (\s -> s /= toRemove.spot) orig.nearby }


addAdjacent : Stone -> Stone -> Stone
addAdjacent toAdd orig =
    { orig | adjacent = toAdd.spot :: orig.adjacent }


removeAdjacent : Stone -> Stone -> Stone
removeAdjacent toRemove orig =
    { orig | adjacent = List.filter (\s -> s /= toRemove.spot) orig.adjacent }


nearbyStones : Stones -> Spot -> List Stone
nearbyStones =
    nearbyStonesOn Board.default


nearbyStonesOn : Board -> Stones -> Spot -> List Stone
nearbyStonesOn board stones spot =
    stoneList stones
        |> List.filter (\s -> isNearbyOn board spot s.spot)


withNeighbors : Stones -> Stone -> Stone
withNeighbors =
    withNeighborsOn Board.default


withNeighborsOn : Board -> Stones -> Stone -> Stone
withNeighborsOn board stones stone =
    let
        nearby =
            nearbyStonesOn board stones stone.spot

        adjacentStones : List Stone
        adjacentStones =
            nearby
                |> List.filter (\s -> s.color == stone.color)
                |> List.filter (\s -> adjacentOn board stone.spot s.spot)
    in
    { stone
        | nearby = List.map .spot nearby
        , adjacent = List.map .spot adjacentStones
    }



-- Stones dictionary


type alias Stones =
    Dict ( Int, Int ) Stone


stoneKey : Stone -> ( Int, Int )
stoneKey { spot } =
    ( spot.x, spot.y )


stoneList : Stones -> List Stone
stoneList stones =
    Dict.values stones


updateStones : (Stone -> Stone) -> List Spot -> Stones -> Stones
updateStones transform spots stones =
    List.foldl (\s -> Dict.update ( s.x, s.y ) (Maybe.map transform)) stones spots


removedStones : Stones -> Stones -> List Stone
removedStones old new =
    let
        maybeMissingStone : Stone -> Maybe Stone
        maybeMissingStone oldStone =
            case Dict.get (stoneKey oldStone) new of
                Just _ ->
                    Nothing

                Nothing ->
                    Just oldStone
    in
    List.filterMap maybeMissingStone (stoneList old)



-- Moves


type Move
    = Place Stone
    | Pass


type alias Play =
    { color : Color
    , move : Move
    }


createPlay : Color -> Spot -> Play
createPlay color spot =
    { color = color
    , move = Place <| createStone color spot
    }


createPass : Color -> Play
createPass color =
    { color = color
    , move = Pass
    }


stonesFromPlays : List Play -> List Stone
stonesFromPlays plays =
    let
        getMaybeStone play =
            case play.move of
                Place stone ->
                    Just stone

                _ ->
                    Nothing
    in
    List.filterMap getMaybeStone plays
