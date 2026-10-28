module Rules.Position exposing (..)

import Dict exposing (Dict)
import Rules.Board exposing (coordRange, stoneRadius)



-- Config and constants


diameter : Int
diameter =
    -- stone diameter
    2 * stoneRadius


adjacentDistance : Float
adjacentDistance =
    -- stones which are considered connected
    sqrt 2 * toFloat diameter


nearbyDistance : Float
nearbyDistance =
    -- stones that could affect this stone's liberty status
    2 * toFloat diameter + adjacentDistance



-- Spot


type alias Spot =
    -- a spot has two coords
    { x : Int, y : Int }


translate : Spot -> Spot -> Spot
translate spot offset =
    Spot (spot.x + offset.x) (spot.y + offset.y)


distance : Spot -> Spot -> Float
distance s1 s2 =
    sqrt (toFloat (distanceSquared s1 s2))


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
isWithinStone s1 s2 =
    distanceSquared s1 s2 < stoneRadius * stoneRadius


overlaps : Spot -> Spot -> Bool
overlaps s1 s2 =
    distanceSquared s1 s2 < diameter * diameter


overlapsAny : Spot -> List Spot -> Bool
overlapsAny spot =
    List.any (overlaps spot)


adjacent : Spot -> Spot -> Bool
adjacent s1 s2 =
    distanceSquared s1 s2 < 2 * diameter * diameter


boardMin : Int
boardMin =
    stoneRadius


boardMax : Int
boardMax =
    coordRange - stoneRadius


isWithinBoard : Spot -> Bool
isWithinBoard spot =
    let
        isWithin : Int -> Bool
        isWithin coord =
            coord >= boardMin && coord <= boardMax
    in
    isWithin spot.x && isWithin spot.y



-- Players


type Player
    = Black
    | White


otherPlayer : Player -> Player
otherPlayer p =
    case p of
        White ->
            Black

        Black ->
            White



-- Stone


type alias Stone =
    { player : Player
    , spot : Spot
    , nearby : List Spot
    , adjacent : List Spot
    }


createStone : Player -> Spot -> Stone
createStone player spot =
    { player = player
    , spot = spot
    , nearby = []
    , adjacent = []
    }


stoneDistance : Stone -> Stone -> Float
stoneDistance s1 s2 =
    distance s1.spot s2.spot


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
nearbyStones stones spot =
    stoneList stones
        |> List.filter (\s -> distance spot s.spot < nearbyDistance)


enhanceInfo : Stones -> Stone -> Stone
enhanceInfo stones stone =
    let
        nearby =
            nearbyStones stones stone.spot

        adjacentStones : List Stone
        adjacentStones =
            nearby
                |> List.filter (\s -> s.player == stone.player)
                |> List.filter (\s -> stoneDistance stone s < adjacentDistance)
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


addStones : (Stone -> Stone) -> List Spot -> Stones -> Stones
addStones addFun spots sts =
    List.foldl (\s -> Dict.update ( s.x, s.y ) (Maybe.map addFun)) sts spots


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
    { player : Player
    , move : Move
    }


createPlay : Player -> Spot -> Play
createPlay player spot =
    { player = player
    , move = Place <| createStone player spot
    }


createPass : Player -> Play
createPass player =
    { player = player
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
