module Go exposing (..)

import Dict exposing (Dict)



-- Config and constants


coordRange =
    -- number of coordinates on actual board
    -- sorry this is actually still discrete
    1000


stoneR =
    -- stone radius
    -- 9x9: 55.6, 13x13: 38.5, 19x19: 26.3
    38


diameter =
    -- stone diameter
    2 * stoneR


adjacentDistance =
    -- stones which are considered connected
    1.4142


nearbyDistance =
    -- stones that could affect this stone's liberty status
    2 + adjacentDistance



-- Spot


type alias Spot =
    -- a spot has two coords
    { x : Int, y : Int }


distance : Spot -> Spot -> Float
distance s1 s2 =
    sqrt <| toFloat ((s1.x - s2.x) ^ 2 + (s1.y - s2.y) ^ 2)


overlaps : Spot -> List Spot -> Bool
overlaps spots =
    List.any (\s -> distance spots s < diameter)


boardMin : Int
boardMin =
    stoneR


boardMax : Int
boardMax =
    coordRange - stoneR


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
    , liberties : List Spot
    }


createStone : Player -> Spot -> Stone
createStone player spot =
    { player = player
    , spot = spot
    , nearby = []
    , adjacent = []
    , liberties = []
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
        |> List.filter (\s -> distance spot s.spot < diameter * nearbyDistance)


enhanceInfo : Stones -> Stone -> Stone
enhanceInfo stones stone =
    let
        nearby =
            nearbyStones stones stone.spot

        adjacentStones : List Stone
        adjacentStones =
            nearby
                |> List.filter (\s -> s.player == stone.player)
                |> List.filter (\s -> stoneDistance stone s < diameter * adjacentDistance)
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
    Dict.toList stones |> List.map (\( _, s ) -> s)


addStones : (Stone -> Stone) -> List Spot -> Stones -> Stones
addStones addFun spots sts =
    List.foldl (\s -> Dict.update ( s.x, s.y ) (Maybe.map addFun)) sts spots
