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


isWithinBoard : Stone -> Bool
isWithinBoard { spot } =
    let
        isWithin : Int -> Bool
        isWithin coord =
            coord > stoneR && coord < coordRange - stoneR
    in
    isWithin spot.x && isWithin spot.y


stoneDistance : Stone -> Stone -> Float
stoneDistance s1 s2 =
    distance s1.spot s2.spot



-- Stones dictionary


type alias Stones =
    Dict ( Int, Int ) Stone


stoneKey : Stone -> ( Int, Int )
stoneKey { spot } =
    ( spot.x, spot.y )


stoneList : Stones -> List Stone
stoneList stones =
    Dict.toList stones |> List.map (\( _, s ) -> s)



-- Play logic


playIfLegal : Stone -> Stones -> Maybe Stones
playIfLegal stone stones =
    let
        nearbyStones : List Stone
        nearbyStones =
            stoneList stones
                |> List.filter (\s -> stoneDistance stone s < diameter * nearbyDistance)

        adjacentStones : List Stone
        adjacentStones =
            nearbyStones
                |> List.filter (\s -> s.player == stone.player)
                |> List.filter (\s -> stoneDistance stone s < diameter * adjacentDistance)

        stoneWithExtras =
            { stone
                | nearby = List.map .spot nearbyStones
                , adjacent = List.map .spot adjacentStones
            }

        addNearby : Stone -> Stone
        addNearby orig =
            { orig | nearby = stone.spot :: orig.nearby }

        addAdjacent : Stone -> Stone
        addAdjacent orig =
            { orig | adjacent = stone.spot :: orig.adjacent }

        addStones : (Stone -> Stone) -> List Stone -> Stones -> Stones
        addStones addFun stonesList sts =
            List.foldl (\s -> Dict.update (stoneKey s) (Maybe.map addFun)) sts stonesList

        overlapsNearby : Bool
        overlapsNearby =
            overlaps stone.spot (List.map .spot nearbyStones)
    in
    if isWithinBoard stone && (not <| overlapsNearby) then
        Dict.insert (stoneKey stone) stoneWithExtras stones
            |> addStones addNearby nearbyStones
            |> addStones addAdjacent adjacentStones
            |> Just

    else
        Nothing
