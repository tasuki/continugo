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



-- Coords


type alias Coords =
    { x : Int, y : Int }


distance : Coords -> Coords -> Float
distance c1 c2 =
    sqrt <| toFloat ((c1.x - c2.x) ^ 2 + (c1.y - c2.y) ^ 2)


overlaps : Coords -> List Coords -> Bool
overlaps coords =
    List.any (\c -> distance coords c < diameter)



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
    , coords : Coords
    , nearby : List Coords
    , adjacent : List Coords
    , liberties : List Coords
    }


createStone : Player -> Coords -> Stone
createStone player coords =
    { player = player
    , coords = coords
    , nearby = []
    , adjacent = []
    , liberties = []
    }


isWithinBoard : Stone -> Bool
isWithinBoard { coords } =
    let
        isWithin : Int -> Bool
        isWithin coord =
            coord > stoneR && coord < coordRange - stoneR
    in
    isWithin coords.x && isWithin coords.y


stoneDistance : Stone -> Stone -> Float
stoneDistance s1 s2 =
    distance s1.coords s2.coords



-- Stones dictionary


type alias Stones =
    Dict ( Int, Int ) Stone


stoneKey : Stone -> ( Int, Int )
stoneKey { coords } =
    ( coords.x, coords.y )


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
                | nearby = List.map .coords nearbyStones
                , adjacent = List.map .coords adjacentStones
            }

        addNearbyStone : Stone -> Stones -> Stones
        addNearbyStone s =
            Dict.update (stoneKey s)
                (Maybe.map (\orig -> { orig | nearby = stone.coords :: orig.nearby }))

        addNearby : Stones -> Stones
        addNearby sts =
            List.foldl addNearbyStone sts nearbyStones

        addAdjacentStone : Stone -> Stones -> Stones
        addAdjacentStone s =
            Dict.update (stoneKey s)
                (Maybe.map (\orig -> { orig | adjacent = stone.coords :: orig.adjacent }))

        addAdjacent : Stones -> Stones
        addAdjacent sts =
            List.foldl addAdjacentStone sts adjacentStones

        overlapsNearby : Bool
        overlapsNearby =
            overlaps stone.coords (List.map .coords nearbyStones)
    in
    if isWithinBoard stone && (not <| overlapsNearby) then
        Dict.insert (stoneKey stone) stoneWithExtras stones
            |> addNearby
            |> addAdjacent
            |> Just

    else
        Nothing
