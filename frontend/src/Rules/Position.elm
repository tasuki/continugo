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


{-| For squared distance q and diameter d, nearby means q < (6 + 4√2)d².
Subtract 6d², then square only a nonnegative remainder.
-}
isNearby : Spot -> Spot -> Bool
isNearby a b =
    let
        diameterSquared =
            diameter * diameter

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
isWithinBoard spot =
    let
        isWithin : Int -> Bool
        isWithin coord =
            coord >= boardMin && coord <= boardMax
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
nearbyStones stones spot =
    stoneList stones
        |> List.filter (\s -> isNearby spot s.spot)


withNeighbors : Stones -> Stone -> Stone
withNeighbors stones stone =
    let
        nearby =
            nearbyStones stones stone.spot

        adjacentStones : List Stone
        adjacentStones =
            nearby
                |> List.filter (\s -> s.color == stone.color)
                |> List.filter (\s -> adjacent stone.spot s.spot)
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
