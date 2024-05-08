module Go exposing (..)

import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy exposing (..)



-- Config and constants


coordRange =
    -- number of coordinates on actual board
    -- sorry this is actually still discrete
    1000


stoneR =
    -- stone radius: coordRange / boardSize / 2
    -- 9x9: 55.6, 13x13: 38.5, 19x19: 26.3
    38


diameter =
    -- stone diameter
    2 * stoneR


adjacentDistance =
    -- stones which are considered connected
    1.4142 * diameter


nearbyDistance =
    -- stones that could affect this stone's liberty status
    2 * diameter + adjacentDistance



-- Spot


type alias Spot =
    -- a spot has two coords
    { x : Int, y : Int }


distance : Spot -> Spot -> Float
distance s1 s2 =
    sqrt <| toFloat ((s1.x - s2.x) ^ 2 + (s1.y - s2.y) ^ 2)


overlaps : Spot -> Spot -> Bool
overlaps s1 s2 =
    distance s1 s2 < diameter


overlapsAny : Spot -> List Spot -> Bool
overlapsAny spot =
    List.any (overlaps spot)


adjacent : Spot -> Spot -> Bool
adjacent s1 s2 =
    distance s1 s2 < adjacentDistance


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
    Dict.toList stones |> List.map (\( _, s ) -> s)


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



-- Links helpers


getStoneLinks : Stone -> List ( Spot, Spot )
getStoneLinks stone =
    List.map (\spots -> ( stone.spot, spots )) stone.adjacent


getUniqueLinks : Stones -> List ( Spot, Spot )
getUniqueLinks stones =
    let
        allLinks : List ( Spot, Spot )
        allLinks =
            stoneList stones
                |> List.concatMap (\s -> List.map (\a -> ( s.spot, a )) s.adjacent)

        -- filter out half of the duplicate links, so as to show each only once
        isChosen : ( Spot, Spot ) -> Bool
        isChosen ( s1, s2 ) =
            if s1.y < s2.y then
                True

            else
                s1.y == s2.y && s1.x < s2.x
    in
    List.filter isChosen allLinks


spotBorderNearestTo : Spot -> Spot -> Spot
spotBorderNearestTo otherSpot spot =
    let
        factor =
            stoneR / distance spot otherSpot

        roundAwayFromZero x =
            if x < 0 then
                floor x

            else
                ceiling x

        coord s os =
            s + (roundAwayFromZero <| factor * (toFloat <| os - s))
    in
    Spot (coord spot.x otherSpot.x) (coord spot.y otherSpot.y)



-- VIEW


hideLines : Stone -> Svg msg
hideLines { spot } =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt <| stoneR * 7
        ]
        []


viewStone : String -> Stone -> Svg msg
viewStone extraClass { player, spot } =
    let
        class =
            case player of
                Black ->
                    "black"

                White ->
                    "white"
    in
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        , SA.class class
        , SA.class extraClass
        ]
        []


viewHighlight : Spot -> Svg msg
viewHighlight spot =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        ]
        []


viewLiberty : Spot -> Svg msg
viewLiberty spot =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        ]
        []


viewLink : ( Spot, Spot ) -> Svg msg
viewLink ( s1, s2 ) =
    Svg.line
        [ SA.x1 <| String.fromInt s1.x
        , SA.y1 <| String.fromInt s1.y
        , SA.x2 <| String.fromInt s2.x
        , SA.y2 <| String.fromInt s2.y
        ]
        []


viewGhostStone : Stone -> Svg msg
viewGhostStone stone =
    viewStone "" stone


viewGhostLink : ( Spot, Spot ) -> Svg msg
viewGhostLink ( ghost, existing ) =
    viewLink ( spotBorderNearestTo existing ghost, existing )



-- Keyed


spotKeyStr : Spot -> String
spotKeyStr s =
    "-" ++ String.fromInt s.x ++ "-" ++ String.fromInt s.y


hideLinesKeyed : Stone -> ( String, Svg msg )
hideLinesKeyed stone =
    ( "hide" ++ spotKeyStr stone.spot
    , lazy hideLines stone
    )


viewKeyedStone : String -> Stone -> ( String, Svg msg )
viewKeyedStone extraClass stone =
    ( "stone" ++ spotKeyStr stone.spot
    , lazy2 viewStone extraClass stone
    )


viewKeyedLink : ( Spot, Spot ) -> ( String, Svg msg )
viewKeyedLink ( s1, s2 ) =
    ( "link" ++ spotKeyStr s1 ++ spotKeyStr s2
    , lazy viewLink ( s1, s2 )
    )


classJustPlayed : Maybe Spot -> Stone -> String
classJustPlayed justPlayed stone =
    if justPlayed == Just stone.spot then
        "just-played"

    else
        ""
