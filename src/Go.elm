module Go exposing (..)


coordRange =
    1000


stoneR =
    -- 9x9: 55.6, 13x13: 38.5, 19x19: 26.3
    38


diameter =
    2 * stoneR


connectedDistance =
    1.4142



-- MODEL


type alias Stones =
    List Stone


type alias Links =
    List ( Stone, Stone )


type alias Stone =
    ( Player, Coords )


type Player
    = Black
    | White


type alias Coords =
    { x : Int, y : Int }


otherPlayer : Player -> Player
otherPlayer p =
    case p of
        White ->
            Black

        Black ->
            White


withinBoard : Stone -> Bool
withinBoard ( _, coords ) =
    let
        isWithin : Int -> Bool
        isWithin coord =
            coord > stoneR && coord < coordRange - stoneR
    in
    isWithin coords.x && isWithin coords.y


distance : Stone -> Stone -> Float
distance ( _, c1 ) ( _, c2 ) =
    sqrt <| toFloat ((c1.x - c2.x) ^ 2 + (c1.y - c2.y) ^ 2)


overlaps : Stone -> Stones -> Bool
overlaps stone =
    List.any (\s -> distance stone s < diameter)


newLinks : Stone -> Stones -> Links
newLinks stone =
    let
        ( player, _ ) =
            stone

        maybeLink s1 s2 =
            if distance s1 s2 < diameter * connectedDistance then
                Just ( s1, s2 )

            else
                Nothing
    in
    List.filter (\( p, _ ) -> p == player) >> List.filterMap (maybeLink stone)


playIfLegal : Stone -> Stones -> Maybe Stones
playIfLegal stone stones =
    if withinBoard stone && (not <| overlaps stone stones) then
        Just <| stone :: stones

    else
        Nothing
