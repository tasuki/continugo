module Rules.Board exposing (Board, boardSize, coordRange, default, diameter, fromSize, radius, size, stoneRadius)

{-| The coordinate space is always 1000 units. Positive integer board sizes
must leave a stone radius of at least one. The default is 13x13; the frontend
passes its selected board explicitly to geometry and rules.
-}


type Board
    = Board Int


default : Board
default =
    Board 13


coordRange : Int
coordRange =
    -- number of coordinates on actual board
    -- sorry this is actually still discrete
    1000


fromSize : Int -> Maybe Board
fromSize value =
    if value > 0 && value <= coordRange // 2 then
        Just (Board value)

    else
        Nothing


size : Board -> Int
size (Board value) =
    value


radius : Board -> Int
radius board =
    -- 9x9: 55.55, 13x13: 38.46, 19x19: 26.31
    -- this doesn't work as a float for a multitude of reasons
    coordRange // (2 * size board)


diameter : Board -> Int
diameter board =
    2 * radius board


boardSize : Int
boardSize =
    -- the number of "intersections"
    -- what was an intersection again?
    size default


stoneRadius : Int
stoneRadius =
    radius default
