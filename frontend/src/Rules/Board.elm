module Rules.Board exposing (boardSize, coordRange, stoneRadius)


coordRange : Int
coordRange =
    -- number of coordinates on actual board
    -- sorry this is actually still discrete
    1000


boardSize : Int
boardSize =
    -- the number of "intersections"
    -- what was an intersection again?
    13


stoneRadius : Int
stoneRadius =
    -- 9x9: 55.55, 13x13: 38.46, 19x19: 26.31
    -- this doesn't work as a float for a multitude of reasons
    coordRange // (2 * boardSize)
