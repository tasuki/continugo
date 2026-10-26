module Board exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA


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


preciseRadius : Float
preciseRadius =
    -- for drawing the lines
    toFloat coordRange / (toFloat boardSize * 2)


stoneRadius : Int
stoneRadius =
    -- 9x9: 55.55, 13x13: 38.46, 19x19: 26.31
    -- this doesn't work as a float for a multitude of reasons
    floor preciseRadius


offset : Float -> Float
offset offsetBy =
    offsetBy * preciseRadius * 2 - preciseRadius


viewLines : List (Svg msg)
viewLines =
    let
        line x1 y1 x2 y2 =
            Svg.line
                [ SA.x1 <| String.fromFloat x1
                , SA.y1 <| String.fromFloat y1
                , SA.x2 <| String.fromFloat x2
                , SA.y2 <| String.fromFloat y2
                ]
                []

        ( start, end ) =
            ( preciseRadius, offset <| toFloat boardSize )

        offsets =
            List.range 1 boardSize |> List.map toFloat

        horizontal =
            List.map (\o -> line start (offset o) end (offset o)) offsets

        vertical =
            List.map (\o -> line (offset o) start (offset o) end) offsets
    in
    horizontal ++ vertical


viewStars : List (Svg msg)
viewStars =
    let
        ( a, z ) =
            ( 4, boardSize - 3 )

        star : Int -> Int -> Svg msg
        star x y =
            Svg.circle
                [ SA.cx <| String.fromFloat <| offset <| toFloat x
                , SA.cy <| String.fromFloat <| offset <| toFloat y
                , SA.r <| String.fromInt 4
                ]
                []
    in
    [ star a a
    , star z a
    , star a z
    , star z z
    ]
