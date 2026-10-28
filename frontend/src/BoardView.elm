module BoardView exposing (viewLines, viewStars)

import Rules.Board exposing (boardSize, coordRange)
import Svg exposing (Svg)
import Svg.Attributes as SA


preciseRadius : Float
preciseRadius =
    -- for drawing the lines
    toFloat coordRange / (toFloat boardSize * 2)


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
