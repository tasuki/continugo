module Board exposing (..)

import Go exposing (coordRange)
import Svg exposing (Svg)
import Svg.Attributes as SA


boardSize =
    13


preciseR =
    coordRange / (boardSize * 2)


offset : Float -> Float
offset lines =
    lines * preciseR * 2 - preciseR


viewLines : List (Svg msg)
viewLines =
    let
        line x1 y1 x2 y2 =
            Svg.line
                [ SA.x1 <| String.fromFloat x1
                , SA.y1 <| String.fromFloat y1
                , SA.x2 <| String.fromFloat x2
                , SA.y2 <| String.fromFloat y2
                , SA.stroke "#993"
                , SA.strokeWidth "1.5"
                ]
                []

        ( start, end ) =
            ( preciseR, offset boardSize )

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

        star x y =
            Svg.circle
                [ SA.cx <| String.fromFloat (offset x)
                , SA.cy <| String.fromFloat (offset y)
                , SA.r <| String.fromInt 4
                , SA.fill "#993"
                ]
                []
    in
    [ star a a
    , star z a
    , star a z
    , star z z
    ]
