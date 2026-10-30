module BoardView exposing (viewLines, viewStars)

import Rules.Board as Board exposing (Board, coordRange)
import Svg exposing (Svg)
import Svg.Attributes as SA


offset : Board -> Float -> Float
offset board offsetBy =
    (offsetBy - 0.5) * toFloat coordRange / toFloat (Board.size board)


viewLines : Board -> List (Svg msg)
viewLines board =
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
            ( offset board 1, offset board (toFloat (Board.size board)) )

        offsets =
            List.range 1 (Board.size board) |> List.map (toFloat >> offset board)

        horizontal =
            List.map (\o -> line start o end o) offsets

        vertical =
            List.map (\o -> line o start o end) offsets
    in
    horizontal ++ vertical


viewStars : Board -> List (Svg msg)
viewStars board =
    let
        size =
            Board.size board

        edge =
            if size < 13 then
                3

            else
                4

        offsets =
            if size == 19 then
                [ edge, 10, size + 1 - edge ]

            else
                [ edge, size + 1 - edge ]

        star : Int -> Int -> Svg msg
        star x y =
            Svg.circle
                [ SA.cx <| String.fromFloat <| offset board <| toFloat x
                , SA.cy <| String.fromFloat <| offset board <| toFloat y
                , SA.r <| String.fromInt 4
                ]
                []
    in
    if size < 7 then
        []

    else
        List.concatMap (\x -> List.map (star x) offsets) offsets
            ++ (if size /= 19 && modBy 2 size == 1 then
                    [ star ((size + 1) // 2) ((size + 1) // 2) ]

                else
                    []
               )
