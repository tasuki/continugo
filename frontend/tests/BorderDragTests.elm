module BorderDragTests exposing (tests)

import Analysis
import BoardGeometry exposing (Border(..))
import Dict
import DragFrom exposing (DragFrom(..))
import Expect
import Rules.Board as Board
import Rules.Position exposing (..)
import Test exposing (..)


tests : Test
tests =
    describe "Dragging from borders"
        [ test "All four edges clamp at the last connected centre on every board size" <|
            \_ ->
                [ ( 9, 133 ), ( 13, 92 ), ( 19, 63 ) ]
                    |> List.concatMap
                        (\( size, reach ) ->
                            let
                                board =
                                    Board.fromSize size |> Maybe.withDefault Board.default
                            in
                            [ ( Left, Spot reach 700 )
                            , ( Right, Spot (1000 - reach) 700 )
                            , ( Top, Spot 700 reach )
                            , ( Bottom, Spot 700 (1000 - reach) )
                            ]
                                |> List.map
                                    (\( border, expected ) ->
                                        DragFrom.findBorderDraggedSpot board [] [ border ] (Spot 700 700)
                                            == Just expected
                                    )
                        )
                    |> Expect.equal (List.repeat 12 True)
        , test "Corners retain both starting borders" <|
            \_ ->
                [ ( [ Left, Top ], Spot 92 92 )
                , ( [ Right, Top ], Spot 908 92 )
                , ( [ Left, Bottom ], Spot 92 908 )
                , ( [ Right, Bottom ], Spot 908 908 )
                ]
                    |> List.map
                        (\( borders, expected ) ->
                            DragFrom.findBorderDraggedSpot Board.default [] borders (Spot 500 500) == Just expected
                        )
                    |> Expect.equal (List.repeat 4 True)
        , test "Dragging towards an edge still leaves the stone inside the board" <|
            \_ ->
                DragFrom.findBorderDraggedSpot Board.default [] [ Left, Top ] (Spot 0 0)
                    |> Expect.equal (Just (Spot 38 38))
        , test "Snapping around blockers cannot break a border connection" <|
            \_ ->
                Expect.equal ( Just (Spot 92 474), Nothing )
                    ( DragFrom.findBorderDraggedSpot Board.default [ Spot 92 550 ] [ Left ] (Spot 500 500)
                    , DragFrom.findBorderDraggedSpot Board.default [ Spot 65 65 ] [ Left, Top ] (Spot 500 500)
                    )
        , test "The starting hover determines the borders; central drags remain free" <|
            \_ ->
                [ Spot 1 500, Spot 38 38, Spot 500 500 ]
                    |> List.map (DragFrom.getDragFrom Board.default Dict.empty Black)
                    |> Expect.equal
                        [ DragBorders (Spot 1 500) [ Left ]
                        , DragBorders (Spot 38 38) [ Left, Top ]
                        , DragSpot (Spot 500 500)
                        ]
        , test "An own stone still takes priority, and a hover snapped away from the edge does not latch" <|
            \_ ->
                let
                    stone =
                        createStone Black (Spot 38 500)

                    stones =
                        Dict.singleton (stoneKey stone) stone
                in
                Expect.equal ( DragStone stone, DragSpot (Spot 80 500) )
                    ( DragFrom.getDragFrom Board.default stones Black stone.spot
                    , DragFrom.getDragFrom Board.default stones Black (Spot 80 500)
                    )
        , test "Border presses remain taps until the pointer moves" <|
            \_ ->
                let
                    start =
                        Spot 38 38

                    origin =
                        DragFrom.getDragFrom Board.default Dict.empty Black start
                in
                Expect.equal ( True, False )
                    ( DragFrom.isTap start origin, DragFrom.isTap (Spot 500 500) origin )
        , test "Analysis circles follow the same edge and corner constraints" <|
            \_ ->
                [ Spot 38 500, Spot 38 38 ]
                    |> List.map
                        (\start ->
                            Analysis.start Board.default Dict.empty start Analysis.empty
                                |> Analysis.move Board.default Dict.empty (Spot 500 700)
                                |> Analysis.finish Board.default Dict.empty (Spot 500 700)
                                |> Tuple.first
                                |> .circles
                        )
                    |> Expect.equal [ [ Spot 92 700 ], [ Spot 92 92 ] ]
        ]
