module TerritoryViewTests exposing (tests)

import Expect
import Rules.Board as Board
import Rules.Territory exposing (Areas)
import Sgf
import TerritoryView
import Test exposing (..)


tests : Test
tests =
    describe "Score presentation"
        [ test "Results use board points without rounding a small lead into a draw" <|
            \_ ->
                Expect.equal [ "jigo", "B+169", "W+169", "W+101.4", "B+<0.1" ]
                    (List.map (TerritoryView.resultString Board.default 0)
                        [ Areas 0 0
                        , Areas 1000000 0
                        , Areas 0 1000000
                        , Areas 200000 800000
                        , Areas 500001 499999
                        ]
                    )
        , test "Root komi adjusts the result in board points on every board size" <|
            \_ ->
                let
                    result size root =
                        TerritoryView.resultString
                            (Maybe.withDefault Board.default (Board.fromSize size))
                            (Sgf.komiFromRoot root |> Maybe.withDefault 0)
                            (Areas 500000 500000)
                in
                Expect.equal
                    [ "jigo", "jigo", "W+6.3", "W+6.3", "W+6.3", "B+2.5", "jigo" ]
                    [ result 13 []
                    , result 13 [ ( "KM", [ "0" ] ) ]
                    , result 9 [ ( "KM", [ "6.3" ] ) ]
                    , result 13 [ ( "KM", [ "6.3" ] ) ]
                    , result 19 [ ( "KM", [ "6.3" ] ) ]
                    , result 13 [ ( "KM", [ "-2.5" ] ) ]
                    , result 13 [ ( "KM", [ "bad" ] ) ]
                    ]
        , test "Komi can reverse a territory lead or make a draw" <|
            \_ ->
                let
                    board =
                        Maybe.withDefault Board.default (Board.fromSize 10)

                    territory =
                        Areas 600000 400000
                in
                Expect.equal [ "B+20", "jigo", "W+0.5" ]
                    (List.map (\komi -> TerritoryView.resultString board komi territory) [ 0, 20, 20.5 ])
        , test "The whole board is size squared points" <|
            \_ ->
                Expect.equal [ "81", "169", "361" ]
                    (List.map
                        (\size -> TerritoryView.formatArea (Maybe.withDefault Board.default (Board.fromSize size)) 1000000)
                        [ 9, 13, 19 ]
                    )
        , test "Area counts are rounded to one decimal place" <|
            \_ ->
                Expect.equal "3.2" (TerritoryView.formatArea Board.default (3.24 * 1000000 / 169))
        ]
