module TerritoryViewTests exposing (tests)

import Expect
import Rules.Board as Board
import Rules.Territory exposing (Areas)
import TerritoryView
import Test exposing (..)


tests : Test
tests =
    describe "Score presentation"
        [ test "Results use board points without rounding a small lead into a draw" <|
            \_ ->
                Expect.equal [ "jigo", "B+169", "W+169", "W+101.4", "B+<0.1" ]
                    (List.map (TerritoryView.resultString Board.default)
                        [ Areas 0 0
                        , Areas 1000000 0
                        , Areas 0 1000000
                        , Areas 200000 800000
                        , Areas 500001 499999
                        ]
                    )
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
