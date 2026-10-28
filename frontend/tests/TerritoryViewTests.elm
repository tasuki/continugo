module TerritoryViewTests exposing (tests)

import Expect
import Rules.Board exposing (stoneRadius)
import Rules.Territory exposing (Areas)
import TerritoryView
import Test exposing (..)


tests : Test
tests =
    describe "Score presentation"
        [ test "Results use stone-area units without rounding a small lead into a draw" <|
            \_ ->
                Expect.equal [ "jigo", "B+220.4", "W+220.4", "W+132.3", "B+<0.1" ]
                    (List.map TerritoryView.resultString
                        [ Areas 0 0
                        , Areas 1000000 0
                        , Areas 0 1000000
                        , Areas 200000 800000
                        , Areas 500001 499999
                        ]
                    )
        , test "One scoring unit is the area of a stone disc, not its bounding square" <|
            \_ ->
                Expect.equal "1" (TerritoryView.formatArea (pi * toFloat stoneRadius ^ 2))
        , test "Area counts are rounded to one decimal place" <|
            \_ ->
                Expect.equal "3.2" (TerritoryView.formatArea (3.24 * pi * toFloat stoneRadius ^ 2))
        ]
