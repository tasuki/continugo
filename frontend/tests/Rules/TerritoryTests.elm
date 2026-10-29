module Rules.TerritoryTests exposing (tests)

import Dict
import Expect
import Fixtures.Territory as TerritoryFixtures
import Fuzz
import Rules.Board exposing (coordRange)
import Rules.Position exposing (Color(..), Spot, Stone, Stones, createStone, stoneKey)
import Rules.Territory as Territory exposing (Outcome(..))
import Test exposing (..)


stones : List Stone -> Stones
stones =
    List.map (\s -> ( stoneKey s, s )) >> Dict.fromList


black : Int -> Int -> Stone
black x y =
    createStone Black (Spot x y)


white : Int -> Int -> Stone
white x y =
    createStone White (Spot x y)


boardArea : Float
boardArea =
    toFloat coordRange ^ 2


tests : Test
tests =
    describe "Voronoi area scoring"
        [ describe "Shared territory fixtures" TerritoryFixtures.tests
        , test "More stones do not necessarily mean more territory" <|
            \_ ->
                let
                    area =
                        Territory.areas (stones [ black 100 450, black 100 550, white 500 500 ])
                in
                Expect.greaterThan area.black area.white
        , test "Adding a stone deep inside your area does not add a stone-count bonus" <|
            \_ ->
                Expect.equal
                    (Territory.areas (stones [ black 300 500, white 900 500 ]))
                    (Territory.areas (stones [ black 100 500, black 300 500, white 900 500 ]))
        , test "Removing a captured stone and restoring it changes and restores the score" <|
            \_ ->
                let
                    original =
                        stones [ black 100 500, white 900 500 ]

                    captured =
                        Dict.remove ( 900, 500 ) original

                    restored =
                        Dict.insert ( 900, 500 ) (white 900 500) captured
                in
                Expect.equal [ Draw, Win Black boardArea, Draw ]
                    (List.map (Territory.areas >> Territory.result) [ original, captured, restored ])
        , test "A small real lead is not rounded into a draw" <|
            \_ ->
                Expect.equal (Win Black 2)
                    (Territory.result (Territory.Areas 500001 499999))
        , test "Floating-point noise in a tied diagram is ignored" <|
            \_ ->
                Expect.equal Draw
                    (Territory.result (Territory.Areas 500000.000001 499999.999999))
        , fuzz (Fuzz.listOfLength 12 (Fuzz.map2 Spot (Fuzz.intRange 38 962) (Fuzz.intRange 38 962))) "All Voronoi cells partition the entire board" <|
            \spots ->
                let
                    position =
                        List.indexedMap
                            (\i spot ->
                                createStone
                                    (if modBy 2 i == 0 then
                                        Black

                                     else
                                        White
                                    )
                                    spot
                            )
                            spots
                            |> stones

                    actual =
                        Territory.areas position
                in
                Expect.all
                    [ \_ -> Expect.within (Expect.Absolute 0.00001) boardArea (actual.black + actual.white)
                    , \_ -> Expect.atLeast 0 actual.black
                    , \_ -> Expect.atLeast 0 actual.white
                    , \_ -> Expect.atMost boardArea actual.black
                    , \_ -> Expect.atMost boardArea actual.white
                    ]
                    ()
        ]
