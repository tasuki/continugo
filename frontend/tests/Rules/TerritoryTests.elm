module Rules.TerritoryTests exposing (tests)

import Dict
import Expect
import Fuzz
import Rules.Board exposing (coordRange)
import Rules.Position exposing (Player(..), Spot, Stone, Stones, createStone, stoneKey)
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


expectAreas : Float -> Float -> Stones -> Expect.Expectation
expectAreas expectedBlack expectedWhite position =
    let
        actual =
            Territory.areas position
    in
    Expect.all
        [ \_ -> Expect.within (Expect.Absolute 0.00001) expectedBlack actual.black
        , \_ -> Expect.within (Expect.Absolute 0.00001) expectedWhite actual.white
        ]
        ()


tests : Test
tests =
    describe "Voronoi area scoring"
        [ test "An empty board has no owned area and is a draw" <|
            \_ ->
                Expect.equal ( Territory.Areas 0 0, Draw )
                    ( Territory.areas Dict.empty, Territory.result (Territory.areas Dict.empty) )
        , test "A single stone owns the entire board, even when near a corner" <|
            \_ ->
                Expect.all
                    [ \_ -> expectAreas boardArea 0 (stones [ black 38 38 ])
                    , \_ -> expectAreas 0 boardArea (stones [ white 962 962 ])
                    , \_ -> Expect.equal (Win Black boardArea) (Territory.result (Territory.areas (stones [ black 500 500 ])))
                    ]
                    ()
        , test "Two symmetric stones divide the board equally" <|
            \_ ->
                let
                    position =
                        stones [ black 100 500, white 900 500 ]
                in
                Expect.all
                    [ \_ -> expectAreas (boardArea / 2) (boardArea / 2) position
                    , \_ -> Expect.equal Draw (Territory.result (Territory.areas position))
                    ]
                    ()
        , test "An off-centre bisector gives the correct shares and area margin" <|
            \_ ->
                let
                    position =
                        stones [ black 100 500, white 300 500 ]
                in
                Expect.all
                    [ \_ -> expectAreas (boardArea * 0.2) (boardArea * 0.8) position
                    , \_ -> Expect.equal (Win White (boardArea * 0.6)) (Territory.result (Territory.areas position))
                    ]
                    ()
        , test "A diagonal bisector clips the board correctly" <|
            \_ ->
                expectAreas (boardArea / 2)
                    (boardArea / 2)
                    (stones [ black 250 250, white 750 750 ])
        , test "Cocircular sites have four quarter-board cells, not a triangulation-dependent score" <|
            \_ ->
                expectAreas (boardArea / 4)
                    (boardArea * 0.75)
                    (stones [ black 250 250, white 750 250, white 250 750, white 750 750 ])
        , test "Collinear sites are supported" <|
            \_ ->
                expectAreas (boardArea * 0.35)
                    (boardArea * 0.65)
                    (stones [ black 200 500, white 500 500, white 800 500 ])
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
