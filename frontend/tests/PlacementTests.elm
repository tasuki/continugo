module PlacementTests exposing (tests)

import Dict
import Expect
import Placement exposing (nearestFreeSpot)
import Rules.Play as Play
import Rules.Position exposing (..)
import Test exposing (..)


tests : Test
tests =
    describe "Frontend placement adjustment"
        [ test "Basic: 200, hover 250 => 276" <|
            \_ ->
                Expect.equal (Just <| Spot 276 200)
                    (nearestFreeSpot (Spot 250 200) [ Spot 200 200 ])
        , test "Basic: 200, hover 270 => 276" <|
            \_ ->
                Expect.equal (Just <| Spot 276 200)
                    (nearestFreeSpot (Spot 270 200) [ Spot 200 200 ])
        , test "Basic: 200, hover 230 => Nothing (cause 46 > stoneR)" <|
            \_ ->
                Expect.equal Nothing
                    (nearestFreeSpot (Spot 230 200) [ Spot 200 200 ])
        , test "Between two stones" <|
            \_ ->
                Expect.equal (Just <| Spot 240 265)
                    (nearestFreeSpot
                        (Spot 237 240)
                        [ Spot 200 200
                        , Spot 280 200
                        ]
                    )
        , test "Only the frontend adjusts an overlapping coordinate" <|
            \_ ->
                let
                    stones =
                        Play.playStones [ createPlay Black (Spot 200 200) ] Dict.empty

                    intended =
                        Spot 250 200
                in
                Expect.equal ( Nothing, Just (Spot 276 200) )
                    ( Play.playIfLegal stones (createStone White intended)
                    , Placement.playNearby White stones intended |> Maybe.map (Tuple.second >> .spot)
                    )
        ]
