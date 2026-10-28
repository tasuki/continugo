module Rules.PositionTests exposing (tests)

import Dict
import Expect
import Fuzz
import Rules.Position exposing (..)
import Test exposing (..)


tests : Test
tests =
    let
        spotFuzzer =
            Fuzz.map2 Spot (Fuzz.intRange 0 1000) (Fuzz.intRange 0 1000)

        origin =
            Spot 500 500
    in
    describe "Position geometry and cached neighbors"
        [ fuzz2 spotFuzzer spotFuzzer "Integer nearby checks preserve the existing radius" <|
            \a b ->
                Expect.equal
                    (distance a b < 2 * toFloat diameter + adjacentDistance)
                    (isNearby a b)
        , test "Nearby filtering keeps close stones and excludes distant ones" <|
            \_ ->
                let
                    close =
                        createStone Black (translate origin (Spot (3 * diameter) diameter))

                    distant =
                        createStone White (translate origin (Spot (3 * diameter) (2 * diameter)))

                    stones =
                        Dict.fromList [ ( stoneKey close, close ), ( stoneKey distant, distant ) ]
                in
                Expect.equal [ close.spot ] (List.map .spot (nearbyStones stones origin))
        , test "Cached adjacency is strict and only connects matching colors" <|
            \_ ->
                let
                    inside =
                        createStone Black (translate origin (Spot (diameter - 1) diameter))

                    boundary =
                        createStone Black (translate origin (Spot diameter diameter))

                    oppositeColor =
                        createStone White (translate origin (Spot diameter 0))

                    cachedConnections neighbor =
                        withNeighbors
                            (Dict.singleton (stoneKey neighbor) neighbor)
                            (createStone Black origin)
                            |> .adjacent
                in
                Expect.equal [ [ inside.spot ], [], [] ]
                    (List.map cachedConnections [ inside, boundary, oppositeColor ])
        ]
