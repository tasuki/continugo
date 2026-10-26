module LibertiesTests exposing (..)

import Expect
import Fuzz
import Go exposing (..)
import Liberties exposing (..)
import Test exposing (..)


closest : Spot -> (() -> List Spot) -> Float
closest toSpot spots =
    List.map (distance toSpot) (spots ())
        |> List.minimum
        |> Maybe.withDefault 10000


findLibertiesTest =
    describe "findLiberties"
        [ let
            libs _ =
                findLiberties (createStone Black (Spot 560 314))
                    [ Spot 561 234 -- up
                    , Spot 635 331 -- lower right
                    , Spot 653 415 -- lower right further
                    , Spot 505 368 -- lower left
                    , Spot 543 455 -- lower left further
                    ]
          in
          describe "Basic test"
            [ test "Has three liberties" <|
                \_ -> Expect.equal 3 (List.length <| libs ())
            , test "Has upper right liberty" <|
                \_ -> Expect.lessThan 15 (closest (Spot 638 251) libs)
            , test "Has down liberty" <|
                \_ -> Expect.lessThan 6 (closest (Spot 581 389) libs)
            , test "Has left liberty" <|
                \_ -> Expect.lessThan 40 (closest (Spot 478 276) libs)
            ]
        , let
            libs : () -> List Spot
            libs _ =
                findLiberties (createStone Black (Spot 879 881))
                    [ Spot 881 793 -- up
                    , Spot 961 877 -- right
                    , Spot 879 962 -- down
                    , Spot 796 876 -- left
                    ]
          in
          describe "Two liberties in lower right"
            [ test "Has two liberties" <|
                \_ -> Expect.equal 2 (List.length <| libs ())
            , test "Has corner liberty" <|
                \_ -> Expect.lessThan 5 (closest (Spot 956 955) libs)
            , test "Has lower left liberty" <|
                \_ -> Expect.lessThan 5 (closest (Spot 802 952) libs)
            ]
        , let
            libs _ =
                findLiberties (createStone Black (Spot 895 266))
                    [ Spot 961 227 -- right
                    , Spot 866 189 -- up
                    , Spot 820 281 -- left
                    , Spot 895 344 -- down
                    ]
          in
          describe "One liberty on the right side"
            [ test "Has 1 liberty" <|
                \_ -> Expect.equal 1 (List.length <| libs ())
            , test "Has that liberty" <|
                \_ -> Expect.lessThan 5 (closest (Spot 961 305) libs)
            ]
        , let
            libs _ =
                findLiberties (createStone Black (Spot 949 51))
                    [ Spot 859 70 -- left
                    , Spot 930 141 -- down
                    ]
          in
          describe "No liberties in upper right"
            [ test "Has 0 liberties" <|
                \_ -> Expect.equal 0 (List.length <| libs ())
            ]
        ]


hasLibertiesTest : Test
hasLibertiesTest =
    let
        bruteForce spot nearby =
            List.any
                (\offset ->
                    let
                        candidate =
                            normalize spot offset
                    in
                    isWithinBoard candidate
                        && List.all (\other -> distance candidate other >= toFloat diameter) nearby
                )
                zeroLibCandidates

        spotFuzzer =
            Fuzz.map2 Spot (Fuzz.intRange boardMin boardMax) (Fuzz.intRange boardMin boardMax)

        offsetsFuzzer =
            Fuzz.listOfLengthBetween 0
                18
                (Fuzz.map2 Spot (Fuzz.intRange -180 180) (Fuzz.intRange -180 180))
    in
    describe "Exact interval liberty search"
        [ fuzz2 spotFuzzer offsetsFuzzer "Matches the exhaustive integer-ring search" <|
            \spot offsets ->
                let
                    nearby =
                        List.map (normalize spot) offsets
                in
                Expect.equal (bruteForce spot nearby)
                    (hasLiberties (createStone Black spot) nearby)
        , fuzz2 (Fuzz.intRange 0 3) offsetsFuzzer "Matches brute force at all four board corners" <|
            \corner offsets ->
                let
                    spot =
                        Spot
                            (if modBy 2 corner == 0 then
                                boardMin

                             else
                                boardMax
                            )
                            (if corner < 2 then
                                boardMin

                             else
                                boardMax
                            )

                    nearby =
                        List.map (normalize spot) offsets
                in
                Expect.equal (bruteForce spot nearby)
                    (hasLiberties (createStone Black spot) nearby)
        , test "An isolated touching liberty is not lost to interval rounding" <|
            \_ ->
                let
                    spot =
                        Spot boardMin boardMin

                    nearby =
                        [ Spot (boardMin + 2 * diameter) boardMin
                        , Spot boardMin (boardMin + diameter)
                        , Spot (boardMin + diameter) (boardMin + diameter)
                        ]
                in
                Expect.equal True (hasLiberties (createStone Black spot) nearby)
        , test "Squared-distance predicates keep strict touching and adjacency rules" <|
            \_ ->
                Expect.equal ( False, True, False )
                    ( overlaps (Spot 0 0) (Spot diameter 0)
                    , adjacent (Spot 0 0) (Spot (diameter - 1) diameter)
                    , adjacent (Spot 0 0) (Spot diameter diameter)
                    )
        ]


findNearestPlayableTest =
    describe "findNearestPlayable"
        [ test "Basic: 200, hover 250 => 276" <|
            \_ ->
                Expect.equal (Just <| Spot 276 200)
                    (findNearestPlayable (Spot 250 200) [ Spot 200 200 ])
        , test "Basic: 200, hover 270 => 276" <|
            \_ ->
                Expect.equal (Just <| Spot 276 200)
                    (findNearestPlayable (Spot 270 200) [ Spot 200 200 ])
        , test "Basic: 200, hover 230 => Nothing (cause 46 > stoneR)" <|
            \_ ->
                Expect.equal Nothing
                    (findNearestPlayable (Spot 230 200) [ Spot 200 200 ])
        , test "Between two stones" <|
            \_ ->
                Expect.equal (Just <| Spot 240 265)
                    (findNearestPlayable
                        (Spot 237 240)
                        [ Spot 200 200
                        , Spot 280 200
                        ]
                    )
        ]
