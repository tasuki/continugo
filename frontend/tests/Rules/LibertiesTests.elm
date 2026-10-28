module Rules.LibertiesTests exposing (..)

import Expect
import Fuzz
import Rules.Liberties exposing (..)
import Rules.Position exposing (..)
import Test exposing (..)


closestDistance : Spot -> (() -> List Spot) -> Float
closestDistance toSpot spots =
    List.map (distance toSpot) (spots ())
        |> List.minimum
        |> Maybe.withDefault 10000


sampleLibertiesTest =
    describe "sampleLiberties"
        [ let
            libs _ =
                sampleLiberties (createStone Black (Spot 560 314))
                    [ Spot 561 234 -- up
                    , Spot 635 331 -- lower right
                    , Spot 653 415 -- lower right further
                    , Spot 505 368 -- lower left
                    , Spot 543 455 -- lower left further
                    ]
          in
          describe "Three liberty samples around a blocked stone"
            [ test "Has three non-overlapping liberty samples" <|
                \_ -> Expect.equal 3 (List.length <| libs ())
            , test "Has upper right liberty" <|
                \_ -> Expect.lessThan 15 (closestDistance (Spot 638 251) libs)
            , test "Has down liberty" <|
                \_ -> Expect.lessThan 6 (closestDistance (Spot 581 389) libs)
            , test "Has left liberty" <|
                \_ -> Expect.lessThan 40 (closestDistance (Spot 478 276) libs)
            ]
        , let
            libs : () -> List Spot
            libs _ =
                sampleLiberties (createStone Black (Spot 879 881))
                    [ Spot 881 793 -- up
                    , Spot 961 877 -- right
                    , Spot 879 962 -- down
                    , Spot 796 876 -- left
                    ]
          in
          describe "Two liberty samples near the lower-right corner"
            [ test "Has two non-overlapping liberty samples" <|
                \_ -> Expect.equal 2 (List.length <| libs ())
            , test "Has corner liberty" <|
                \_ -> Expect.lessThan 5 (closestDistance (Spot 956 955) libs)
            , test "Has lower left liberty" <|
                \_ -> Expect.lessThan 5 (closestDistance (Spot 802 952) libs)
            ]
        , let
            libs _ =
                sampleLiberties (createStone Black (Spot 895 266))
                    [ Spot 961 227 -- right
                    , Spot 866 189 -- up
                    , Spot 820 281 -- left
                    , Spot 895 344 -- down
                    ]
          in
          describe "One liberty sample on the right side"
            [ test "Has one liberty sample" <|
                \_ -> Expect.equal 1 (List.length <| libs ())
            , test "Has that liberty" <|
                \_ -> Expect.lessThan 5 (closestDistance (Spot 961 305) libs)
            ]
        , let
            libs _ =
                sampleLiberties (createStone Black (Spot 949 51))
                    [ Spot 859 70 -- left
                    , Spot 930 141 -- down
                    ]
          in
          describe "No liberties in upper right"
            [ test "Has no liberty samples" <|
                \_ -> Expect.equal 0 (List.length <| libs ())
            ]
        ]


hasLibertiesTest : Test
hasLibertiesTest =
    let
        exhaustiveOffsets =
            let
                range =
                    List.range -(2 * diameter) (2 * diameter)

                origin =
                    Spot 0 0
            in
            List.concatMap (\x -> List.map (Spot x) range) range
                |> List.filter (\offset -> adjacent origin offset && not (overlaps origin offset))

        probeBlockers =
            let
                offset =
                    2 * diameter - 1
            in
            [ Spot -offset 0, Spot offset 0, Spot 0 -offset, Spot 0 offset ]

        bruteForce spot nearby =
            List.any
                (\offset ->
                    let
                        candidate =
                            translate spot offset
                    in
                    isWithinBoard candidate
                        && List.all (\other -> distanceSquared candidate other >= diameter * diameter) nearby
                )
                exhaustiveOffsets

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
                        List.map (translate spot) offsets
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
                        List.map (translate spot) offsets
                in
                Expect.equal (bruteForce spot nearby)
                    (hasLiberties (createStone Black spot) nearby)
        , fuzz2 spotFuzzer offsetsFuzzer "Matches brute force with all quick probes blocked" <|
            \spot offsets ->
                let
                    nearby =
                        List.map (translate spot) (probeBlockers ++ offsets)
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
