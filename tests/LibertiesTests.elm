module LibertiesTests exposing (..)

import Expect
import Go exposing (..)
import Liberties exposing (..)
import Test exposing (..)


closest : Spot -> (() -> List Spot) -> Float
closest toSpot spots =
    List.map (distance toSpot) (spots ())
        |> List.minimum
        |> Maybe.withDefault 10000


helpersTest =
    describe "Helpers"
        [ describe "findShift"
            [ test "find the shift of coords" <|
                \_ ->
                    Expect.equal (Shift 2 -4) <|
                        findShift (Spot 1 2) (Spot 3 -2)
            ]
        , describe "boardForce"
            [ test "works for small numbers" <|
                \_ -> Expect.equal (Shift stoneR 0) (boardForce <| Spot 0 100)
            , test "works for large numbers" <|
                \_ -> Expect.equal (Shift -stoneR 0) (boardForce <| Spot 1000 100)
            ]
        ]


findLibertyTest =
    describe "findLiberty"
        [ test "One liberty on the right side" <|
            \_ ->
                Expect.equal (Just <| Spot 961 305) <|
                    findLiberty
                        [ Spot 961 227 -- right
                        , Spot 866 189 -- up
                        , Spot 820 281 -- left
                        , Spot 895 344 -- down
                        ]
                        (Spot 895 266)
                        20
                        (Spot 961 305)
        , test "Two liberties in lower right: corner" <|
            \_ ->
                Expect.equal (Just <| Spot 956 955) <|
                    findLiberty
                        [ Spot 881 793 -- up
                        , Spot 961 877 -- right
                        , Spot 879 962 -- down
                        , Spot 796 876 -- left
                        ]
                        (Spot 879 881)
                        20
                        (Spot 956 955)
        , test "Two liberties in lower right: lower left" <|
            \_ ->
                Expect.equal (Just <| Spot 802 952) <|
                    findLiberty
                        [ Spot 881 793 -- up
                        , Spot 961 877 -- right
                        , Spot 879 962 -- down
                        , Spot 796 876 -- left
                        ]
                        (Spot 879 881)
                        20
                        (Spot 802 952)
        ]


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
