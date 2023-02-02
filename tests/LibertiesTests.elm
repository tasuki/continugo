module LibertiesTests exposing (..)

import Expect
import Go exposing (..)
import Liberties exposing (..)
import Test exposing (..)


expectAll : List Expect.Expectation -> Expect.Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()


expectLiberty : Int -> Int -> Int -> Spot -> Expect.Expectation
expectLiberty allowedError x y spot =
    Expect.all
        [ \_ -> Expect.lessThan allowedError <| abs (x - spot.x)
        , \_ -> Expect.lessThan allowedError <| abs (y - spot.y)
        ]
        ()


helpersTest =
    describe "Helpers"
        [ test "Determines the shift of coords" <|
            \_ ->
                findShift { x = 1, y = 2 } { x = 3, y = -2 }
                    |> Expect.equal (Shift { x = 2, y = -4 })
        ]


libertiesTest =
    describe "Liberties"
        [ test "Find 3 liberties basic example" <|
            \_ ->
                let
                    liberties =
                        findLiberties (createStone Black { x = 560, y = 314 })
                            [ { x = 561, y = 234 } -- up
                            , { x = 635, y = 331 } -- lower right
                            , { x = 653, y = 415 } -- lower right further
                            , { x = 505, y = 368 } -- lower left
                            , { x = 543, y = 455 } -- lower left further
                            ]
                in
                case liberties of
                    a :: b :: c :: [] ->
                        expectAll
                            [ expectLiberty 5 638 251 a -- upper right
                            , expectLiberty 5 581 389 b -- down
                            , expectLiberty 9 478 276 c -- left
                            ]

                    _ ->
                        Expect.fail <|
                            "Expected to find 3 liberties, found: "
                                ++ String.fromInt (List.length liberties)
                                ++ " "
                                ++ Debug.toString liberties
        , test "Find 2 liberties in lower right corner" <|
            \_ ->
                let
                    liberties =
                        findLiberties (createStone Black { x = 879, y = 881 })
                            [ { x = 881, y = 793 } -- up
                            , { x = 961, y = 877 } -- right
                            , { x = 879, y = 962 } -- down
                            , { x = 796, y = 876 } -- left
                            ]
                in
                case liberties of
                    a :: b :: [] ->
                        -- corner liberty
                        expectAll
                            [ expectLiberty 5 956 955 a
                            , expectLiberty 5 802 952 b
                            ]

                    _ ->
                        Expect.fail <|
                            "Expected to find 2 liberties, found: "
                                ++ String.fromInt (List.length liberties)
                                ++ " "
                                ++ Debug.toString liberties
        , test "Find 1 liberty on right side" <|
            \_ ->
                let
                    liberties =
                        findLiberties (createStone Black { x = 895, y = 266 })
                            [ { x = 961, y = 227 } -- right
                            , { x = 866, y = 189 } -- up
                            , { x = 820, y = 281 } -- left
                            , { x = 895, y = 344 } -- down
                            ]
                in
                case liberties of
                    a :: [] ->
                        expectLiberty 5 961 305 a

                    _ ->
                        Expect.fail <|
                            "Expected to find 1 liberty, found: "
                                ++ String.fromInt (List.length liberties)
                                ++ " "
                                ++ Debug.toString liberties
        , test "Find no liberties in upper right" <|
            \_ ->
                let
                    liberties =
                        findLiberties (createStone Black { x = 949, y = 51 })
                            [ { x = 859, y = 70 } -- left
                            , { x = 930, y = 141 } -- down
                            ]
                in
                case liberties of
                    [] ->
                        Expect.pass

                    _ ->
                        Expect.fail <|
                            "Expected to find 0 liberties, found: "
                                ++ String.fromInt (List.length liberties)
                                ++ " "
                                ++ Debug.toString liberties
        ]
