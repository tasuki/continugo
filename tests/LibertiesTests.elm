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
                    |> Expect.equal { x = 2, y = -4 }
        ]


libertiesTest =
    describe "Liberties"
        [ test "Find 3 liberties basic example" <|
            \_ ->
                let
                    liberties =
                        findLiberties (createStone Black { x = 560, y = 314 })
                            [ createStone White { x = 561, y = 234 } -- up
                            , createStone White { x = 635, y = 331 } -- lower right
                            , createStone White { x = 653, y = 415 } -- lower right further
                            , createStone White { x = 505, y = 368 } -- lower left
                            , createStone White { x = 543, y = 455 } -- lower left further
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
                            [ createStone White { x = 881, y = 793 } -- up
                            , createStone White { x = 961, y = 877 } -- right
                            , createStone White { x = 879, y = 962 } -- down
                            , createStone White { x = 796, y = 876 } -- left
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
                            [ createStone White { x = 961, y = 227 } -- right
                            , createStone White { x = 866, y = 189 } -- up
                            , createStone White { x = 820, y = 281 } -- left
                            , createStone White { x = 895, y = 344 } -- down
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
        ]
