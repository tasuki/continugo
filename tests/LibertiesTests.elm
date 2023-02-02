module LibertiesTests exposing (..)

import Expect
import Go exposing (..)
import Liberties exposing (..)
import Test exposing (..)


expectAll : List Expect.Expectation -> Expect.Expectation
expectAll expectations =
    Expect.all (List.map always expectations) ()


expectLiberty : Int -> Int -> Int -> Coords -> Expect.Expectation
expectLiberty allowedError x y coords =
    Expect.all
        [ \_ -> Expect.lessThan allowedError <| abs (x - coords.x)
        , \_ -> Expect.lessThan allowedError <| abs (y - coords.y)
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
                            [ expectLiberty 4 638 251 a -- upper right
                            , expectLiberty 3 581 389 b -- down
                            , expectLiberty 9 478 276 c -- left
                            ]

                    _ ->
                        Expect.fail <|
                            "Expected to find 3 liberties, found: "
                                ++ String.fromInt (List.length liberties)
                                ++ " "
                                ++ Debug.toString liberties
        ]
