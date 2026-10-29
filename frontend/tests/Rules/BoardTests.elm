module Rules.BoardTests exposing (tests)

import Dict
import Expect
import Rules.Board as Board exposing (Board)
import Rules.Game as Game
import Rules.Liberties as Liberties
import Rules.Position exposing (..)
import Test exposing (..)


sizeTests : ( Int, Int ) -> Test
sizeTests ( value, radius ) =
    case Board.fromSize value of
        Just board ->
            configuredTests board radius

        Nothing ->
            test ("Construct board size " ++ String.fromInt value) <|
                \_ -> Expect.fail "Expected a valid board size"


configuredTests : Board -> Int -> Test
configuredTests board radius =
    let
        d =
            2 * radius

        origin =
            Spot 0 0
    in
    describe (String.fromInt (Board.size board) ++ "x" ++ String.fromInt (Board.size board))
        [ test "Radius is calculated from the equivalent board size" <|
            \_ -> Expect.equal radius (Board.radius board)
        , test "The stone centre must stay one radius inside the board" <|
            \_ ->
                Expect.equal [ True, False, False ]
                    (List.map (isWithinBoardOn board)
                        [ Spot radius (1000 - radius)
                        , Spot (radius - 1) radius
                        , Spot radius (1001 - radius)
                        ]
                    )
        , test "Touching is allowed and adjacency remains strict" <|
            \_ ->
                Expect.equal ( False, True, False )
                    ( overlapsOn board origin (Spot d 0)
                    , adjacentOn board origin (Spot (d - 1) d)
                    , adjacentOn board origin (Spot d d)
                    )
        , test "Games preserve the board through placements, passes, and restoration" <|
            \_ ->
                Game.onBoard board
                    |> Game.play (Spot radius 500)
                    |> Maybe.andThen (Game.play (Spot (radius + d) 500))
                    |> Maybe.andThen Game.pass
                    |> Maybe.map
                        (\game ->
                            ( game.board, Dict.size game.stones, Game.fromRecordOn board game.record == game )
                        )
                    |> Expect.equal (Just ( board, 2, True ))
        , test "Liberty search uses the selected radius beyond the four quick probes" <|
            \_ ->
                let
                    offset =
                        2 * d - 1
                in
                Liberties.hasLibertiesOn board
                    (createStone Black (Spot 500 500))
                    [ Spot (500 - offset) 500
                    , Spot (500 + offset) 500
                    , Spot 500 (500 - offset)
                    , Spot 500 (500 + offset)
                    ]
                    |> Expect.equal True
        ]


tests : Test
tests =
    describe "Board sizes"
        [ test "The default remains the 13x13 equivalent" <|
            \_ -> Expect.equal 13 (Board.size Board.default)
        , test "Invalid sizes cannot construct boards" <|
            \_ ->
                List.map Board.fromSize [ -1, 0, 501, 1000 ]
                    |> Expect.equal [ Nothing, Nothing, Nothing, Nothing ]
        , test "Sizes at the coordinate system's limits are accepted" <|
            \_ ->
                List.map (Board.fromSize >> Maybe.map Board.radius) [ 1, 500 ]
                    |> Expect.equal [ Just 500, Just 1 ]
        , List.map sizeTests [ ( 9, 55 ), ( 11, 45 ), ( 13, 38 ), ( 19, 26 ) ]
            |> describe "Configured rules"
        ]
