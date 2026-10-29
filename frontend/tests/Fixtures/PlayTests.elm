module Fixtures.PlayTests exposing (tests)

import Expect
import Fixtures.Generated as Generated
import Fixtures.Play as Fixtures
import Rules.Board as Board
import Rules.Position exposing (Color(..), Spot, createStone)
import Test exposing (..)


example : Generated.Case
example =
    { filename = "example.txt"
    , name = "Example"
    , boardSize = 13
    , record = ""
    , move = "B[bbbb]"
    , stoneCountChange = Just 0
    }


tests : Test
tests =
    describe "Shared fixture decoding"
        [ test "Default fixtures use a 1000-unit board with radius 38" <|
            \_ ->
                Expect.equal ( 1000, 38 ) ( Board.coordRange, Board.stoneRadius )
        , test "An empty record and a zero stone-count change are valid" <|
            \_ ->
                Fixtures.decode example
                    |> Expect.equal
                        (Ok
                            { name = "Example"
                            , board = Board.default
                            , record = []
                            , playStone = createStone Black (Spot 53 53)
                            , stoneCountChange = Just 0
                            }
                        )
        , [ ( "malformed record", { example | record = "B[bbbb];bad" } )
          , ( "malformed move", { example | move = "bad" } )
          , ( "pass instead of placement", { example | move = "B[]" } )
          ]
            |> List.map
                (\( name, fixture ) ->
                    test ("Rejects " ++ name) <|
                        \_ -> Fixtures.decode fixture |> Expect.err
                )
            |> describe "Invalid SGF"
        , test "A size that would produce a zero-radius stone is rejected" <|
            \_ ->
                Fixtures.decode { example | boardSize = 501 } |> Expect.err
        ]
