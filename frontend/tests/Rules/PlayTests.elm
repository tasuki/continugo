module Rules.PlayTests exposing (tests)

import Dict
import Expect
import Fixtures.Play as PlayFixtures
import Rules.Play exposing (..)
import Test exposing (..)


playCase : PlayFixtures.Case -> Test
playCase tc =
    let
        stones =
            replayPlacements tc.record Dict.empty

        stoneCountChange =
            playIfLegal stones tc.playStone
                |> Maybe.map (\result -> Dict.size result - Dict.size stones)
    in
    test tc.name <|
        \_ ->
            Expect.equal tc.stoneCountChange stoneCountChange


tests : Test
tests =
    PlayFixtures.cases |> List.map playCase |> describe "play"
