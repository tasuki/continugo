module Rules.PlayTests exposing (tests)

import Dict
import Expect
import Fixtures.Play as PlayFixtures
import Rules.Play exposing (..)
import Test exposing (..)


playCase : PlayFixtures.Case -> Expect.Expectation
playCase tc =
    let
        stones =
            replayPlacementsOn tc.board tc.record Dict.empty

        stoneCountChange =
            playIfLegalOn tc.board stones tc.playStone
                |> Maybe.map (\result -> Dict.size result - Dict.size stones)
    in
    Expect.equal tc.stoneCountChange stoneCountChange


tests : Test
tests =
    PlayFixtures.tests playCase |> describe "play"
