module Rules.PlayTests exposing (tests)

import Dict
import Expect
import Fixtures.Play as PlayFixtures
import Rules.Game as Game
import Rules.Play as Play
import Test exposing (..)


tests : Test
tests =
    test "History-free placements leave ko checking to Game" <|
        \_ ->
            PlayFixtures.withCase "ko-shifted.txt" <|
                \tc ->
                    let
                        game =
                            Game.fromRecordOn tc.board (List.reverse tc.record)

                        stoneCountChange =
                            Play.playIfLegalOn tc.board game.stones tc.playStone
                                |> Maybe.map (\stones -> Dict.size stones - Dict.size game.stones)
                    in
                    Expect.equal ( Just 0, Nothing )
                        ( stoneCountChange, Game.play tc.playStone.spot game )
