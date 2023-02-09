module PlayTests exposing (..)

import Dict
import Expect
import Go exposing (..)
import Play exposing (playIfLegal, playStones)
import SamplePositions
import Test exposing (..)


playIfLegalTest =
    describe "playIfLegal"
        [ test "Allows suicide when taking" <|
            \_ ->
                Expect.equal (Just 5) <|
                    Maybe.map (stoneList >> List.length) <|
                        playIfLegal
                            (playStones SamplePositions.suicideOkWhenCapture Dict.empty)
                            (createStone Black <| Spot 943 58)
        ]
