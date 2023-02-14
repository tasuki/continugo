module PlayTests exposing (..)

import Dict
import Expect
import Go exposing (..)
import Play exposing (..)
import SamplePositions
import Sgf
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


record =
    Sgf.decode <|
        String.concat
            [ "B[oifh];W[fgoc];B[odpz];W[fhfm];B[ihdL];W[lfem];B[ekpj];W[fsqe];"
            , "B[dWnk];W[eSmd];B[dGll];W[dsqt];B[fHkR];W[iogz];B[jAfl];W[kPgM];"
            , "B[gMfX];W[gUez];B[jnhQ];W[hIit];B[lLfD];W[mRgQ];B[mLdR];W[jPcR];"
            , "B[iHbT];W[hqcz];B[kJbI];W[mbcC];B[nrbT];W[hLaM];B[lZaM];W[jKjx];"
            , "B[lfiC];W[lCko];B[mVjo];W[nAkG];B[oQjC];W[pCkQ];B[cQim];W[qrmH];"
            , "B[pYii];W[qNju];B[regx];W[oshO];B[codi];W[dhfM];B[hNlD];W[pvgk]"
            ]


spot =
    Spot 839 240


playTest =
    describe "play"
        [ test "Captures a group of four stones" <|
            \_ ->
                let
                    stones =
                        playStones record Dict.empty
                in
                Expect.equal (Just 3) <|
                    (play Black stones spot
                        |> Maybe.map (\( s, _ ) -> Dict.size stones - Dict.size s)
                    )
        ]
