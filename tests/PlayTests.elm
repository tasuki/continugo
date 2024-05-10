module PlayTests exposing (..)

import Dict
import Expect
import Go exposing (..)
import Play exposing (..)
import Sgf
import Test exposing (..)


stoneFromStr : String -> Stone
stoneFromStr str =
    Sgf.decode str
        |> List.head
        |> Maybe.andThen
            (\play ->
                case play.move of
                    Place s ->
                        Just s

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault (createStone Black <| Spot 0 0)


longGameRecord : List Play
longGameRecord =
    Sgf.decode <|
        String.concat
            [ "B[oifh];W[fgoc];B[odpz];W[fhfm];B[ihdL];W[lfem];B[ekpj];W[fsqe];"
            , "B[dWnk];W[eSmd];B[dGll];W[dsqt];B[fHkR];W[iogz];B[jAfl];W[kPgM];"
            , "B[gMfX];W[gUez];B[jnhQ];W[hIit];B[lLfD];W[mRgQ];B[mLdR];W[jPcR];"
            , "B[iHbT];W[hqcz];B[kJbI];W[mbcC];B[nrbT];W[hLaM];B[lZaM];W[jKjx];"
            , "B[lfiC];W[lCko];B[mVjo];W[nAkG];B[oQjC];W[pCkQ];B[cQim];W[qrmH];"
            , "B[pYii];W[qNju];B[regx];W[oshO];B[codi];W[dhfM];B[hNlD];W[pvgk]"
            ]


suicideOkWhenCapture =
    [ createPlay White <| Spot 842 74
    , createPlay White <| Spot 909 151
    , createPlay Black <| Spot 730 73
    , createPlay Black <| Spot 800 161
    , createPlay Black <| Spot 876 234
    , createPlay Black <| Spot 961 240
    ]


suicideOkWhenCaptureConnected =
    Sgf.decode <|
        "B[aMcm];W[aMdK];B[cmco];W[ckdM];B[dKcm];W[eddI];B[eCaO];W[fmcj];W[gaaM];W[cIaS]"


type alias Case =
    { name : String
    , record : List Play
    , playStone : Stone
    , newStones : Maybe Int
    }


cases : List Case
cases =
    [ { name = "Adds a move"
      , record = Sgf.decode ""
      , playStone = stoneFromStr "B[boob]"
      , newStones = Just 1
      }
    , { name = "Doesn't add an illegal move"
      , record = Sgf.decode ""
      , playStone = stoneFromStr "B[butt]"
      , newStones = Nothing
      }
    , { name = "Captures a group of four stones"
      , record = longGameRecord
      , playStone = createStone Black <| Spot 839 240
      , newStones = Just -3
      }
    , { name = "Allows suicide when taking"
      , record = suicideOkWhenCapture
      , playStone = createStone Black <| Spot 943 58
      , newStones = Just -1
      }

    --, { name = "Allows capturing a one-eyed group"
    --  , record = suicideOkWhenCaptureConnected
    --  , playStone = stoneFromStr "W[aMaM]"
    --  , newStones = Just 0
    --  }
    --, { name = "Allows one-eyed group to survive a little longer"
    --  , record = suicideOkWhenCaptureConnected
    --  , playStone = stoneFromStr "B[aMaM]"
    --  , newStones = Just 0
    --  }
    ]


playCase : Case -> Test
playCase tc =
    let
        stones =
            playStones tc.record Dict.empty

        newStones =
            playNearby tc.playStone.player stones tc.playStone.spot
                |> Maybe.map (\( s, _ ) -> Dict.size s - Dict.size stones)
    in
    test tc.name <|
        \_ ->
            Expect.equal tc.newStones newStones


playTest : Test
playTest =
    cases |> List.map playCase |> describe "play"
