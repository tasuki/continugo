module Fixtures.Play exposing (Case, cases, connectedSuicideRecord, decodeRecord, longGameRecord, stoneFromSgf, suicideRecord)

import Rules.Position exposing (..)
import Sgf


decodeRecord : String -> List Play
decodeRecord str =
    case Sgf.decode str of
        Ok record ->
            record

        Err error ->
            Debug.todo error


stoneFromSgf : String -> Stone
stoneFromSgf str =
    case Sgf.decodePlay str |> Maybe.map .move of
        Just (Place stone) ->
            stone

        _ ->
            Debug.todo ("Invalid stone fixture: " ++ str)


longGameRecord : List Play
longGameRecord =
    decodeRecord <|
        String.concat
            [ "B[oifh];W[fgoc];B[odpz];W[fhfm];B[ihdL];W[lfem];B[ekpj];W[fsqe];"
            , "B[dWnk];W[eSmd];B[dGll];W[dsqt];B[fHkR];W[iogz];B[jAfl];W[kPgM];"
            , "B[gMfX];W[gUez];B[jnhQ];W[hIit];B[lLfD];W[mRgQ];B[mLdR];W[jPcR];"
            , "B[iHbT];W[hqcz];B[kJbI];W[mbcC];B[nrbT];W[hLaM];B[lZaM];W[jKjx];"
            , "B[lfiC];W[lCko];B[mVjo];W[nAkG];B[oQjC];W[pCkQ];B[cQim];W[qrmH];"
            , "B[pYii];W[qNju];B[regx];W[oshO];B[codi];W[dhfM];B[hNlD];W[pvgk]"
            ]


suicideRecord : List Play
suicideRecord =
    decodeRecord <|
        "B[aMcm];W[aMdK];B[cmco];W[ckdM];B[dKcm];W[eddI];B[eCaO];W[fmcj];W[gaaM];W[cIaS];B[aMaM]"


connectedSuicideRecord : List Play
connectedSuicideRecord =
    decodeRecord <|
        "B[aMcm];W[aMdK];B[cmco];W[ckdM];B[dKcm];W[eddI];B[eCaO];W[fmcj];W[gaaM];W[cIaS]"


type alias Case =
    { name : String
    , record : List Play
    , playStone : Stone
    , stoneCountChange : Maybe Int
    }


cases : List Case
cases =
    [ { name = "Add a move"
      , record = []
      , playStone = stoneFromSgf "B[boob]"
      , stoneCountChange = Just 1
      }
    , { name = "Don't add an illegal move"
      , record = []
      , playStone = stoneFromSgf "B[butt]"
      , stoneCountChange = Nothing
      }
    , { name = "Capture a group of four stones"
      , record = longGameRecord
      , playStone = stoneFromSgf "B[qheG]"
      , stoneCountChange = Just -3
      }
    , { name = "Disallow suicide"
      , record = suicideRecord
      , playStone = stoneFromSgf "B[cIaS]"
      , stoneCountChange = Nothing
      }
    , { name = "Allow a placement that gains liberties through capture"
      , record = suicideRecord
      , playStone = stoneFromSgf "W[cIaS]"
      , stoneCountChange = Just -4
      }
    , { name = "Allow capturing a one-eyed group with a stone inside"
      , record = connectedSuicideRecord
      , playStone = stoneFromSgf "W[aMaM]"
      , stoneCountChange = Just -3
      }
    , { name = "Allow one-eyed group to survive a little longer by taking"
      , record = connectedSuicideRecord
      , playStone = stoneFromSgf "B[aMaM]"
      , stoneCountChange = Just 0
      }
    ]
