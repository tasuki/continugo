module SgfTests exposing (..)

import Expect
import Go exposing (..)
import SamplePositions
import Sgf
import Test exposing (..)


type alias Case =
    ( List Play, String, String )


cases : List Case
cases =
    [ ( [], "", "an empty list" )
    , ( [ createPlay Black { x = 100, y = 200 }
        , createPlay White { x = 300, y = 400 }
        ]
      , "B[bWdS];W[fOhK]"
      , "a position with round coordinates"
      )
    , ( [ createPlay Black { x = 53, y = 53 }
        , createPlay White { x = 636, y = 636 }
        ]
      , "B[bbbb];W[mmmm]"
      , "a position with round SGF identifiers"
      )
    , ( SamplePositions.basicTest
      , "B[kOgc];W[kPeA];W[mlgt];W[mDhZ];W[jLhe];W[kxiN];B[]"
      , "basic test"
      )
    ]


canEncode : Case -> Test
canEncode testCase =
    case testCase of
        ( decoded, encoded, comment ) ->
            test ("Can encode " ++ comment) <|
                \_ -> Expect.equal (Sgf.encode decoded) encoded


encodeTest =
    cases |> List.map canEncode |> describe "encode"


canDecode : Case -> Test
canDecode testCase =
    case testCase of
        ( decoded, encoded, comment ) ->
            test ("Can decode " ++ comment) <|
                \_ -> Expect.equal (Sgf.decode encoded) decoded


decodeTest =
    cases |> List.map canDecode |> describe "decode"
