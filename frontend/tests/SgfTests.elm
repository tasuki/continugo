module SgfTests exposing (..)

import Expect
import Rules.Position exposing (..)
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
    , ( [ createPlay Black <| Spot 560 314
        , createPlay White <| Spot 561 234 -- up
        , createPlay White <| Spot 635 331 -- lower right
        , createPlay White <| Spot 653 415 -- lower right further
        , createPlay White <| Spot 505 368 -- lower left
        , createPlay White <| Spot 543 455 -- lower left further
        , createPass Black
        ]
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
                \_ -> Expect.equal (Ok decoded) (Sgf.decode encoded)


decodeTest =
    describe "decode"
        [ cases |> List.map canDecode |> describe "valid records"
        , [ "invalid"
          , "invalid;B[bbbb]"
          , "B[bbbb];invalid"
          , "B[bbbb];invalid;W[mmmm]"
          , "B[bb]"
          , "X[bbbb]"
          , "B[bb1b]"
          , " B[bbbb]"
          , "B[bbbb]\n"
          , ";B[bbbb]"
          , "B[bbbb];"
          , "B[bbbb];;W[mmmm]"
          ]
            |> List.map
                (\record ->
                    test ("Rejects " ++ record) <|
                        \_ ->
                            Sgf.decode record |> Expect.err
                )
            |> describe "invalid records"
        , test "Reports the malformed entry and its one-based index" <|
            \_ ->
                Expect.equal (Err "Invalid SGF entry 2: invalid")
                    (Sgf.decode "B[bbbb];invalid;W[mmmm]")
        ]
