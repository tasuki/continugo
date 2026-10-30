module SgfTests exposing (..)

import Expect
import RecordUrl
import Rules.Position exposing (..)
import Sgf
import Test exposing (..)
import Url


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
                \_ -> Expect.equal (document encoded) (Sgf.encode decoded)


encodeTest =
    cases |> List.map canEncode |> describe "encode"


canDecode : Case -> Test
canDecode testCase =
    case testCase of
        ( decoded, encoded, comment ) ->
            test ("Can decode " ++ comment) <|
                \_ ->
                    Expect.equal [ Ok decoded, Ok decoded ]
                        [ Sgf.decode encoded, Sgf.decode (document encoded) ]


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
          , "(;B[bbbb])"
          , "(;FF[4]GM[1];B[bbbb])"
          , "(;FF[9071]GM[9071]B[bbbb])"
          , "(;FF[9071]GM[9071];B[bbbb]W[])"
          , "(;FF[9071]GM[9071];B[bbbb][mmmm])"
          , "(;FF[9071]GM[9071];B[bbbb](;W[]))"
          , "(;FF[9071]GM[9071])garbage"
          , "(;FF[9071]GM[9071]PB[unfinished)"
          , "(;FF[9071]GM[9071]PB[a]PB[b])"
          , "(;FF[9071]GM[9071];)"
          , "(;FF[9071]GM[9071]"
          , "(;FF[9071]GM[9071])()"
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
        , test "Whitespace outside properties and escaped line continuations are accepted" <|
            \_ ->
                Sgf.decodeRecord " \n(; GM[9071] FF[9071] PB[a\\\u{000D}\nb] ; B [bbbb] ; W [] ) \n"
                    |> Result.map (\record -> ( record.root, record.plays ))
                    |> Expect.equal
                        (Ok
                            ( [ ( "GM", [ "9071" ] ), ( "FF", [ "9071" ] ), ( "PB", [ "ab" ] ) ]
                            , [ createPlay Black (Spot 53 53), createPass White ]
                            )
                        )
        ]


document : String -> String
document compact =
    "(;FF[9071]GM[9071]SZ[13]"
        ++ (if String.isEmpty compact then
                ""

            else
                "\n;" ++ String.replace ";" "\n;" compact
           )
        ++ "\n)"


metadataTests : Test
metadataTests =
    let
        sgf =
            "(;FF[9071]GM[9071]SZ[19]PB[A\\]lice]PW[白\\\\黒]RE[B+2.5]C[a; (b)\nline]XX[one][two]\n;B[bbbb]\n;W[]\n)"

        expected =
            { root =
                [ ( "FF", [ "9071" ] )
                , ( "GM", [ "9071" ] )
                , ( "SZ", [ "19" ] )
                , ( "PB", [ "A]lice" ] )
                , ( "PW", [ "白\\黒" ] )
                , ( "RE", [ "B+2.5" ] )
                , ( "C", [ "a; (b)\nline" ] )
                , ( "XX", [ "one", "two" ] )
                ]
            , plays = [ createPlay Black (Spot 53 53), createPass White ]
            }
    in
    describe "Root metadata"
        [ test "Empty player names and result are omitted, but other empty properties remain" <|
            \_ ->
                Sgf.decodeRecord "(;FF[9071]GM[9071]SZ[13]PB[]PW[]RE[]C[])"
                    |> Result.map Sgf.encodeRecord
                    |> Expect.equal (Ok "(;FF[9071]GM[9071]SZ[13]C[]\n)")
        , test "Names, size, result, unknown properties and escaped text decode" <|
            \_ -> Expect.equal (Ok expected) (Sgf.decodeRecord sgf)
        , test "Metadata encodes with SGF escaping" <|
            \_ -> Expect.equal sgf (Sgf.encodeRecord expected)
        , test "URL encoding preserves newlines, Unicode, punctuation and result plus signs" <|
            \_ ->
                Url.fromString ("https://example.com/" ++ RecordUrl.encode expected)
                    |> Maybe.andThen RecordUrl.decode
                    |> Expect.equal (Just expected)
        , test "Old unescaped compact URLs still load" <|
            \_ ->
                Url.fromString "https://example.com/?record=B[bbbb];W[]"
                    |> Maybe.andThen RecordUrl.decode
                    |> Maybe.map .plays
                    |> Expect.equal (Just expected.plays)
        ]
