module DecodersTests exposing (..)

import Expect
import Json.Decode as D
import Main exposing (WindowCoords)
import Test exposing (..)


type alias Case a =
    { name : String
    , json : String
    , decoder : D.Decoder a
    , decodesTo : Result String a
    }


cases : List (Case WindowCoords)
cases =
    [ { name = "Decodes touch"
      , json = """{ "touches": { "0": { "pageX": 2.71828, "pageY": 3.141592 }, "length": 1 } }"""
      , decoder = Main.decodeTouch identity
      , decodesTo = Ok { x = 2.71828, y = 3.141592 }
      }
    , { name = "Does not decode when nothing touches"
      , json = """{ "touches": { "length": 0 } }"""
      , decoder = Main.decodeTouch identity
      , decodesTo = Err "No touch point"
      }
    , { name = "Does not decode when there are several touches"
      , json = """{ "touches": { "0": { "pageX": 1, "pageY": 1 }, "1": { "pageX": 2, "pageY": 2 }, "length": 2 } }"""
      , decoder = Main.decodeTouch identity
      , decodesTo = Err "More than one touch point"
      }
    , { name = "Decodes changed touch"
      , json = """{ "changedTouches": { "0": { "pageX": 2.71828, "pageY": 3.141592 }, "length": 1 } }"""
      , decoder = Main.decodeChangedTouch identity
      , decodesTo = Ok { x = 2.71828, y = 3.141592 }
      }
    ]


decodeCase : Case WindowCoords -> Test
decodeCase tc =
    test tc.name <|
        \_ ->
            let
                decoded : Result String WindowCoords
                decoded =
                    D.decodeString tc.decoder tc.json
                        |> Result.mapError D.errorToString
            in
            case ( tc.decodesTo, decoded ) of
                ( Ok expected, Ok actual ) ->
                    Expect.equal expected actual

                ( Err expected, Err actual ) ->
                    if String.contains expected actual then
                        Expect.pass

                    else
                        Expect.fail <|
                            "Expected '"
                                ++ expected
                                ++ "', but not found in: '"
                                ++ actual
                                ++ "'"

                ( Ok _, Err actual ) ->
                    Expect.fail actual

                ( Err expected, Ok actual ) ->
                    Expect.fail <|
                        "Expected to fail with '"
                            ++ expected
                            ++ "', but succeeded..."


decodeTest : Test
decodeTest =
    cases |> List.map decodeCase |> describe "decode window coords"
