module DecodersTests exposing (..)

import Expect
import Json.Decode as D
import Main exposing (Msg(..))
import Test exposing (..)


type alias Case c =
    { name : String
    , json : String
    , decoder : D.Decoder c
    , decodesTo : Result String c
    }


cases : List (Case Main.Msg)
cases =
    [ { name = "Decodes touch"
      , json = """{ "touches": { "0": { "pageX": 2.71828, "pageY": 3.141592 }, "length": 1 } }"""
      , decoder = Main.decodeTouch Main.Moved
      , decodesTo = Ok <| Main.Moved { x = 2.71828, y = 3.141592, source = Main.Touch }
      }
    , { name = "Decodes changed touch"
      , json = """{ "changedTouches": { "0": { "pageX": 2.71828, "pageY": 3.141592 }, "length": 1 } }"""
      , decoder = Main.decodeChangedTouch Main.Moved
      , decodesTo = Ok <| Main.Moved { x = 2.71828, y = 3.141592, source = Main.Touch }
      }
    ]


preventDefaultCases : List (Case ( Main.Msg, Bool ))
preventDefaultCases =
    [ { name = "Decodes single touch and prevents default"
      , json = """{ "touches": { "0": { "pageX": 2.71828, "pageY": 3.141592 }, "length": 1 } }"""
      , decoder = Main.decodeSingleTouch Main.Moved
      , decodesTo = Ok <| ( Main.Moved { x = 2.71828, y = 3.141592, source = Main.Touch }, True )
      }
    , { name = "Decodes to clearing touch and not preventing default when there are several touches"
      , json = """{ "touches": { "0": { "pageX": 1, "pageY": 1 }, "1": { "pageX": 2, "pageY": 2 }, "length": 2 } }"""
      , decoder = Main.decodeSingleTouch Main.Moved
      , decodesTo = Ok <| ( Main.ClearTouch, False )
      }
    , { name = "Decodes to clearing touch and not preventing default when nothing touches"
      , json = """{ "touches": { "length": 0 } }"""
      , decoder = Main.decodeSingleTouch Main.Moved
      , decodesTo = Ok <| ( Main.ClearTouch, False )
      }
    ]


decodeCase : Case c -> Test
decodeCase tc =
    test tc.name <|
        \_ ->
            let
                decoded : Result String c
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

                ( Err expected, Ok _ ) ->
                    Expect.fail <|
                        "Expected to fail with '"
                            ++ expected
                            ++ "', but succeeded..."


decodeTest : Test
decodeTest =
    cases |> List.map decodeCase |> describe "decode window coords"


decodePreventDefaultTest : Test
decodePreventDefaultTest =
    preventDefaultCases |> List.map decodeCase |> describe "decode window coords with preventDefault"
