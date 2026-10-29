module Fixtures.Play exposing (Case, decode, decodeRecord, tests, withCase)

{-| The generator parses fixture fields; Elm interprets the SGF strings.
Records are chronological and may include setup-like same-colour sequences.
All cases currently use the 1000-unit board and radius 38 (13x13 equivalent).
-}

import Expect exposing (Expectation)
import Fixtures.Generated as Generated
import Rules.Position exposing (..)
import Sgf
import Test exposing (Test)


type alias Case =
    { name : String
    , record : List Play
    , playStone : Stone
    , stoneCountChange : Maybe Int
    }


decode : Generated.Case -> Result String Case
decode fixture =
    case ( Sgf.decode fixture.record, Sgf.decodePlay fixture.move |> Maybe.map .move ) of
        ( Ok record, Just (Place stone) ) ->
            Ok (Case fixture.name record stone fixture.stoneCountChange)

        ( Err error, _ ) ->
            Err error

        _ ->
            Err ("Expected a placement, got: " ++ fixture.move)


checkCase : (Case -> Expectation) -> Generated.Case -> Expectation
checkCase check fixture =
    case decode fixture of
        Ok tc ->
            check tc

        Err error ->
            Expect.fail (fixture.filename ++ ": " ++ error)


tests : (Case -> Expectation) -> List Test
tests check =
    Generated.cases
        |> List.map
            (\fixture ->
                Test.test (fixture.filename ++ ": " ++ fixture.name) (\_ -> checkCase check fixture)
            )


withCase : String -> (Case -> Expectation) -> Expectation
withCase filename check =
    case List.filter (.filename >> (==) filename) Generated.cases of
        [ fixture ] ->
            checkCase check fixture

        _ ->
            Expect.fail ("Missing or duplicate fixture: " ++ filename)


decodeRecord : String -> List Play
decodeRecord str =
    case Sgf.decode str of
        Ok record ->
            record

        Err error ->
            Debug.todo error
