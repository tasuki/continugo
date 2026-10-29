module Fixtures.Play exposing (Case, decode, decodeRecord, tests, testsOn, withCase)

{-| The generator parses fixture fields; Elm interprets the SGF strings.
Records are chronological and may include setup-like same-colour sequences.
Fixtures default to the 13x13 equivalent unless they specify a board size.
-}

import Expect exposing (Expectation)
import Fixtures.Generated as Generated
import Rules.Board as Board exposing (Board)
import Rules.Position exposing (..)
import Sgf
import Test exposing (Test)


type alias Case =
    { name : String
    , board : Board
    , record : List Play
    , playStone : Stone
    , stoneCountChange : Maybe Int
    }


decode : Generated.Case -> Result String Case
decode fixture =
    case ( Board.fromSize fixture.boardSize, Sgf.decode fixture.record, Sgf.decodePlay fixture.move |> Maybe.map .move ) of
        ( Just board, Ok record, Just (Place stone) ) ->
            Ok (Case fixture.name board record stone fixture.stoneCountChange)

        ( Nothing, _, _ ) ->
            Err ("Invalid board size: " ++ String.fromInt fixture.boardSize)

        ( _, Err error, _ ) ->
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
tests =
    testsFrom Generated.cases


testsOn : Board -> (Case -> Expectation) -> List Test
testsOn board =
    testsFrom (List.filter (.boardSize >> (==) (Board.size board)) Generated.cases)


testsFrom : List Generated.Case -> (Case -> Expectation) -> List Test
testsFrom fixtures check =
    fixtures
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
