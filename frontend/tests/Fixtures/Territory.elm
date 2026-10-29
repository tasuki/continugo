module Fixtures.Territory exposing (tests)

import Dict
import Expect
import Fixtures.Generated as Generated
import Rules.Position exposing (Color(..), stoneKey, stonesFromPlays)
import Rules.Territory as Territory exposing (Outcome(..))
import Sgf
import Test exposing (..)


checkOutcome : Territory.Areas -> Territory.Areas -> Expect.Expectation
checkOutcome expected actual =
    case ( expected.black == expected.white, Territory.result actual ) of
        ( True, Draw ) ->
            Expect.pass

        ( False, Win color margin ) ->
            Expect.all
                [ \_ ->
                    Expect.equal
                        (if expected.black > expected.white then
                            Black

                         else
                            White
                        )
                        color
                , \_ -> Expect.within (Expect.Absolute 0.00001) (abs (expected.black - expected.white)) margin
                ]
                ()

        _ ->
            Expect.fail "The outcome does not match the expected areas"


fixtureTest : Generated.TerritoryCase -> Test
fixtureTest fixture =
    test (fixture.filename ++ ": " ++ fixture.name) <|
        \_ ->
            case Sgf.decode fixture.stones of
                Err error ->
                    Expect.fail (fixture.filename ++ ": " ++ error)

                Ok plays ->
                    let
                        sites =
                            stonesFromPlays plays

                        stones =
                            List.map (\stone -> ( stoneKey stone, stone )) sites |> Dict.fromList

                        actual =
                            Territory.areas stones

                        expected =
                            Territory.Areas fixture.black fixture.white
                    in
                    if List.length sites /= List.length plays || Dict.size stones /= List.length sites then
                        Expect.fail "Scoring fixtures must contain unique placements, not passes"

                    else
                        Expect.all
                            [ \_ -> Expect.within (Expect.Absolute 0.00001) expected.black actual.black
                            , \_ -> Expect.within (Expect.Absolute 0.00001) expected.white actual.white
                            , \_ -> checkOutcome expected actual
                            ]
                            ()


tests : List Test
tests =
    Generated.territoryCases |> List.map fixtureTest
