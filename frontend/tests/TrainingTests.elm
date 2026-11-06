module TrainingTests exposing (tests)

import Expect
import Html.Attributes as HA
import Rules.Board as Board
import Rules.Position exposing (..)
import Sgf
import Svg.Attributes as SA
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Training


example : String
example =
    "(;FF[9071]GM[9071]SZ[9]C[root metadata]\n;B[hAjs]C[{\"win_pct\":49.9,\"score\":0.5,\"policy\":[[\"\",17\\],[\"jiiu\",8\\],[\"hAjs\",7\\]\\],\"train_policy\":true}]\n;W[]C[{\"win_pct\":null,\"score\":null}]XX[one][two]\n)"


tests : Test
tests =
    describe "Training annotations"
        [ test "Training comments and unknown move properties survive SGF round trips" <|
            \_ ->
                Sgf.decodeRecord example
                    |> Result.map Sgf.encodeRecord
                    |> Expect.equal (Ok example)
        , test "Escaped JSON policy pairs decode, including pass" <|
            \_ ->
                Sgf.decodeRecord example
                    |> Result.map (.moveProperties >> List.head >> Maybe.withDefault [] >> Training.annotation)
                    |> Expect.equal
                        (Ok
                            { winPct = Just 49.9
                            , score = Just 0.5
                            , policy = [ ( "", 17 ), ( "jiiu", 8 ), ( "hAjs", 7 ) ]
                            }
                        )
        , test "Absent, ordinary and null comments do not invent evaluations" <|
            \_ ->
                [ [], [ ( "C", [ "just a comment" ] ) ], [ ( "C", [ "{\"win_pct\":null,\"score\":null}" ] ) ] ]
                    |> List.map Training.annotation
                    |> Expect.equal [ Training.empty, Training.empty, Training.empty ]
        , test "Badges show zero values too, with the mover's perspective" <|
            \_ ->
                Training.viewEvaluation (Just White) { winPct = Just 0, score = Just -2.6, policy = [] }
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ Selector.id "training-win-pct" ] >> Query.has [ Selector.text "0%" ]
                        , Query.find [ Selector.id "training-score" ]
                            >> Query.has [ Selector.text "-2.6", Selector.attribute (HA.attribute "aria-label" "White expected score margin") ]
                        ]
        , test "Policy stones use board radius and visits, fading less-visited alternatives" <|
            \_ ->
                Training.viewPolicy Board.default Black [ ( "hAjs", 8 ), ( "jiiu", 4 ), ( "", 2 ), ( "invalid", 8 ) ]
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.findAll [ Selector.tag "circle" ] >> Query.count (Expect.equal 2)
                        , Query.findAll [ Selector.tag "circle" ]
                            >> Query.each (Query.has [ Selector.attribute (SA.r (String.fromInt (Board.radius Board.default))) ])
                        , Query.find [ Selector.attribute (SA.opacity "0.85") ] >> Query.has [ Selector.text "8" ]
                        , Query.find [ Selector.attribute (SA.opacity "0.425") ] >> Query.has [ Selector.text "4" ]
                        , Query.hasNot [ Selector.text "pass" ]
                        ]
        , test "Pass lives in the separated training menu, only while policy is shown" <|
            \_ ->
                Training.viewMenu () True White Training.empty [ ( "hAjs", 8 ), ( "", 2 ) ]
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.findAll [ Selector.class "menu-spacer" ] >> Query.count (Expect.equal 1)
                        , Query.find [ Selector.id "policy-pass" ]
                            >> Query.has [ Selector.text "pass", Selector.text "2", Selector.style "opacity" "0.2125" ]
                        , \_ ->
                            Training.viewMenu () False White Training.empty [ ( "", 2 ) ]
                                |> Query.fromHtml
                                |> Query.hasNot [ Selector.id "policy-pass" ]
                        ]
        , test "Policy toggle emits its action and exposes its state" <|
            \_ ->
                Training.viewToggle () True [ ( "", 17 ) ]
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "policy-toggle" ]
                    |> Expect.all
                        [ Query.has [ Selector.attribute (HA.attribute "aria-pressed" "true") ]
                        , Event.simulate Event.click >> Event.expect ()
                        ]
        , test "Unannotated games have no training badges or toggle" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        Training.viewEvaluation Nothing Training.empty
                            |> Query.fromHtml
                            |> Query.findAll [ Selector.class "icon" ]
                            |> Query.count (Expect.equal 0)
                    , \_ ->
                        Training.viewToggle () False []
                            |> Query.fromHtml
                            |> Query.hasNot [ Selector.id "policy-toggle" ]
                    ]
                    ()
        ]
