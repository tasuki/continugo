module ReplayTests exposing (tests)

import Dict
import Expect
import Fixtures.Play as PlayFixtures
import Html.Attributes as HA
import Json.Decode as Decode
import Json.Encode as Encode
import Main
import Rules.Board as Board
import Rules.Game as Game
import Rules.Position exposing (..)
import Sgf
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Url


tests : Test
tests =
    describe "Replay"
        [ test "Left/right arrows replay; other keys are ignored" <|
            \_ ->
                [ "ArrowLeft", "ArrowRight", "a" ]
                    |> List.map
                        (\key ->
                            Decode.decodeValue Main.replayKey (Encode.object [ ( "key", Encode.string key ) ])
                                |> Result.toMaybe
                        )
                    |> Expect.equal [ Just Main.Prev, Just Main.Next, Nothing ]
        , test "Stepping is bounded by the start and end, including passes" <|
            \_ ->
                let
                    record =
                        [ createPass Black, createPass White ]
                in
                Expect.equal
                    [ [], [], [ createPass Black ], List.reverse record, List.reverse record ]
                    (List.map (\move -> (Main.gameAt Board.default move record).record) [ -1, 0, 1, 2, 3 ])
        , test "Moving across a capture reconstructs the stones and turn" <|
            \_ ->
                PlayFixtures.withCase "capture-four.txt" <|
                    \tc ->
                        let
                            record =
                                tc.record ++ [ createPlay tc.playStone.color tc.playStone.spot ]

                            before =
                                Main.gameAt tc.board (List.length record - 1) record

                            after =
                                Main.gameAt tc.board (List.length record) record
                        in
                        Expect.equal ( -3, White, False )
                            ( Dict.size after.stones - Dict.size before.stones
                            , after.onMove
                            , Game.isFinished after
                            )
        , test "Download includes all moves and root metadata" <|
            \_ ->
                let
                    record =
                        { root = Sgf.empty.root ++ [ ( "RE", [ "B+2.5" ] ) ]
                        , plays = [ createPlay Black (Spot 200 200), createPass White ]
                        }
                in
                Main.viewDownload record.root record.plays
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.has
                        [ Selector.attribute (HA.download "continugo.sgf")
                        , Selector.attribute (HA.href ("data:application/x-go-sgf;charset=utf-8," ++ Url.percentEncode (Sgf.encodeRecord record)))
                        ]
        ]
