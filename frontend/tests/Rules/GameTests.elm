module Rules.GameTests exposing (tests)

import Dict
import Expect
import Fixtures.Play as PlayFixtures
import Rules.Game as Game
import Rules.Play as Play
import Rules.Position exposing (..)
import Test exposing (..)


tests : Test
tests =
    describe "Game progression"
        [ test "A new game starts empty with Black to play" <|
            \_ ->
                let
                    game =
                        Game.empty
                in
                Expect.equal ( Black, [], False )
                    ( game.onMove, Dict.values game.stones, Game.isFinished game )
        , test "Placements and passes advance turns and record newest first" <|
            \_ ->
                Game.play (Spot 200 200) Game.empty
                    |> Maybe.andThen Game.pass
                    |> Maybe.andThen (Game.play (Spot 400 400))
                    |> Maybe.map
                        (\game ->
                            ( game.onMove
                            , List.map .color game.record
                            , Dict.size game.stones
                            )
                        )
                    |> Expect.equal (Just ( White, [ Black, White, Black ], 2 ))
        , test "Exact placement rejects an overlap instead of adjusting it" <|
            \_ ->
                Game.play (Spot 200 200) Game.empty
                    |> Maybe.andThen (Game.play (Spot 250 200))
                    |> Expect.equal Nothing
        , test "An out-of-board placement is rejected" <|
            \_ ->
                Expect.equal Nothing (Game.play (Spot 0 0) Game.empty)
        , test "Two passes finish the game and prevent further placements or passes" <|
            \_ ->
                Game.pass Game.empty
                    |> Maybe.andThen Game.pass
                    |> Maybe.map
                        (\game ->
                            ( Game.isFinished game
                            , Game.play (Spot 500 500) game
                            , Game.pass game
                            )
                        )
                    |> Expect.equal (Just ( True, Nothing, Nothing ))
        , test "A placement interrupts consecutive passes" <|
            \_ ->
                Game.pass Game.empty
                    |> Maybe.andThen (Game.play (Spot 500 500))
                    |> Maybe.andThen Game.pass
                    |> Maybe.map Game.isFinished
                    |> Expect.equal (Just False)
        , test "A capture updates the game and survives history restoration" <|
            \_ ->
                PlayFixtures.withCase "capture-four.txt" <|
                    \tc ->
                        let
                            before =
                                Game.fromRecord (List.reverse tc.record)
                        in
                        Game.play tc.playStone.spot before
                            |> Maybe.map
                                (\game ->
                                    ( Dict.size game.stones - Dict.size before.stones
                                    , game.onMove
                                    , Game.fromRecord game.record == game
                                    )
                                )
                            |> Expect.equal (Just ( -3, White, True ))
        , test "Stepping back over passes restores the turn and finished state" <|
            \_ ->
                let
                    finished =
                        Game.fromRecord [ createPass White, createPass Black ]

                    previous =
                        Game.previous finished
                in
                Expect.equal ( False, White, Game.empty )
                    ( Game.isFinished previous, previous.onMove, Game.previous previous )
        , test "Replaying records still skips illegal stones and allows setup or post-finish moves" <|
            \_ ->
                let
                    record =
                        [ createPass Black
                        , createPass White
                        , createPlay White (Spot 500 500)
                        , createPlay Black (Spot 500 500)
                        , createPlay White (Spot 0 0)
                        ]

                    game =
                        List.foldl Game.replay Game.empty record
                in
                Expect.equal ( 1, Black, List.reverse record )
                    ( Dict.size game.stones, game.onMove, game.record )
        , test "Playing after stepping back keeps only the new branch" <|
            \_ ->
                let
                    before =
                        Game.fromRecord [ createPlay Black (Spot 200 200) ]

                    rewound =
                        Game.replay (createPlay White (Spot 400 400)) before |> Game.previous
                in
                Game.play (Spot 600 600) rewound
                    |> Maybe.map (\game -> ( game.record, Game.previous game == before ))
                    |> Expect.equal (Just ( [ createPlay White (Spot 600 600), createPlay Black (Spot 200 200) ], True ))
        , describe "Cached positions restore captures and illegal recorded moves"
            (PlayFixtures.tests <|
                \tc ->
                    let
                        before =
                            Game.fromRecordOn tc.board (List.reverse tc.record)

                        recorded =
                            { color = tc.playStone.color, move = Place tc.playStone }

                        after =
                            Game.replay recorded before
                    in
                    Expect.all
                        [ \_ -> Expect.equal before (Game.previous after)
                        , \_ -> Expect.equal (Play.replayPlacementsOn tc.board [ recorded ] before.stones) after.stones
                        ]
                        ()
            )
        , test "Record restoration preserves setup-like moves and skips illegal stones" <|
            \_ ->
                let
                    record =
                        [ createPlay White (Spot 0 0)
                        , createPlay Black (Spot 400 400)
                        , createPlay Black (Spot 200 200)
                        ]

                    restored =
                        Game.fromRecord record
                in
                Expect.equal ( record, Black, 2 )
                    ( restored.record, restored.onMove, Dict.size restored.stones )
        ]
