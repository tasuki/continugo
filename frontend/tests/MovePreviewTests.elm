module MovePreviewTests exposing (tests)

import Coverage exposing (Point, canCover)
import Dict
import Expect
import Fixtures.Play as PlayFixtures
import MovePreview
import Rules.Board as Board
import Rules.Game as Game
import Rules.Play as Play
import Rules.Position exposing (..)
import Test exposing (..)


tests : Test
tests =
    describe "Move previews"
        [ test "No candidate leaves the position unpreviewed" <|
            \_ ->
                Expect.equal Nothing (MovePreview.fromCandidates Board.default Dict.empty Nothing Nothing)
        , test "A hovered move occupies space and shows its group's liberties" <|
            \_ ->
                let
                    candidate =
                        createStone Black (Spot 500 500)
                in
                case MovePreview.fromCandidates Board.default Dict.empty (Just candidate) Nothing of
                    Nothing ->
                        Expect.fail "Expected a legal preview"

                    Just preview ->
                        let
                            spots =
                                List.map .spot (stoneList preview.stones)
                        in
                        Expect.equal ( [ candidate.spot ], False, True )
                            ( preview.group
                            , canCover (Coverage.geometry Board.default spots) (Point 500 500)
                            , canCover (Coverage.libertyGeometry Board.default spots preview.group) (Point 600 500)
                            )
        , test "A staged move works without a hover, as after touch release" <|
            \_ ->
                let
                    candidate =
                        createStone Black (Spot 500 500)
                in
                Expect.equal (Just candidate.spot)
                    (MovePreview.fromCandidates Board.default Dict.empty Nothing (Just candidate)
                        |> Maybe.map (.stone >> .spot)
                    )
        , test "Staging takes precedence over a different hovered stone" <|
            \_ ->
                let
                    hovered =
                        createStone Black (Spot 300 500)

                    staged =
                        createStone Black (Spot 700 500)
                in
                Expect.equal (Just [ staged.spot ])
                    (MovePreview.fromCandidates Board.default Dict.empty (Just hovered) (Just staged)
                        |> Maybe.map (.stones >> stoneList >> List.map .spot)
                    )
        , test "The preview highlights the whole connected group" <|
            \_ ->
                let
                    stones =
                        Play.replayPlacements
                            [ createPlay Black (Spot 400 500)
                            , createPlay Black (Spot 500 500)
                            , createPlay White (Spot 700 500)
                            ]
                            Dict.empty
                in
                Expect.equal (Just [ Spot 400 500, Spot 500 500, Spot 600 500 ])
                    (MovePreview.fromCandidates Board.default stones (Just (createStone Black (Spot 600 500))) Nothing
                        |> Maybe.map (.group >> List.sortBy .x)
                    )
        , test "An overlapping candidate is rejected rather than moved elsewhere" <|
            \_ ->
                let
                    stones =
                        Play.replayPlacements [ createPlay Black (Spot 500 500) ] Dict.empty
                in
                Expect.equal Nothing
                    (MovePreview.fromCandidates Board.default stones (Just (createStone White (Spot 510 500))) Nothing)
        , test "A ko-illegal recapture cannot be hovered or staged" <|
            \_ ->
                PlayFixtures.withCase "ko-shifted.txt" <|
                    \tc ->
                        let
                            game =
                                Game.fromRecordOn tc.board (List.reverse tc.record)

                            candidate =
                                Just tc.playStone
                        in
                        Expect.equal ( True, Nothing, Nothing )
                            ( MovePreview.fromCandidates game.board game.stones candidate Nothing /= Nothing
                            , MovePreview.forGame game candidate Nothing
                            , MovePreview.forGame game Nothing candidate
                            )
        , describe "Previewing captures, suicide and ko uses the actual move rules"
            (PlayFixtures.tests previewCase)
        ]


previewCase : PlayFixtures.Case -> Expect.Expectation
previewCase tc =
    let
        game =
            Game.fromRecordOn tc.board (List.reverse tc.record)

        stones =
            game.stones

        expected =
            Game.play tc.playStone.spot game |> Maybe.map .stones

        check preview =
            let
                spots =
                    List.map .spot (stoneList preview.stones)

                shape =
                    Coverage.geometry tc.board spots

                capturedOpenings =
                    List.all
                        (\stone -> canCover shape (Point (toFloat stone.spot.x) (toFloat stone.spot.y)))
                        preview.captured
            in
            Expect.all
                [ \_ -> Expect.equal expected (Just preview.stones)
                , \_ -> Expect.equal preview (MovePreview.fromResult stones ( preview.stones, preview.stone ))
                , \_ -> Expect.equal (removedStones stones preview.stones) preview.captured
                , \_ -> Expect.equal True capturedOpenings
                , \_ -> Expect.equal (Just preview.stone) (Dict.get (stoneKey preview.stone) preview.stones)
                , \_ -> Expect.equal (List.map .spot (Play.group preview.stones preview.stone)) preview.group
                ]
                ()
    in
    case MovePreview.forGame game (Just tc.playStone) Nothing of
        Nothing ->
            Expect.equal Nothing expected

        Just preview ->
            check preview
