module MovePreviewTests exposing (tests)

import DeadZones exposing (Point, canCover, geometry, libertyGeometry)
import Dict
import Expect
import Go exposing (..)
import MovePreview
import Play
import PlayTests
import Test exposing (..)


tests : Test
tests =
    describe "Move previews"
        [ test "No candidate leaves the position unpreviewed" <|
            \_ ->
                Expect.equal Nothing (MovePreview.fromMoves Dict.empty Nothing Nothing)
        , test "A hovered move occupies space and shows its group's liberties" <|
            \_ ->
                let
                    candidate =
                        createStone Black (Spot 500 500)
                in
                case MovePreview.fromMoves Dict.empty (Just candidate) Nothing of
                    Nothing ->
                        Expect.fail "Expected a legal preview"

                    Just preview ->
                        let
                            spots =
                                List.map .spot (stoneList preview.stones)
                        in
                        Expect.equal ( [ candidate.spot ], False, True )
                            ( preview.group
                            , canCover (geometry spots) (Point 500 500)
                            , canCover (libertyGeometry spots preview.group) (Point 600 500)
                            )
        , test "A staged move works without a hover, as after touch release" <|
            \_ ->
                let
                    candidate =
                        createStone Black (Spot 500 500)
                in
                Expect.equal (Just candidate.spot)
                    (MovePreview.fromMoves Dict.empty Nothing (Just candidate)
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
                    (MovePreview.fromMoves Dict.empty (Just hovered) (Just staged)
                        |> Maybe.map (.stones >> stoneList >> List.map .spot)
                    )
        , test "The preview highlights the whole connected group" <|
            \_ ->
                let
                    stones =
                        Play.playStones
                            [ createPlay Black (Spot 400 500)
                            , createPlay Black (Spot 500 500)
                            , createPlay White (Spot 700 500)
                            ]
                            Dict.empty
                in
                Expect.equal (Just [ Spot 400 500, Spot 500 500, Spot 600 500 ])
                    (MovePreview.fromMoves stones (Just (createStone Black (Spot 600 500))) Nothing
                        |> Maybe.map (.group >> List.sortBy .x)
                    )
        , test "An overlapping candidate is rejected rather than moved elsewhere" <|
            \_ ->
                let
                    stones =
                        Play.playStones [ createPlay Black (Spot 500 500) ] Dict.empty
                in
                Expect.equal Nothing
                    (MovePreview.fromMoves stones (Just (createStone White (Spot 510 500))) Nothing)
        , describe "Previewing captures and suicide uses the actual move rules"
            (List.map previewCase PlayTests.cases)
        ]


previewCase : PlayTests.Case -> Test
previewCase tc =
    test tc.name <|
        \_ ->
            let
                stones =
                    Play.playStones tc.record Dict.empty

                expected =
                    Play.playIfLegal stones tc.playStone

                check preview =
                    let
                        spots =
                            List.map .spot (stoneList preview.stones)

                        shape =
                            geometry spots

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
            case MovePreview.fromMoves stones (Just tc.playStone) Nothing of
                Nothing ->
                    Expect.equal Nothing expected

                Just preview ->
                    check preview
