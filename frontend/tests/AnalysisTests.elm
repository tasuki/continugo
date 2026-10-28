module AnalysisTests exposing (tests)

import Analysis exposing (Effect(..))
import Coverage exposing (Point, canCover, geometry)
import Dict
import DragFrom
import Expect
import Fixtures.Play as PlayFixtures
import Fuzz
import Rules.Play as Play
import Rules.Position exposing (..)
import Test exposing (..)


withCircles : List Spot -> Analysis.Model
withCircles circles =
    let
        model =
            Analysis.empty
    in
    { model | circles = circles }


tap : Stones -> Spot -> Analysis.Model -> ( Analysis.Model, Effect )
tap stones spot model =
    Analysis.start stones spot model |> Analysis.finish stones spot


drag : Stones -> Spot -> Spot -> Analysis.Model -> Analysis.Model
drag stones from to model =
    Analysis.start stones from model
        |> Analysis.move stones to
        |> Analysis.finish stones to
        |> Tuple.first


tests : Test
tests =
    describe "Analysis scratch layer"
        [ test "A tap adds an unowned circle without changing the position" <|
            \_ ->
                Expect.equal ( [ Spot 500 500 ], NoEffect )
                    (tap Dict.empty (Spot 500 500) Analysis.empty |> Tuple.mapFirst .circles)
        , test "Visible circle occupancy includes the hover or drag preview" <|
            \_ ->
                Expect.equal [ Spot 500 500, Spot 600 500 ]
                    (Analysis.visibleCircles [ Spot 500 500 ] (Just (Spot 600 500)))
        , test "Circles and previews block deadzones until hidden or removed" <|
            \_ ->
                let
                    coverage circles preview =
                        geometry (Spot 300 500 :: Analysis.visibleCircles circles preview)
                            |> (\shape -> List.map (canCover shape) [ Point 300 500, Point 500 500, Point 600 500 ])
                in
                Expect.equal
                    [ [ False, False, False ], [ False, False, True ], [ False, True, True ] ]
                    [ coverage [ Spot 500 500 ] (Just (Spot 600 500))
                    , coverage [ Spot 500 500 ] Nothing
                    , coverage [] Nothing
                    ]
        , test "Hover previews a snapped circle without adding it" <|
            \_ ->
                let
                    model =
                        withCircles [ Spot 500 500 ]
                            |> Analysis.move Dict.empty (Spot 550 500)
                in
                Expect.equal ( Just (Spot 576 500), [ Spot 500 500 ], Nothing )
                    ( model.preview, model.circles, model.gesture )
        , test "Hover preview uses both colours and circles as blockers" <|
            \_ ->
                let
                    stones =
                        Play.replayPlacements [ createPlay Black (Spot 300 500), createPlay White (Spot 700 500) ] Dict.empty

                    model =
                        withCircles [ Spot 500 500 ]
                in
                [ Spot 350 500, Spot 550 500, Spot 650 500 ]
                    |> List.map (\spot -> Analysis.move stones spot model |> .preview)
                    |> Expect.equal [ Just (Spot 376 500), Just (Spot 576 500), Just (Spot 624 500) ]
        , test "Hover hides the preview over occupied objects, blocked spots, and outside the board" <|
            \_ ->
                let
                    stones =
                        Play.replayPlacements [ createPlay White (Spot 700 500) ] Dict.empty

                    model =
                        withCircles [ Spot 500 500 ] |> Analysis.move stones (Spot 400 400)
                in
                [ Spot 500 500, Spot 700 500, Spot 538 500, Spot -1 500, Spot 1001 500 ]
                    |> List.map (\spot -> Analysis.move stones spot model |> .preview)
                    |> Expect.equal (List.repeat 5 Nothing)
        , test "Leaving the board clears hover without cancelling pending circle removal" <|
            \_ ->
                let
                    model =
                        tap Dict.empty (Spot 500 500) (withCircles [ Spot 500 500 ])
                            |> Tuple.first
                            |> Analysis.move Dict.empty (Spot 600 500)
                            |> Analysis.clearHover
                in
                Expect.equal ( Nothing, Just ( 1, Spot 500 500 ) ) ( model.preview, model.pendingRemoval )
        , test "Hovering then releasing without a press cannot add a circle" <|
            \_ ->
                Analysis.move Dict.empty (Spot 500 500) Analysis.empty
                    |> Analysis.finish Dict.empty (Spot 500 500)
                    |> Tuple.first
                    |> .circles
                    |> Expect.equal []
        , test "Circles cannot overlap other circles, stones, or the board edge" <|
            \_ ->
                let
                    stones =
                        Play.replayPlacements [ createPlay White (Spot 700 500) ] Dict.empty

                    model =
                        withCircles [ Spot 500 500 ]
                in
                [ Spot 538 500, Spot 662 500, Spot -10 500 ]
                    |> List.map (\spot -> tap stones spot model |> Tuple.first |> .circles)
                    |> Expect.equal (List.repeat 3 model.circles)
        , test "Taps snap to the nearest fitting position around circles, stones, and board edges" <|
            \_ ->
                let
                    stones =
                        Play.replayPlacements [ createPlay White (Spot 700 500) ] Dict.empty

                    model =
                        withCircles [ Spot 500 500 ]
                in
                [ Spot 550 500, Spot 650 500, Spot 10 500 ]
                    |> List.map (\spot -> tap stones spot model |> Tuple.first |> .circles)
                    |> Expect.equal
                        [ [ Spot 500 500, Spot 576 500 ]
                        , [ Spot 500 500, Spot 624 500 ]
                        , [ Spot 500 500, Spot 38 500 ]
                        ]
        , test "Dragging from empty space previews and commits the same snapped circle" <|
            \_ ->
                let
                    preview =
                        withCircles [ Spot 500 500 ]
                            |> Analysis.start Dict.empty (Spot 300 300)
                            |> Analysis.move Dict.empty (Spot 550 500)

                    committed =
                        Analysis.finish Dict.empty (Spot 550 500) preview |> Tuple.first
                in
                Expect.equal ( Just (Spot 576 500), [ Spot 500 500, Spot 576 500 ] )
                    ( preview.preview, committed.circles )
        , test "Both colours and other circles get links, with no duplicates" <|
            \_ ->
                let
                    stones =
                        Play.replayPlacements
                            [ createPlay Black (Spot 400 500), createPlay White (Spot 600 500) ]
                            Dict.empty
                in
                Analysis.links stones [ Spot 500 500, Spot 500 600, Spot 800 800 ]
                    |> List.sortBy (\( a, b ) -> ( a.x, b.x, b.y ))
                    |> Expect.equal
                        [ ( Spot 500 500, Spot 400 500 )
                        , ( Spot 500 500, Spot 500 600 )
                        , ( Spot 500 500, Spot 600 500 )
                        ]
        , test "Dragging a circle creates another circle without moving the origin" <|
            \_ ->
                let
                    model =
                        drag Dict.empty (Spot 500 500) (Spot 900 500) (withCircles [ Spot 500 500 ])
                in
                Expect.equal [ Spot 500 500, Spot 607 500 ] model.circles
        , test "Dragging works from stones of either colour" <|
            \_ ->
                [ Black, White ]
                    |> List.map
                        (\color ->
                            let
                                stones =
                                    Play.replayPlacements [ createPlay color (Spot 500 500) ] Dict.empty
                            in
                            drag stones (Spot 500 500) (Spot 900 500) Analysis.empty |> .circles
                        )
                    |> Expect.equal (List.repeat 2 [ Spot 607 500 ])
        , test "Dragging avoids every circle and stone" <|
            \_ ->
                let
                    stones =
                        Play.replayPlacements [ createPlay White (Spot 600 570) ] Dict.empty

                    model =
                        drag stones (Spot 500 500) (Spot 900 500) (withCircles [ Spot 500 500, Spot 650 450 ])
                in
                case List.drop 2 model.circles of
                    [ added ] ->
                        Expect.equal True
                            (adjacent added (Spot 500 500)
                                && Analysis.fits [ Spot 500 500, Spot 650 450, Spot 600 570 ] added
                            )

                    _ ->
                        Expect.fail "Expected one fitted circle"
        , test "No placement is emitted when the dragged direction is blocked" <|
            \_ ->
                let
                    model =
                        withCircles [ Spot 500 500, Spot 576 500, Spot 424 500, Spot 500 576, Spot 500 424 ]
                in
                Expect.equal model.circles (drag Dict.empty (Spot 500 500) (Spot 900 500) model |> .circles)
        , fuzz2 (Fuzz.intRange -100 1100) (Fuzz.intRange -100 1100) "Dragged candidates always fit and stay connected" <|
            \x y ->
                case DragFrom.findDraggedSpot [ Spot 500 500, Spot 600 570 ] (Spot 500 500) (Spot x y) of
                    Nothing ->
                        Expect.pass

                    Just candidate ->
                        Expect.equal True
                            (adjacent candidate (Spot 500 500)
                                && Analysis.fits [ Spot 500 500, Spot 600 570 ] candidate
                            )
        , test "Small pointer jitter is a tap, not a drag" <|
            \_ ->
                Analysis.start Dict.empty (Spot 500 500) (withCircles [ Spot 500 500 ])
                    |> Analysis.finish Dict.empty (Spot 503 502)
                    |> Tuple.second
                    |> Expect.equal (ScheduleRemoval 1)
        , test "Single-tap removal waits for its timer" <|
            \_ ->
                let
                    ( model, effect ) =
                        tap Dict.empty (Spot 500 500) (withCircles [ Spot 500 500 ])
                in
                Expect.equal ( [ Spot 500 500 ], ScheduleRemoval 1, [] )
                    ( model.circles, effect, (Analysis.expire 1 model).circles )
        , test "Double-tap proposes the exact circle and cancels pending removal" <|
            \_ ->
                let
                    ( model, effect ) =
                        tap Dict.empty (Spot 500 500) (withCircles [ Spot 500 500 ])
                            |> Tuple.first
                            |> tap Dict.empty (Spot 502 501)
                in
                Expect.equal ( Propose (Spot 500 500), [ Spot 500 500 ] )
                    ( effect, (Analysis.expire 1 model).circles )
        , test "The removal timer cannot fire during the second press" <|
            \_ ->
                tap Dict.empty (Spot 500 500) (withCircles [ Spot 500 500 ])
                    |> Tuple.first
                    |> Analysis.start Dict.empty (Spot 500 500)
                    |> Analysis.expire 1
                    |> Analysis.finish Dict.empty (Spot 500 500)
                    |> Tuple.second
                    |> Expect.equal (Propose (Spot 500 500))
        , test "Dragging instead of a second tap extends the circle, not proposes or deletes it" <|
            \_ ->
                let
                    model =
                        tap Dict.empty (Spot 500 500) (withCircles [ Spot 500 500 ])
                            |> Tuple.first
                            |> drag Dict.empty (Spot 500 500) (Spot 900 500)
                            |> Analysis.expire 1
                in
                Expect.equal [ Spot 500 500, Spot 607 500 ] model.circles
        , test "Quickly tapping different circles removes both, rather than proposing either" <|
            \_ ->
                tap Dict.empty (Spot 400 500) (withCircles [ Spot 400 500, Spot 600 500 ])
                    |> Tuple.first
                    |> tap Dict.empty (Spot 600 500)
                    |> Tuple.first
                    |> Analysis.expire 2
                    |> .circles
                    |> Expect.equal []
        , test "Old timers cannot remove a circle tapped again later" <|
            \_ ->
                tap Dict.empty (Spot 500 500) (withCircles [ Spot 500 500 ])
                    |> Tuple.first
                    |> Analysis.cancel
                    |> tap Dict.empty (Spot 500 500)
                    |> Tuple.first
                    |> Analysis.expire 1
                    |> .circles
                    |> Expect.equal [ Spot 500 500 ]
        , test "Cancelling preserves annotations and cancels drag and removal" <|
            \_ ->
                let
                    model =
                        tap Dict.empty (Spot 500 500) (withCircles [ Spot 500 500 ])
                            |> Tuple.first
                            |> Analysis.cancel
                            |> Analysis.expire 1
                in
                Expect.equal ( [ Spot 500 500 ], ( Nothing, Nothing, Nothing ) )
                    ( model.circles, ( model.gesture, model.preview, model.pendingRemoval ) )
        , test "Actual moves discard only circles now occupied by stones" <|
            \_ ->
                let
                    stones =
                        Play.replayPlacements [ createPlay Black (Spot 500 500) ] Dict.empty
                in
                Analysis.reconcile stones (withCircles [ Spot 500 500, Spot 700 500 ])
                    |> .circles
                    |> Expect.equal [ Spot 700 500 ]
        , test "Analysis allows a physically fitting circle even when playing it would be suicide" <|
            \_ ->
                let
                    stones =
                        Play.replayPlacements PlayFixtures.suicideRecord Dict.empty

                    candidate =
                        PlayFixtures.stoneFromSgf "B[cIaS]"
                in
                Expect.equal ( Nothing, [ candidate.spot ] )
                    ( Play.playIfLegal stones candidate
                    , tap stones candidate.spot Analysis.empty |> Tuple.first |> .circles
                    )
        ]
