module PointerInputTests exposing (tests)

import Expect
import PointerInput exposing (Action(..), Event(..), Pointer)
import Test exposing (Test, describe, test)


finger : Int -> Bool -> Pointer
finger id primary =
    { id = id, primary = primary, button = 0, hovering = False, coords = { x = 100, y = 200 } }


run : List Event -> ( PointerInput.State, List Action )
run events =
    List.foldl
        (\event ( state, actions ) ->
            let
                ( next, action ) =
                    PointerInput.update event state
            in
            ( next, actions ++ Maybe.withDefault [] (Maybe.map List.singleton action) )
        )
        ( PointerInput.empty, [] )
        events


tests : Test
tests =
    describe "Pointer input"
        [ test "Mouse/pen can hover; unpressed touch and right clicks do nothing" <|
            \_ ->
                let
                    touch =
                        finger 1 True

                    mouse =
                        { touch | hovering = True }
                in
                run [ Move touch, Up touch, Move mouse, Down { mouse | button = 2 }, Up { mouse | button = 2 } ]
                    |> Tuple.second
                    |> Expect.equal [ MoveAt mouse.coords ]
        , test "A press, drag and release produces one gesture" <|
            \_ ->
                let
                    touch =
                        finger 1 True
                in
                run [ Down touch, Move touch, Up touch, Up touch, Move touch ]
                    |> Tuple.second
                    |> Expect.equal [ StartAt touch.coords, MoveAt touch.coords, FinishAt touch.coords ]
        , test "A second finger cancels; neither remaining finger can resume the gesture" <|
            \_ ->
                let
                    first =
                        finger 1 True

                    second =
                        finger 2 False

                    next =
                        finger 3 True
                in
                run [ Down first, Down second, Move first, Up first, Move second, Up second, Down next, Up next ]
                    |> Tuple.second
                    |> Expect.equal [ StartAt first.coords, CancelGesture, StartAt next.coords, FinishAt next.coords ]
        , test "Browser cancellation or leaving cancels the press and invalidates pending coordinates" <|
            \_ ->
                let
                    touch =
                        finger 1 True

                    cancelled event =
                        run [ Down touch, event, Move touch, Up touch ]
                            |> (\( state, actions ) -> ( state.pressed, state.revision, actions ))

                    expected =
                        ( Nothing, 1, [ StartAt touch.coords, CancelGesture ] )
                in
                Expect.equal
                    ( expected, expected )
                    ( cancelled (Cancel touch.id), cancelled (Leave touch.id) )
        ]
