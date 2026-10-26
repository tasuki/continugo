module PointerInput exposing (Action(..), Event(..), Pointer, State, cancel, empty, events, update)

{-| Turns browser pointer events into start, move, finish, or cancel actions.
The pressed pointer is tracked independently of the game's drag state.

The input path is update → coordinate conversion → Main.handleBoardPointer,
which handles previews, staging, placement, and analysis without distinguishing
touch from mouse. A revision check discards lookups from cancelled gestures.

Mouse/pen can hover; a finger only moves the preview while pressed. Only a
primary press starts a gesture. A second finger cancels it, and lifting a
finger never resumes it. Browser cancellation or leaving the board container
also cancels the press.

CSS `touch-action: pinch-zoom` leaves zooming to the browser without allowing
single-finger page panning. No separate touch/mouse listeners or preventDefault
calls are needed.

-}

import DocumentCoords exposing (DocumentCoords)
import Html exposing (Attribute)
import Html.Events as Events
import Json.Decode as Decode


type alias State =
    { pressed : Maybe Int
    , revision : Int
    }


empty : State
empty =
    { pressed = Nothing, revision = 0 }


{-| The revision also invalidates coordinate lookups still in flight in Main.
-}
cancel : State -> State
cancel state =
    { pressed = Nothing, revision = state.revision + 1 }


type alias Pointer =
    { id : Int
    , primary : Bool
    , button : Int
    , hovering : Bool
    , coords : DocumentCoords
    }


type Event
    = Down Pointer
    | Move Pointer
    | Up Pointer
    | Cancel Int
    | Leave Int


type Action
    = StartAt DocumentCoords
    | MoveAt DocumentCoords
    | FinishAt DocumentCoords
    | CancelGesture


update : Event -> State -> ( State, Maybe Action )
update event state =
    case event of
        Down pointer ->
            if pointer.button /= 0 then
                ( state, Nothing )

            else if not pointer.primary || state.pressed /= Nothing then
                ( cancel state, Just CancelGesture )

            else
                ( { state | pressed = Just pointer.id }, Just (StartAt pointer.coords) )

        Move pointer ->
            if state.pressed == Just pointer.id then
                ( state, Just (MoveAt pointer.coords) )

            else if state.pressed == Nothing && pointer.primary && pointer.hovering then
                ( state, Just (MoveAt pointer.coords) )

            else
                ( state, Nothing )

        Up pointer ->
            if state.pressed == Just pointer.id && pointer.button == 0 then
                ( { state | pressed = Nothing }, Just (FinishAt pointer.coords) )

            else
                ( state, Nothing )

        Cancel id ->
            cancelPressed id state

        Leave id ->
            cancelPressed id state


cancelPressed : Int -> State -> ( State, Maybe Action )
cancelPressed id state =
    if state.pressed == Just id then
        ( cancel state, Just CancelGesture )

    else
        ( state, Nothing )



-- Browser boundary: no touch/mouse listeners and no preventDefault.
-- CSS touch-action decides which gestures belong to the browser.


events : (Event -> msg) -> List (Attribute msg)
events toMsg =
    [ Events.on "pointerdown" (Decode.map (Down >> toMsg) pointerDecoder)
    , Events.on "pointermove" (Decode.map (Move >> toMsg) pointerDecoder)
    , Events.on "pointerup" (Decode.map (Up >> toMsg) pointerDecoder)
    , Events.on "pointercancel" (Decode.map (Cancel >> toMsg) idDecoder)
    , Events.on "pointerleave" (Decode.map (Leave >> toMsg) idDecoder)
    ]


idDecoder : Decode.Decoder Int
idDecoder =
    Decode.field "pointerId" Decode.int


pointerDecoder : Decode.Decoder Pointer
pointerDecoder =
    Decode.map5 Pointer
        idDecoder
        (Decode.field "isPrimary" Decode.bool)
        (Decode.field "button" Decode.int)
        (Decode.map2 (\kind buttons -> kind /= "touch" && buttons == 0)
            (Decode.field "pointerType" Decode.string)
            (Decode.field "buttons" Decode.int)
        )
        (Decode.map2 DocumentCoords
            (Decode.field "pageX" Decode.float)
            (Decode.field "pageY" Decode.float)
        )
