module Rules.Game exposing (Game, empty, fromRecord, fromRecordOn, isFinished, onBoard, pass, play, previous, replay)

{-| Rules depend only on elm/core and other Rules modules. The frontend owns
rendering, gestures, placement adjustment, and record serialization.

Game handles normal turn progression and ko; Play handles history-free placements
on arbitrary positions. Records are stored newest first, matching the frontend's
existing history convention. Previous positions share immutable stone maps, so
stepping back does not recalculate captures.

-}

import Dict
import Rules.Board as Board exposing (Board)
import Rules.Play as Play
import Rules.Position exposing (..)


type alias Game =
    { board : Board
    , record : List Play
    , onMove : Color
    , stones : Stones
    , history : List Stones
    }


empty : Game
empty =
    onBoard Board.default


onBoard : Board -> Game
onBoard board =
    { board = board
    , record = []
    , onMove = Black
    , stones = Dict.empty
    , history = []
    }


{-| Replay a record, accepting setup-like sequences of same-colour moves.
Illegal placements are skipped on the board but retained in the record.
This is not a validator for externally submitted game histories.
-}
fromRecord : List Play -> Game
fromRecord =
    fromRecordOn Board.default


fromRecordOn : Board -> List Play -> Game
fromRecordOn board record =
    List.foldl replay (onBoard board) (List.reverse record)


{-| Apply one recorded move with the same permissive semantics as fromRecordOn.
-}
replay : Play -> Game -> Game
replay recorded game =
    let
        stones =
            case recorded.move of
                Place stone ->
                    playStone stone game
                        |> Maybe.withDefault game.stones

                Pass ->
                    game.stones
    in
    advance recorded stones game


previous : Game -> Game
previous game =
    case ( game.record, game.history ) of
        ( _ :: record, stones :: history ) ->
            { game
                | record = record
                , stones = stones
                , history = history
                , onMove =
                    List.head record
                        |> Maybe.map (.color >> otherColor)
                        |> Maybe.withDefault Black
            }

        _ ->
            game


isFinished : Game -> Bool
isFinished game =
    case game.record of
        first :: second :: _ ->
            first.move == Pass && second.move == Pass

        _ ->
            False


advance : Play -> Stones -> Game -> Game
advance recorded stones game =
    { board = game.board
    , record = recorded :: game.record
    , onMove = otherColor recorded.color
    , stones = stones
    , history = game.stones :: game.history
    }


play : Spot -> Game -> Maybe Game
play spot game =
    if isFinished game then
        Nothing

    else
        playStone (createStone game.onMove spot) game
            |> Maybe.andThen
                (\stones ->
                    Dict.get ( spot.x, spot.y ) stones
                        |> Maybe.map (\stone -> advance { color = game.onMove, move = Place stone } stones game)
                )


playStone : Stone -> Game -> Maybe Stones
playStone stone game =
    Play.playIfLegalOn game.board game.stones stone
        |> Maybe.andThen
            (\stones ->
                if violatesKo stone stones game then
                    Nothing

                else
                    Just stones
            )


violatesKo : Stone -> Stones -> Game -> Bool
violatesKo stone stones game =
    -- A surviving placement adds one stone: equal counts mean one capture.
    if Dict.size stones /= Dict.size game.stones then
        False

    else
        case ( List.head game.record |> Maybe.map .move, List.head game.history ) of
            ( Just (Place last), Just previousStones ) ->
                last.color
                    /= stone.color
                    && List.length (removedStones previousStones game.stones)
                    == 1
                    && not (Dict.member (stoneKey last) stones)

            _ ->
                False


pass : Game -> Maybe Game
pass game =
    if isFinished game then
        Nothing

    else
        Just (advance { color = game.onMove, move = Pass } game.stones game)
