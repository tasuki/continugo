module Rules.Game exposing (Game, empty, fromRecord, fromRecordOn, isFinished, onBoard, pass, play)

{-| Rules depend only on elm/core and other Rules modules. The frontend owns
rendering, gestures, placement adjustment, and record serialization.

Game handles normal turn progression; Play handles exact placements on arbitrary
positions. Neither implements ko yet. Records are stored newest first, matching
the frontend's existing history convention.

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
    { board = board
    , record = record
    , stones = Play.replayPlacementsOn board (List.reverse record) Dict.empty
    , onMove =
        List.head record
            |> Maybe.map (.color >> otherColor)
            |> Maybe.withDefault Black
    }


isFinished : Game -> Bool
isFinished game =
    case game.record of
        first :: second :: _ ->
            first.move == Pass && second.move == Pass

        _ ->
            False


advance : Move -> Stones -> Game -> Game
advance move stones game =
    { board = game.board
    , record = { color = game.onMove, move = move } :: game.record
    , onMove = otherColor game.onMove
    , stones = stones
    }


play : Spot -> Game -> Maybe Game
play spot game =
    if isFinished game then
        Nothing

    else
        Play.playIfLegalOn game.board game.stones (createStone game.onMove spot)
            |> Maybe.andThen
                (\stones ->
                    Dict.get ( spot.x, spot.y ) stones
                        |> Maybe.map (\stone -> advance (Place stone) stones game)
                )


pass : Game -> Maybe Game
pass game =
    if isFinished game then
        Nothing

    else
        Just (advance Pass game.stones game)
