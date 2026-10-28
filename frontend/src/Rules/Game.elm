module Rules.Game exposing (Game, empty, fromRecord, isFinished, pass, play)

{-| Rules depend only on elm/core and other Rules modules. The frontend owns
rendering, gestures, placement adjustment, and record serialization.

Game handles normal turn progression; Play handles exact placements on arbitrary
positions. Neither implements ko yet. Records are stored newest first, matching
the frontend's existing history convention.

-}

import Dict
import Rules.Play as Play
import Rules.Position exposing (..)


type alias Game =
    { record : List Play
    , onMove : Player
    , stones : Stones
    }


empty : Game
empty =
    { record = []
    , onMove = Black
    , stones = Dict.empty
    }


{-| Replay a record, accepting setup-like sequences of same-colour moves.
Illegal placements are skipped on the board but retained in the record.
This is not a validator for externally submitted game histories.
-}
fromRecord : List Play -> Game
fromRecord record =
    { record = record
    , stones = Play.playStones (List.reverse record) Dict.empty
    , onMove =
        List.head record
            |> Maybe.map (.player >> otherPlayer)
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
    { record = { player = game.onMove, move = move } :: game.record
    , onMove = otherPlayer game.onMove
    , stones = stones
    }


play : Spot -> Game -> Maybe Game
play spot game =
    if isFinished game then
        Nothing

    else
        Play.playIfLegal game.stones (createStone game.onMove spot)
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
