module MovePreview exposing (Preview, fromMoves, fromResult)

import Dict
import Maybe.Extra
import Rules.Play as Play
import Rules.Position exposing (Spot, Stone, Stones, removedStones, stoneKey)


type alias Preview =
    { stones : Stones
    , stone : Stone
    , group : List Spot
    , captured : List Stone
    }


{-| A staged move takes precedence over hovering. Replay the exact candidate,
not a nearby move, so captures and connections match what would be confirmed.
-}
fromMoves : Stones -> Maybe Stone -> Maybe Stone -> Maybe Preview
fromMoves stones hovered staged =
    Maybe.Extra.or staged hovered
        |> Maybe.andThen
            (\candidate ->
                Play.playIfLegal stones candidate
                    |> Maybe.andThen
                        (\result ->
                            Dict.get (stoneKey candidate) result
                                |> Maybe.map (\stone -> fromResult stones ( result, stone ))
                        )
            )


{-| Reuse a successful legality check instead of replaying the move to preview it.
The stone must be the surviving, enhanced stone from the resulting position.
-}
fromResult : Stones -> ( Stones, Stone ) -> Preview
fromResult stones ( result, stone ) =
    { stones = result
    , stone = stone
    , group = List.map .spot (Play.group result stone)
    , captured = removedStones stones result
    }
