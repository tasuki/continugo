module BoardHover exposing (State, at)

import List.Extra
import MovePreview
import Placement
import Rules.Game as Game
import Rules.Play as Play
import Rules.Position exposing (..)


type alias State a =
    { a
        | game : Game.Game
        , ghostStone : Maybe MovePreview.Preview
        , highlightedGroup : List Spot
        , justPlayed : Maybe Spot
    }


at : State a -> Spot -> State a
at model hoverSpot =
    let
        overStone =
            stoneList model.game.stones
                |> List.Extra.find (\s -> isWithinStoneOn model.game.board hoverSpot s.spot)

        alreadyShown stone =
            Just stone.spot == model.justPlayed || List.member stone.spot model.highlightedGroup
    in
    case overStone of
        Just stone ->
            if alreadyShown stone then
                model

            else
                { model
                    | highlightedGroup = List.map .spot (Play.group model.game.stones stone)
                    , ghostStone = Nothing
                    , justPlayed = Nothing
                }

        Nothing ->
            { model
                | ghostStone = previewNearbyMove hoverSpot model
                , highlightedGroup = []
                , justPlayed = Nothing
            }


previewNearbyMove : Spot -> State a -> Maybe MovePreview.Preview
previewNearbyMove coords model =
    case Placement.playNearby model.game.board model.game.onMove model.game.stones coords of
        Nothing ->
            Nothing

        Just result ->
            case model.ghostStone of
                Just previous ->
                    if previous.stone.spot == (Tuple.second result).spot then
                        -- Preserve the wrapper for Svg.Lazy's reference comparison.
                        model.ghostStone

                    else
                        Just (MovePreview.fromResult model.game.stones result)

                Nothing ->
                    Just (MovePreview.fromResult model.game.stones result)
