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
    let
        candidate =
            Placement.nearbySpot model.game.board model.game.stones coords
                |> Maybe.map (createStone model.game.onMove)
    in
    case MovePreview.forGame model.game candidate Nothing of
        Nothing ->
            Nothing

        Just preview ->
            case model.ghostStone of
                Just previous ->
                    if previous.stone.spot == preview.stone.spot then
                        -- Preserve the wrapper for Svg.Lazy's reference comparison.
                        model.ghostStone

                    else
                        Just preview

                Nothing ->
                    Just preview
