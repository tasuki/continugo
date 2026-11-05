module BoardGeometry exposing (Border(..), adjacentDistance, borderDistance, borderReach, borderSpot, connectedBorders, distance)

{-| Shared geometry for dragging and drawing
-}

import Rules.Board as Board exposing (Board)
import Rules.Position exposing (Spot, distanceSquared)


type Border
    = Left
    | Right
    | Top
    | Bottom


borderDistance : Border -> Spot -> Int
borderDistance border spot =
    case border of
        Left ->
            spot.x

        Right ->
            Board.coordRange - spot.x

        Top ->
            spot.y

        Bottom ->
            Board.coordRange - spot.y


borderSpot : Border -> Spot -> Spot
borderSpot border spot =
    case border of
        Left ->
            { spot | x = 0 }

        Right ->
            { spot | x = Board.coordRange }

        Top ->
            { spot | y = 0 }

        Bottom ->
            { spot | y = Board.coordRange }


{-| Last integer centre that prevents fitting enemy stones from connecting
beneath it. Keep the strict adjacency and integer rounding used by the rules.
-}
borderReach : Board -> Int
borderReach board =
    let
        d =
            Board.diameter board

        lastConnectedHalfGap =
            ceiling (toFloat d / sqrt 2) - 1
    in
    Board.radius board
        + ceiling (sqrt (toFloat (d * d - lastConnectedHalfGap * lastConnectedHalfGap)))
        - 1


connectedBorders : Board -> Spot -> List Border
connectedBorders board spot =
    List.filter (\border -> borderDistance border spot <= borderReach board) [ Left, Right, Top, Bottom ]


adjacentDistance : Board -> Float
adjacentDistance board =
    sqrt 2 * toFloat (Board.diameter board)


distance : Spot -> Spot -> Float
distance a b =
    sqrt (toFloat (distanceSquared a b))
