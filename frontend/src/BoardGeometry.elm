module BoardGeometry exposing (adjacentDistance, distance)

{-| Floating-point lengths for dragging and drawing
-}

import Rules.Board as Board exposing (Board)
import Rules.Position exposing (Spot, distanceSquared)


adjacentDistance : Board -> Float
adjacentDistance board =
    sqrt 2 * toFloat (Board.diameter board)


distance : Spot -> Spot -> Float
distance a b =
    sqrt (toFloat (distanceSquared a b))
