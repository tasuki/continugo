module BoardGeometry exposing (adjacentDistance, distance)

{-| Floating-point lengths for dragging and drawing
-}

import Rules.Position exposing (Spot, diameter, distanceSquared)


adjacentDistance : Float
adjacentDistance =
    sqrt 2 * toFloat diameter


distance : Spot -> Spot -> Float
distance a b =
    sqrt (toFloat (distanceSquared a b))
