module DeadZones exposing (Geometry, Point, canCover, geometry, view)

{-| A visual, continuous-space model, independent of turns and suicide rules.

The possible stone centres form the inset board minus diameter-radius discs.
To show the area a stone could cover, expand that set by one stone radius:
keep its interior, and stroke its exposed boundary with a round, diameter-wide
stroke. Intersection points are kept too, even when a playable pocket shrinks
to a single point and has no boundary arcs left.

-}

import Board exposing (coordRange, stoneRadius)
import Go exposing (Spot, Stones, diameter, spotKeyStr, stoneList)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed


type alias Point =
    { x : Float, y : Float }


type alias Arc =
    { center : Point, start : Float, end : Float }


type alias Line =
    { start : Point, end : Point }


type alias Geometry =
    { centers : List Point
    , arcs : List Arc
    , lines : List Line
    , points : List Point
    }


radius : Float
radius =
    toFloat stoneRadius


deadRadius : Float
deadRadius =
    toFloat diameter


epsilon : Float
epsilon =
    0.0000001


low : Float
low =
    radius


high : Float
high =
    toFloat coordRange - radius


distance : Point -> Point -> Float
distance a b =
    sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)


placeable : List Point -> Point -> Bool
placeable centers p =
    p.x >= low - epsilon && p.x <= high + epsilon && p.y >= low - epsilon && p.y <= high + epsilon && List.all (\c -> distance c p >= deadRadius - epsilon) centers


onCircle : Point -> Float -> Point
onCircle c angle =
    Point (c.x + deadRadius * cos angle) (c.y + deadRadius * sin angle)


angleOf : Point -> Point -> Float
angleOf c p =
    let
        angle =
            atan2 (p.y - c.y) (p.x - c.x)
    in
    if angle < 0 then
        angle + 2 * pi

    else
        angle


circleCrossings : Point -> Point -> List Point
circleCrossings a b =
    let
        d =
            distance a b
    in
    if d <= epsilon || d > 2 * deadRadius then
        []

    else
        let
            h =
                sqrt (max 0 (deadRadius ^ 2 - (d / 2) ^ 2))

            mx =
                (a.x + b.x) / 2

            my =
                (a.y + b.y) / 2

            dx =
                (b.x - a.x) / d * h

            dy =
                (b.y - a.y) / d * h
        in
        [ Point (mx - dy) (my + dx), Point (mx + dy) (my - dx) ]


boardEdges : List Line
boardEdges =
    [ Line (Point low low) (Point high low)
    , Line (Point high low) (Point high high)
    , Line (Point high high) (Point low high)
    , Line (Point low high) (Point low low)
    ]


along : Line -> Float -> Point
along line t =
    Point
        (line.start.x + t * (line.end.x - line.start.x))
        (line.start.y + t * (line.end.y - line.start.y))


lineCrossings : Line -> Point -> List Float
lineCrossings line c =
    let
        dx =
            line.end.x - line.start.x

        dy =
            line.end.y - line.start.y

        a =
            dx ^ 2 + dy ^ 2

        b =
            2 * (dx * (line.start.x - c.x) + dy * (line.start.y - c.y))

        cc =
            (line.start.x - c.x) ^ 2 + (line.start.y - c.y) ^ 2 - deadRadius ^ 2

        discriminant =
            b ^ 2 - 4 * a * cc
    in
    if discriminant < 0 then
        []

    else
        [ (-b - sqrt discriminant) / (2 * a), (-b + sqrt discriminant) / (2 * a) ]
            |> List.filter (\t -> t >= 0 && t <= 1)


intervals : List Float -> List ( Float, Float )
intervals values =
    let
        sorted =
            List.sort values
    in
    List.map2 Tuple.pair sorted (List.drop 1 sorted)
        |> List.filter (\( a, b ) -> b > a)


geometry : List Spot -> Geometry
geometry spots =
    let
        centers =
            List.map (\p -> Point (toFloat p.x) (toFloat p.y)) spots

        circleBoundary c =
            let
                -- Only these discs can cover any part of this circle.
                neighbours =
                    List.filter (\other -> other /= c && distance c other <= 2 * deadRadius + epsilon) centers

                crossings =
                    List.concatMap (circleCrossings c) neighbours
                        ++ List.concatMap (\edge -> List.map (along edge) (lineCrossings edge c)) boardEdges

                arcs =
                    (0 :: pi :: (2 * pi) :: List.map (angleOf c) crossings)
                        |> intervals
                        |> List.filter (\( a, b ) -> placeable neighbours (onCircle c ((a + b) / 2)))
                        |> List.map (\( a, b ) -> Arc c a b)
            in
            ( arcs, List.filter (placeable centers) crossings )

        edgeBoundary edge =
            let
                cuts =
                    0 :: 1 :: List.concatMap (lineCrossings edge) centers

                lines =
                    intervals cuts
                        |> List.filter (\( a, b ) -> placeable centers (along edge ((a + b) / 2)))
                        |> List.map (\( a, b ) -> Line (along edge a) (along edge b))
            in
            ( lines, List.map (along edge) cuts |> List.filter (placeable centers) )

        circles =
            List.map circleBoundary centers

        edges =
            List.map edgeBoundary boardEdges
    in
    { centers = centers
    , arcs = List.concatMap Tuple.first circles
    , lines = List.concatMap Tuple.first edges
    , points = List.concatMap Tuple.second circles ++ List.concatMap Tuple.second edges
    }


{-| Whether a board point belongs to the swept area. Useful for checking the
same geometry that the SVG draws; this is not a move-legality predicate.
-}
canCover : Geometry -> Point -> Bool
canCover shape p =
    let
        lineDistance line =
            let
                dx =
                    line.end.x - line.start.x

                dy =
                    line.end.y - line.start.y

                t =
                    ((p.x - line.start.x) * dx + (p.y - line.start.y) * dy) / (dx ^ 2 + dy ^ 2)
            in
            distance p (along line (clamp 0 1 t))

        arcDistance arc =
            let
                angle =
                    angleOf arc.center p
            in
            if angle >= arc.start && angle <= arc.end then
                abs (distance p arc.center - deadRadius)

            else
                min (distance p (onCircle arc.center arc.start)) (distance p (onCircle arc.center arc.end))

        near d =
            d <= radius + epsilon
    in
    p.x >= 0 && p.x <= toFloat coordRange && p.y >= 0 && p.y <= toFloat coordRange && (placeable shape.centers p || List.any (arcDistance >> near) shape.arcs || List.any (lineDistance >> near) shape.lines || List.any (distance p >> near) shape.points)


pointString : Point -> String
pointString p =
    String.fromFloat p.x ++ " " ++ String.fromFloat p.y


boardRect : List (Svg.Attribute msg) -> Svg msg
boardRect attributes =
    Svg.rect (SA.x "0" :: SA.y "0" :: SA.width (String.fromInt coordRange) :: SA.height (String.fromInt coordRange) :: attributes) []


circle : Float -> String -> Point -> Svg msg
circle r fill p =
    Svg.circle [ SA.cx (String.fromFloat p.x), SA.cy (String.fromFloat p.y), SA.r (String.fromFloat r), SA.fill fill ] []


view : Stones -> Svg msg
view stones =
    let
        spots =
            List.map .spot (stoneList stones)

        positionKey =
            List.map spotKeyStr spots |> String.join ""
    in
    -- Browsers can retain a stale image of nested SVG masks when only their
    -- contents change. Replace both masks and their painted rectangle whenever
    -- the occupied positions change, not just when the stone count changes.
    Svg.Keyed.node "g"
        [ SA.id "dead-zones", SA.pointerEvents "none" ]
        [ ( positionKey, viewGeometry (geometry spots) ) ]


viewGeometry : Geometry -> Svg msg
viewGeometry shape =
    let
        arcPath arc =
            "M " ++ pointString (onCircle arc.center arc.start) ++ " A " ++ String.fromFloat deadRadius ++ " " ++ String.fromFloat deadRadius ++ " 0 0 1 " ++ pointString (onCircle arc.center arc.end)

        linePath line =
            "M " ++ pointString line.start ++ " L " ++ pointString line.end

        maskAttributes name =
            [ SA.id name, SA.maskUnits "userSpaceOnUse", SA.x "0", SA.y "0", SA.width (String.fromInt coordRange), SA.height (String.fromInt coordRange) ]
    in
    Svg.g []
        [ Svg.defs []
            [ Svg.mask (maskAttributes "stone-swept-area")
                [ Svg.rect [ SA.x (String.fromFloat low), SA.y (String.fromFloat low), SA.width (String.fromFloat (high - low)), SA.height (String.fromFloat (high - low)), SA.fill "white" ] []
                , Svg.g [] (List.map (circle deadRadius "black") shape.centers)
                , Svg.path
                    [ SA.d (String.join " " (List.map arcPath shape.arcs ++ List.map linePath shape.lines))
                    , SA.fill "none"
                    , SA.stroke "white"
                    , SA.strokeWidth (String.fromInt diameter)
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    ]
                    []
                , Svg.g [] (List.map (circle radius "white") shape.points)
                ]
            , Svg.mask (maskAttributes "dead-zone-area")
                [ boardRect [ SA.fill "white" ]
                , boardRect [ SA.fill "black", SA.mask "url(#stone-swept-area)" ]
                ]
            ]
        , boardRect [ SA.id "dead-zone-fill", SA.mask "url(#dead-zone-area)" ]
        ]
