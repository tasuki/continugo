module DeadZones exposing (Geometry, Point, canCover, geometry, libertyGeometry, view, viewLiberties)

{-| A visual, continuous-space model, independent of turns and suicide rules.

The possible stone centres form the inset board minus diameter-radius discs.
To show the area a stone could cover, expand that set by one stone radius:
keep its interior, and stroke its exposed boundary with a round, diameter-wide
stroke. Intersection points are kept too, even when a playable pocket shrinks
to a single point and has no boundary arcs left.

For a group's liberty-area, first restrict the possible centres to the union
of adjacency discs around its members. Both displays share the same sweep.

-}

import Board exposing (coordRange, stoneRadius)
import Go exposing (Spot, Stones, adjacentDistance, diameter, spotKeyStr, stoneList)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed


type alias Point =
    { x : Float, y : Float }


type alias Circle =
    { center : Point, radius : Float }


type alias Arc =
    { center : Point, radius : Float, start : Float, end : Float }


type alias Line =
    { start : Point, end : Point }


type alias Geometry =
    { centers : List Point
    , reach : Maybe (List Point)
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


inReach : Maybe (List Point) -> Point -> Bool
inReach reach p =
    reach
        |> Maybe.map (List.any (\c -> distance c p <= adjacentDistance + epsilon))
        |> Maybe.withDefault True


validCenter : List Point -> Maybe (List Point) -> Point -> Bool
validCenter centers reach p =
    placeable centers p && inReach reach p


onCircle : { a | center : Point, radius : Float } -> Float -> Point
onCircle c angle =
    Point (c.center.x + c.radius * cos angle) (c.center.y + c.radius * sin angle)


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


circleCrossings : Circle -> Circle -> List Point
circleCrossings a b =
    let
        d =
            distance a.center b.center
    in
    if d <= epsilon || d > a.radius + b.radius || d < abs (a.radius - b.radius) then
        []

    else
        let
            alongCenters =
                (a.radius ^ 2 - b.radius ^ 2 + d ^ 2) / (2 * d)

            h =
                sqrt (max 0 (a.radius ^ 2 - alongCenters ^ 2))

            ux =
                (b.center.x - a.center.x) / d

            uy =
                (b.center.y - a.center.y) / d

            mx =
                a.center.x + alongCenters * ux

            my =
                a.center.y + alongCenters * uy
        in
        [ Point (mx - h * uy) (my + h * ux), Point (mx + h * uy) (my - h * ux) ]


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


lineCrossings : Line -> Circle -> List Float
lineCrossings line c =
    let
        dx =
            line.end.x - line.start.x

        dy =
            line.end.y - line.start.y

        a =
            dx ^ 2 + dy ^ 2

        b =
            2 * (dx * (line.start.x - c.center.x) + dy * (line.start.y - c.center.y))

        cc =
            (line.start.x - c.center.x) ^ 2 + (line.start.y - c.center.y) ^ 2 - c.radius ^ 2

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
geometry =
    regionGeometry Nothing


{-| Restrict possible centres to spots adjacent to at least one group member,
then expand by a stone radius, just like the deadzone's swept area.
-}
libertyGeometry : List Spot -> List Spot -> Geometry
libertyGeometry stones group =
    regionGeometry (Just group) stones


toPoint : Spot -> Point
toPoint p =
    Point (toFloat p.x) (toFloat p.y)


regionGeometry : Maybe (List Spot) -> List Spot -> Geometry
regionGeometry group spots =
    let
        centers =
            case reach of
                Nothing ->
                    List.map toPoint spots

                Just members ->
                    -- Only blockers that can intersect the group's possible
                    -- centres affect its liberty-area (or its swept boundary).
                    List.map toPoint spots
                        |> List.filter (\c -> List.any (\member -> distance c member <= deadRadius + adjacentDistance + epsilon) members)

        reach =
            Maybe.map (List.map toPoint) group

        circles =
            List.map (\c -> Circle c deadRadius) centers
                ++ List.map (\c -> Circle c adjacentDistance) (Maybe.withDefault [] reach)

        circleBoundary c =
            let
                neighbours =
                    List.filter (\other -> other /= c && distance c.center other.center <= c.radius + other.radius + epsilon) circles

                nearbyStones =
                    List.filter (\other -> distance c.center other <= c.radius + deadRadius + epsilon) centers

                crossings =
                    List.concatMap (circleCrossings c) neighbours
                        ++ List.concatMap (\edge -> List.map (along edge) (lineCrossings edge c)) boardEdges

                arcs =
                    (0 :: pi :: (2 * pi) :: List.map (angleOf c.center) crossings)
                        |> intervals
                        |> List.filter (\( a, b ) -> validCenter nearbyStones reach (onCircle c ((a + b) / 2)))
                        |> List.map (\( a, b ) -> Arc c.center c.radius a b)
            in
            ( arcs, List.filter (validCenter nearbyStones reach) crossings )

        edgeBoundary edge =
            let
                cuts =
                    0 :: 1 :: List.concatMap (lineCrossings edge) circles

                lines =
                    intervals cuts
                        |> List.filter (\( a, b ) -> validCenter centers reach (along edge ((a + b) / 2)))
                        |> List.map (\( a, b ) -> Line (along edge a) (along edge b))
            in
            ( lines, List.map (along edge) cuts |> List.filter (validCenter centers reach) )

        circleBoundaries =
            List.map circleBoundary circles

        edges =
            List.map edgeBoundary boardEdges
    in
    { centers = centers
    , reach = reach
    , arcs = List.concatMap Tuple.first circleBoundaries
    , lines = List.concatMap Tuple.first edges
    , points = List.concatMap Tuple.second circleBoundaries ++ List.concatMap Tuple.second edges
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
                abs (distance p arc.center - arc.radius)

            else
                min (distance p (onCircle arc arc.start)) (distance p (onCircle arc arc.end))

        near d =
            d <= radius + epsilon
    in
    p.x >= 0 && p.x <= toFloat coordRange && p.y >= 0 && p.y <= toFloat coordRange && (validCenter shape.centers shape.reach p || List.any (arcDistance >> near) shape.arcs || List.any (lineDistance >> near) shape.lines || List.any (distance p >> near) shape.points)


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


maskAttributes : String -> List (Svg.Attribute msg)
maskAttributes name =
    [ SA.id name, SA.maskUnits "userSpaceOnUse", SA.x "0", SA.y "0", SA.width (String.fromInt coordRange), SA.height (String.fromInt coordRange) ]


sweptMasks : String -> Geometry -> List (Svg msg)
sweptMasks name shape =
    let
        arcPath arc =
            "M " ++ pointString (onCircle arc arc.start) ++ " A " ++ String.fromFloat arc.radius ++ " " ++ String.fromFloat arc.radius ++ " 0 0 1 " ++ pointString (onCircle arc arc.end)

        linePath line =
            "M " ++ pointString line.start ++ " L " ++ pointString line.end

        reachMask =
            name ++ "-reach"

        reachAttributes =
            case shape.reach of
                Nothing ->
                    []

                Just _ ->
                    [ SA.mask ("url(#" ++ reachMask ++ ")") ]

        reachDefinition =
            case shape.reach of
                Nothing ->
                    []

                Just centers ->
                    [ Svg.mask (maskAttributes reachMask) (List.map (circle adjacentDistance "white") centers) ]
    in
    reachDefinition
        ++ [ Svg.mask (maskAttributes name)
                [ Svg.g reachAttributes
                    [ Svg.rect [ SA.x (String.fromFloat low), SA.y (String.fromFloat low), SA.width (String.fromFloat (high - low)), SA.height (String.fromFloat (high - low)), SA.fill "white" ] []
                    , Svg.g [] (List.map (circle deadRadius "black") shape.centers)
                    ]
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
           ]


viewGeometry : Geometry -> Svg msg
viewGeometry shape =
    Svg.g []
        [ Svg.defs []
            (sweptMasks "stone-swept-area" shape
                ++ [ Svg.mask (maskAttributes "dead-zone-area")
                        [ boardRect [ SA.fill "white" ]
                        , boardRect [ SA.fill "black", SA.mask "url(#stone-swept-area)" ]
                        ]
                   ]
            )
        , boardRect [ SA.id "dead-zone-fill", SA.mask "url(#dead-zone-area)" ]
        ]


viewLiberties : Stones -> List Spot -> Svg msg
viewLiberties stones group =
    let
        spots =
            List.map .spot (stoneList stones)

        positionKey =
            (List.map spotKeyStr spots |> String.join "") ++ "/" ++ (List.map spotKeyStr group |> String.join "")
    in
    Svg.Keyed.node "g"
        [ SA.id "liberties", SA.pointerEvents "none" ]
        (if List.isEmpty group then
            []

         else
            [ ( positionKey
              , Svg.g []
                    [ Svg.defs [] (sweptMasks "liberty-swept-area" (libertyGeometry spots group))
                    , boardRect [ SA.id "liberty-area-fill", SA.mask "url(#liberty-swept-area)" ]
                    ]
              )
            ]
        )
