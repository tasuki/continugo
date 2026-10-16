module DeadZonesTests exposing (tests)

import Board exposing (stoneRadius)
import DeadZones exposing (Point, canCover, geometry)
import Expect
import Fuzz
import Go exposing (Spot, diameter)
import Test exposing (..)


r : Float
r =
    toFloat stoneRadius


d : Int
d =
    diameter


tests : Test
tests =
    describe "Space a physically placeable stone could cover"
        [ test "An empty board is reachable, except its sharp corners" <|
            \_ ->
                let
                    shape =
                        geometry []
                in
                Expect.equal [ True, True, True, False, False ]
                    (List.map (canCover shape)
                        [ Point 500 500, Point 0 500, Point 1000 500, Point 0 0, Point 1000 1000 ]
                    )
        , test "The sweep reaches beyond the legal-centre boundary by one radius" <|
            \_ ->
                let
                    shape =
                        geometry [ Spot 500 500 ]
                in
                Expect.equal [ False, True, True ]
                    (List.map (canCover shape)
                        [ Point (500 + r - 1) 500
                        , Point (500 + r) 500
                        , Point (500 + r + 1) 500
                        ]
                    )
        , test "An occupied stone interior is not reachable from outside" <|
            \_ ->
                let
                    shape =
                        geometry [ Spot 500 500 ]
                in
                Expect.equal [ False, False, False ]
                    (List.map (canCover shape) [ Point 500 500, Point 510 510, Point 480 480 ])
        , test "A captured centre surrounded by four touching stones remains a whole stone-sized opening" <|
            \_ ->
                let
                    shape =
                        geometry
                            [ Spot (500 - d) 500, Spot (500 + d) 500, Spot 500 (500 - d), Spot 500 (500 + d) ]
                in
                Expect.equal [ True, True, False ]
                    (List.map (canCover shape)
                        [ Point 500 500, Point (500 + r) 500, Point (501 + r) 500 ]
                    )
        , test "A tiny pocket is expanded, not discarded or shown as a tiny centre-sized hole" <|
            \_ ->
                let
                    gap =
                        d + 1

                    shape =
                        geometry
                            [ Spot (500 - gap) 500, Spot (500 + gap) 500, Spot 500 (500 - gap), Spot 500 (500 + gap) ]
                in
                Expect.equal True (canCover shape (Point (500 + r + 0.5) 500))
        , test "A stone against the wall blocks coverage until a touching neighbour fits" <|
            \_ ->
                let
                    shape =
                        geometry [ Spot stoneRadius 500 ]
                in
                Expect.equal ( False, True )
                    ( canCover shape (Point 0 (500 + toFloat d - 1))
                    , canCover shape (Point 0 (500 + toFloat d))
                    )
        , test "A completely blocked board has no swept area" <|
            \_ ->
                let
                    -- A dense set deliberately used as a geometry fixture,
                    -- not a legal move sequence.
                    coords =
                        List.range 0 20 |> List.map ((*) 50)

                    spots =
                        List.concatMap (\x -> List.map (Spot x) coords) coords

                    shape =
                        geometry spots
                in
                Expect.equal ( ( [], [], [] ), False )
                    ( ( shape.arcs, shape.lines, shape.points ), canCover shape (Point 500 500) )
        , test "Removing a stone restores the swept area (capture or undo)" <|
            \_ ->
                Expect.equal ( False, True )
                    ( canCover (geometry [ Spot 500 500 ]) (Point 500 500)
                    , canCover (geometry []) (Point 500 500)
                    )
        , test "The rounded board corner is handled by circular, not square expansion" <|
            \_ ->
                let
                    shape =
                        geometry []
                in
                Expect.equal ( False, True )
                    ( canCover shape (Point 10 10), canCover shape (Point 12 12) )
        , fuzz2
            (Fuzz.listOfLength 12 (Fuzz.map2 Spot (Fuzz.intRange 38 962) (Fuzz.intRange 38 962)))
            (Fuzz.map2 Point (Fuzz.floatRange 38 962) (Fuzz.floatRange 38 962))
            "Every physically valid centre sweeps out an entire stone"
          <|
            \spots center ->
                let
                    valid =
                        List.all (\s -> sqrt ((center.x - toFloat s.x) ^ 2 + (center.y - toFloat s.y) ^ 2) >= toFloat d) spots

                    shape =
                        geometry spots

                    samples =
                        List.range 0 15
                            |> List.map
                                (\i ->
                                    let
                                        angle =
                                            toFloat i * pi / 8
                                    in
                                    Point (center.x + r * cos angle) (center.y + r * sin angle)
                                )
                in
                Expect.equal True (not valid || List.all (canCover shape) samples)
        , fuzz (Fuzz.listOfLength 12 (Fuzz.map2 Spot (Fuzz.intRange 38 962) (Fuzz.intRange 38 962))) "Every exposed arc midpoint is a physically valid centre" <|
            \spots ->
                let
                    shape =
                        geometry spots

                    valid p =
                        p.x >= r - 0.000001 && p.x <= 1000 - r + 0.000001 && p.y >= r - 0.000001 && p.y <= 1000 - r + 0.000001 && List.all (\s -> sqrt ((p.x - toFloat s.x) ^ 2 + (p.y - toFloat s.y) ^ 2) >= toFloat d - 0.000001) spots

                    midpoint arc =
                        let
                            angle =
                                (arc.start + arc.end) / 2
                        in
                        Point (arc.center.x + toFloat d * cos angle) (arc.center.y + toFloat d * sin angle)
                in
                Expect.equal True (List.all (midpoint >> valid) shape.arcs)
        ]
