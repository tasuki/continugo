module CoverageTests exposing (tests)

import BoardGeometry
import Coverage exposing (Point, canCover)
import Dict
import Expect
import Fixtures.Play exposing (decodeRecord)
import Fuzz
import Rules.Board as Board exposing (stoneRadius)
import Rules.Play as Play
import Rules.Position as Position exposing (Spot, diameter)
import Test exposing (..)


geometry : List Spot -> Coverage.Geometry
geometry =
    Coverage.geometry Board.default


libertyGeometry : List Spot -> List Spot -> Coverage.Geometry
libertyGeometry =
    Coverage.libertyGeometry Board.default


adjacentDistance : Float
adjacentDistance =
    BoardGeometry.adjacentDistance Board.default


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
        , test "B[sjbR];B[rBes] has no integer centre in the tiny right-wall pocket" <|
            \_ ->
                let
                    spots =
                        Play.replayPlacements (decodeRecord "B[sjbR];B[rBes]") Dict.empty
                            |> Position.stoneList
                            |> List.map .spot

                    pocket =
                        Point 990 169
                in
                Expect.equal ( False, False, True )
                    ( canCover (geometry spots) pocket
                    , canCover (libertyGeometry spots spots) pocket
                    , canCover (geometry [ Spot 944 95, Spot 911 226 ]) pocket
                    )
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
                Expect.equal ( [], False )
                    ( shape.spans, canCover shape (Point 500 500) )
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
        , describe "A group's liberty-area"
            [ test "An empty group has no highlighted area" <|
                \_ ->
                    Expect.equal False (canCover (libertyGeometry [] []) (Point 500 500))
            , test "The reported black eye is not a liberty of the diagonally placed white group" <|
                \_ ->
                    let
                        record =
                            "B[ckaM];B[aMck];B[dIck];B[ckdI];B[aMdI];B[dIaM];W[rbaU];B[pmbt];W[sjcl];B[pNdt];W[rfdY];B[pXfw];W[sbfQ];B[sAhO];B[qBhv];W[aMfg];W[ckfg];W[fgaM];W[fgck];W[dIdI];W[dIfg]"

                        stones =
                            Play.replayPlacements (decodeRecord record) Dict.empty

                        members key =
                            Dict.get key stones
                                |> Maybe.map (Play.group stones >> List.map .spot)
                                |> Maybe.withDefault []

                        spots =
                            List.map .spot (Position.stoneList stones)

                        white =
                            libertyGeometry spots (members ( 190, 190 ))

                        black =
                            libertyGeometry spots (members ( 114, 38 ))

                        eye =
                            [ Point 114 114, Point 140 114, Point 114 140 ]
                    in
                    Expect.equal ( False, [ False, False, False ], [ True, True, True ] )
                        ( Position.adjacent (Spot 114 114) (Spot 190 190)
                        , List.map (canCover white) eye
                        , List.map (canCover black) eye
                        )
            , test "An isolated centre exactly d*sqrt(2) away is not a liberty" <|
                \_ ->
                    let
                        diagonal =
                            Spot (500 + d) (500 + d)

                        spots =
                            [ Spot (500 - d) 500, Spot (500 + d) 500, Spot 500 (500 - d), Spot 500 (500 + d), diagonal ]

                        shape =
                            libertyGeometry spots [ diagonal ]
                    in
                    Expect.equal ( True, [ False, False, False ] )
                        ( canCover (geometry spots) (Point 500 500)
                        , List.map (canCover shape) [ Point 500 500, Point (500 + r) 500, Point 500 (500 + r) ]
                        )
            , test "Another genuinely adjacent member can still contribute the isolated liberty" <|
                \_ ->
                    let
                        diagonal =
                            Spot (500 + d) (500 + d)

                        neighbour =
                            Spot (500 + d) 500

                        spots =
                            [ Spot (500 - d) 500, neighbour, Spot 500 (500 - d), Spot 500 (500 + d), diagonal ]
                    in
                    Expect.equal True (canCover (libertyGeometry spots [ diagonal, neighbour ]) (Point 500 500))
            , test "A lone stone has a continuous ring reaching one radius beyond adjacency" <|
                \_ ->
                    let
                        shape =
                            libertyGeometry [ Spot 500 500 ] [ Spot 500 500 ]
                    in
                    Expect.equal [ False, True, True, False ]
                        (List.map (canCover shape)
                            [ Point (500 + r - 1) 500
                            , Point (500 + r) 500
                            , Point (500 + adjacentDistance + r - 1) 500
                            , Point (500 + adjacentDistance + r + 1) 500
                            ]
                        )
            , test "Every member contributes, but a distant stone outside the group does not" <|
                \_ ->
                    let
                        shape =
                            libertyGeometry [ Spot 400 500, Spot 500 500, Spot 800 500 ] [ Spot 400 500, Spot 500 500 ]
                    in
                    Expect.equal [ True, True, False ]
                        (List.map (canCover shape) [ Point 300 500, Point 600 500, Point 850 500 ])
            , test "Other stones block the liberty-area" <|
                \_ ->
                    Expect.equal ( True, False )
                        ( canCover (libertyGeometry [ Spot 500 500 ] [ Spot 500 500 ]) (Point 580 500)
                        , canCover (libertyGeometry [ Spot 500 500, Spot 600 500 ] [ Spot 500 500 ]) (Point 580 500)
                        )
            , test "An isolated liberty centre still highlights a stone-sized opening" <|
                \_ ->
                    let
                        shape =
                            libertyGeometry
                                [ Spot (500 - d) 500, Spot (500 + d) 500, Spot 500 (500 - d), Spot 500 (500 + d) ]
                                [ Spot (500 - d) 500 ]
                    in
                    Expect.equal [ True, True, False ]
                        (List.map (canCover shape) [ Point 500 500, Point (500 + r) 500, Point (501 + r) 500 ])
            , test "Liberty-area is clipped at the board edge" <|
                \_ ->
                    let
                        shape =
                            libertyGeometry [ Spot stoneRadius 500 ] [ Spot stoneRadius 500 ]
                    in
                    Expect.equal [ True, False, False ]
                        (List.map (canCover shape) [ Point 0 (500 + toFloat d), Point 0 500, Point -1 (500 + toFloat d) ])
            , fuzz2
                (Fuzz.listOfLength 8 (Fuzz.map2 Spot (Fuzz.intRange 38 962) (Fuzz.intRange 38 962)))
                (Fuzz.map2 Point (Fuzz.floatRange 0 1000) (Fuzz.floatRange 0 1000))
                "The highlighted area is always a subset of the global swept area"
              <|
                \spots point ->
                    Expect.equal True
                        (not (canCover (libertyGeometry spots (List.take 2 spots)) point)
                            || canCover (geometry spots) point
                        )
            ]
        , fuzz2
            (Fuzz.listOfLength 12 (Fuzz.map2 Spot (Fuzz.intRange 38 962) (Fuzz.intRange 38 962)))
            (Fuzz.map2 (\x y -> Point (toFloat x) (toFloat y)) (Fuzz.intRange 38 962) (Fuzz.intRange 38 962))
            "Every physically valid integer centre sweeps out an entire stone"
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
        , fuzz (Fuzz.listOfLength 12 (Fuzz.map2 Spot (Fuzz.intRange 38 962) (Fuzz.intRange 38 962))) "Every sweep span has physically valid integer endpoints" <|
            \spots ->
                let
                    shape =
                        geometry spots

                    valid spot =
                        Position.isWithinBoardOn Board.default spot
                            && not (Position.overlapsAnyOn Board.default spot spots)

                    validSpan span =
                        span.left
                            <= span.right
                            && valid (Spot span.left span.y)
                            && valid (Spot span.right span.y)
                in
                Expect.equal True (List.all validSpan shape.spans)
        ]
