module BoardSizeTests exposing (tests)

import Analysis
import BoardGeometry
import BoardView
import Coverage
import Dict
import DragFrom
import Expect
import Main
import MovePreview
import Placement
import Rules.Board as Board exposing (Board)
import Rules.Position exposing (..)
import Sgf
import StoneView
import Svg
import Svg.Attributes as SA
import TerritoryView
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


tests : Test
tests =
    [ 9, 13, 19 ]
        |> List.filterMap Board.fromSize
        |> List.map configuredTests
        |> describe "Frontend board sizes"


configuredTests : Board -> Test
configuredTests board =
    let
        size =
            Board.size board

        r =
            Board.radius board

        d =
            Board.diameter board

        origin =
            Spot 500 500

        record =
            [ createPlay Black (Spot r 500), createPlay White (Spot (r + d) 500) ]
    in
    describe (String.fromInt size ++ "×" ++ String.fromInt size)
        [ test "The menu button selects this board" <|
            \_ ->
                Main.viewNewGames
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "button", Selector.containing [ Selector.text (String.fromInt size ++ "×" ++ String.fromInt size) ] ]
                    |> Event.simulate Event.click
                    |> Event.expect (Main.NewBoard board)
        , test "Grid lines and stones use the selected size" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        Svg.g [] (BoardView.viewLines board)
                            |> Query.fromHtml
                            |> Query.findAll [ Selector.tag "line" ]
                            |> Query.count (Expect.equal (2 * size))
                    , \_ ->
                        StoneView.viewStone board "" (createStone Black origin)
                            |> Query.fromHtml
                            |> Query.has [ Selector.attribute (SA.r (String.fromInt r)) ]
                    ]
                    ()
        , test "Placement and analysis snap to the same board edge" <|
            \_ ->
                let
                    intended =
                        Spot 1 500

                    circles =
                        Analysis.start board Dict.empty intended Analysis.empty
                            |> Analysis.finish board Dict.empty intended
                            |> Tuple.first
                            |> .circles
                in
                Expect.equal ( Just (Spot r 500), [ Spot r 500 ] )
                    ( Placement.nearbySpot board Dict.empty intended, circles )
        , test "Stone and analysis drags use the selected connection distance" <|
            \_ ->
                let
                    target =
                        Spot 900 500

                    expected =
                        Spot (500 + floor (BoardGeometry.adjacentDistance board)) 500

                    circles =
                        Analysis.start board Dict.empty origin (withCircle origin)
                            |> Analysis.move board Dict.empty target
                            |> Analysis.finish board Dict.empty target
                            |> Tuple.first
                            |> .circles
                in
                Expect.equal ( Just expected, [ origin, expected ] )
                    ( DragFrom.findDraggedSpot board [ origin ] origin target, circles )
        , test "Previews and coverage agree with touching stones" <|
            \_ ->
                let
                    game =
                        Main.gameAt board 1 [ createPlay Black origin ]

                    preview =
                        MovePreview.fromCandidates board game.stones (Just (createStone White (Spot (500 + d) 500))) Nothing

                    coverage =
                        Coverage.geometry board [ origin ]
                in
                Expect.equal ( Just 2, [ False, True ] )
                    ( Maybe.map (.stones >> Dict.size) preview
                    , List.map (Coverage.canCover coverage)
                        [ Coverage.Point (toFloat (500 + r - 1)) 500, Coverage.Point (toFloat (500 + r)) 500 ]
                    )
        , test "Replay and SGF retain the selected board and all stones" <|
            \_ ->
                let
                    sgf =
                        Sgf.onBoard board

                    game =
                        Main.gameAt board 2 record
                in
                Sgf.decodeRecord (Sgf.encodeRecord { sgf | plays = record })
                    |> Result.map (\decoded -> ( Sgf.boardFromRoot decoded.root, game.board, Dict.size game.stones ))
                    |> Expect.equal (Ok ( Just board, board, 2 ))
        , test "The score uses this board's stone-disc area" <|
            \_ ->
                Expect.equal "1" (TerritoryView.formatArea board (pi * toFloat r ^ 2))
        ]


withCircle : Spot -> Analysis.Model
withCircle spot =
    let
        model =
            Analysis.empty
    in
    { model | circles = [ spot ] }
