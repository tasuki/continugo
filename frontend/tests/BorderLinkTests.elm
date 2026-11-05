module BorderLinkTests exposing (tests)

import Analysis
import Dict
import Expect
import Main
import MovePreview
import Rules.Board as Board
import Rules.Position exposing (..)
import StoneView exposing (getBorderLinks)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


tests : Test
tests =
    describe "Border connections"
        [ test "Links reach the actual boundary on all four sides" <|
            \_ ->
                Expect.equal
                    [ ( Spot 60 500, Spot 0 500 )
                    , ( Spot 940 500, Spot 1000 500 )
                    , ( Spot 500 60, Spot 500 0 )
                    , ( Spot 500 940, Spot 500 1000 )
                    ]
                    (List.concatMap (createStone Black >> getBorderLinks Board.default)
                        [ Spot 60 500, Spot 940 500, Spot 500 60, Spot 500 940 ]
                    )
        , test "A corner stone links to both edges; a central stone links to neither" <|
            \_ ->
                Expect.equal
                    ( [ ( Spot 38 38, Spot 0 38 ), ( Spot 38 38, Spot 38 0 ) ], [] )
                    ( getBorderLinks Board.default (createStone Black (Spot 38 38))
                    , getBorderLinks Board.default (createStone White (Spot 500 500))
                    )
        , test "The cutoff uses enemy connectivity and integer centres at every board size" <|
            \_ ->
                [ ( 9, 133 ), ( 13, 92 ), ( 19, 63 ) ]
                    |> List.map
                        (\( size, lastBlocked ) ->
                            let
                                board =
                                    Board.fromSize size |> Maybe.withDefault Board.default

                                links x =
                                    getBorderLinks board (createStone White (Spot x 500))
                            in
                            ( links lastBlocked, links (lastBlocked + 1) )
                        )
                    |> Expect.equal
                        [ ( [ ( Spot 133 500, Spot 0 500 ) ], [] )
                        , ( [ ( Spot 92 500, Spot 0 500 ) ], [] )
                        , ( [ ( Spot 63 500, Spot 0 500 ) ], [] )
                        ]
        , test "Placed stones use the ordinary link layer" <|
            \_ ->
                Main.viewPosition Board.default placed Nothing [] Nothing [] Main.Normal Analysis.empty
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "links" ]
                    |> Query.findAll [ Selector.tag "line" ]
                    |> Query.count (Expect.equal 1)
        , test "Hover and staged previews use the ghost link layer alongside placed links" <|
            \_ ->
                let
                    preview =
                        MovePreview.fromCandidates Board.default placed (Just (createStone White (Spot 500 60))) Nothing

                    check mode =
                        Main.viewPosition Board.default placed preview [] Nothing [] mode Analysis.empty
                            |> Query.fromHtml
                            |> Expect.all
                                (List.map
                                    (\id view ->
                                        view
                                            |> Query.find [ Selector.id id ]
                                            |> Query.findAll [ Selector.tag "line" ]
                                            |> Query.count (Expect.equal 1)
                                    )
                                    [ "ghost-links", "links" ]
                                )
                in
                Expect.all [ \_ -> check Main.Normal, \_ -> check Main.Staged ] ()
        ]


placed : Stones
placed =
    let
        stone =
            createStone Black (Spot 60 500)
    in
    Dict.singleton (stoneKey stone) stone
