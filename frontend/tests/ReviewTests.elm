module ReviewTests exposing (tests)

import Expect
import Json.Encode as Encode
import Main
import Review
import Rules.Board as Board
import Rules.Game as Game
import Rules.Position exposing (..)
import Sgf exposing (Tree(..))
import Test exposing (..)


black =
    createPlay Black (Spot 200 200)


white =
    createPlay White (Spot 500 500)


alternative =
    createPlay White (Spot 700 700)


forest =
    [ Node black
        []
        [ Node white [ ( "C", [ "game" ] ) ] [ Node (createPass Black) [] [] ]
        , Node alternative [] [ Node (createPlay Black (Spot 300 700)) [] [ Node (createPass White) [] [] ] ]
        , Node (createPass White) [] []
        ]
    ]


plays model =
    (Review.line Sgf.empty.root model).plays


tests : Test
tests =
    describe "Saved variations"
        [ test "An alternative preserves the game and replaying it selects the existing branch" <|
            \_ ->
                let
                    original =
                        Review.fromTrees (Sgf.fromRecord { root = Sgf.empty.root, plays = [ black, white ], moveProperties = [] })

                    branch =
                        Review.add 1 alternative original

                    repeated =
                        Review.add 1 alternative branch
                in
                Expect.equal
                    ( [ black, white ], [ black, alternative ], branch.trees )
                    ( (Sgf.mainRecord Sgf.empty.root branch.trees).plays, plays branch, repeated.trees )
        , test "Replaying an imported move ignores cached stone connectivity" <|
            \_ ->
                let
                    record =
                        { root = Sgf.empty.root, plays = [ black, white, createPlay Black (Spot 280 200) ], moveProperties = [] }

                    original =
                        Review.fromTrees (Sgf.fromRecord record)
                in
                Main.gameAt Board.default 2 record.plays
                    |> Game.play (Spot 280 200)
                    |> Maybe.andThen (.record >> List.head)
                    |> Maybe.map (\play -> (Review.add 2 play original).trees)
                    |> Expect.equal (Just original.trees)
        , test "Sibling markers exclude the current move and its children" <|
            \_ ->
                Review.fromTrees forest
                    |> Review.siblings 2
                    |> List.map Tuple.first
                    |> Expect.equal [ 1, 2 ]
        , test "The root and positions without literal siblings have no markers" <|
            \_ ->
                let
                    review =
                        Review.fromTrees forest
                in
                Expect.equal ( [], [], [] )
                    ( Review.siblings 0 review, Review.siblings 1 review, Review.siblings 3 review )
        , test "Selecting a sibling remembers its continuation without changing the tree" <|
            \_ ->
                let
                    review =
                        Review.fromTrees forest |> Review.selectSibling 2 1
                in
                Expect.equal ( [ 0, 1, 0, 0 ], forest ) ( review.selected, review.trees )
        , test "Selecting an invalid sibling leaves the selected line unchanged" <|
            \_ ->
                let
                    review =
                        Review.fromTrees forest
                in
                Expect.equal review (Review.selectSibling 0 1 review |> Review.selectSibling 2 900)
        , test "Switching at the same move crosses non-sibling nodes and skips shorter lines" <|
            \_ ->
                Review.fromTrees forest
                    |> Review.switch 1 3
                    |> (\review -> Expect.equal [ 0, 1, 0, 0 ] review.selected)
        , test "Up selects the original game without changing the move tree" <|
            \_ ->
                let
                    review =
                        Review.fromTrees forest |> Review.switch 1 3 |> Review.switch -1 3
                in
                Expect.equal ( [ 0, 0, 0 ], forest ) ( review.selected, review.trees )
        , test "Returning to a fork remembers its selected continuation" <|
            \_ ->
                Review.fromTrees forest
                    |> Review.select [ 0, 1, 0 ]
                    |> Review.select [ 0 ]
                    |> .selected
                    |> Expect.equal [ 0, 1, 0, 0 ]
        , test "Switching is bounded at the root and outer lines" <|
            \_ ->
                let
                    original =
                        Review.fromTrees forest
                in
                Expect.equal original.selected (Review.switch -1 3 original |> Review.switch 1 0 |> .selected)
        , test "Tree SGF round-trips nested variations, passes, and escaped properties" <|
            \_ ->
                let
                    root =
                        Sgf.empty.root ++ [ ( "PB", [ "A]lice\\" ] ) ]
                in
                Sgf.encodeTree root forest |> Sgf.decodeTree |> Expect.equal (Ok ( root, forest ))
        , test "The record API and viewer read the original game from a tree" <|
            \_ ->
                Sgf.encodeTree Sgf.empty.root forest
                    |> Sgf.decodeRecord
                    |> Result.map .plays
                    |> Expect.equal (Ok [ black, white, createPass Black ])
        , test "Sessions restore a variation beyond the end of the actual game" <|
            \_ ->
                Main.readReviewSession
                    (Encode.object
                        [ ( "sgf", Encode.string (Sgf.encodeTree Sgf.empty.root forest) )
                        , ( "move", Encode.int 4 )
                        , ( "path", Encode.list Encode.int [ 0, 1, 0, 0 ] )
                        ]
                    )
                    |> Maybe.map (\( review, record, game ) -> ( review.selected, List.length record.plays, ( List.length game.record, Board.size game.board ) ))
                    |> Expect.equal (Just ( [ 0, 1, 0, 0 ], 4, ( 4, 13 ) ))
        , test "Invalid saved paths fall back to the game" <|
            \_ ->
                Review.fromTrees forest |> Review.select [ -1, 900 ] |> .selected |> Expect.equal [ 0, 0, 0 ]
        ]
