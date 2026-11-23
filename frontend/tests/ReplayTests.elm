module ReplayTests exposing (tests)

import BoardScene
import Dict
import Expect
import Fixtures.Play as PlayFixtures
import Html.Attributes as HA
import Json.Decode as Decode
import Json.Encode as Encode
import Main
import Rules.Board as Board
import Rules.Game as Game
import Rules.Position exposing (..)
import Sgf
import Svg
import Svg.Attributes as SA
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Url


tests : Test
tests =
    describe "Replay"
        [ test "Menu symbols and arrow keys trigger their actions" <|
            \_ ->
                [ ( "!", Main.OpenMenu )
                , ( "?", Main.ToggleHelp )
                , ( "*", Main.PassTurn )
                , ( "%", Main.ToggleScoring )
                , ( "o", Main.ToggleAnalysis )
                , ( "p", Main.TogglePolicy )
                , ( "ArrowLeft", Main.Prev )
                , ( "←", Main.Prev )
                , ( "ArrowRight", Main.Next )
                , ( "→", Main.Next )
                , ( "ArrowUp", Main.SwitchVariation -1 )
                , ( "ArrowDown", Main.SwitchVariation 1 )
                , ( "Escape", Main.ClosePanels )
                ]
                    |> List.map
                        (\( key, action ) ->
                            ( shortcut False False [ ( "key", Encode.string key ) ], Just action )
                        )
                    |> List.unzip
                    |> (\( actual, expected ) -> Expect.equal expected actual)
        , test "Typing, browser shortcuts, repeated toggles and unrelated keys are ignored" <|
            \_ ->
                [ [ ( "key", Encode.string "a" ) ]
                , [ ( "key", Encode.string "↓" ) ]
                , [ ( "key", Encode.string "p" ), ( "ctrlKey", Encode.bool True ) ]
                , [ ( "key", Encode.string "p" ), ( "metaKey", Encode.bool True ) ]
                , [ ( "key", Encode.string "ArrowLeft" ), ( "altKey", Encode.bool True ) ]
                , [ ( "key", Encode.string "*" ), ( "repeat", Encode.bool True ) ]
                , [ ( "key", Encode.string "o" ), ( "target", Encode.object [ ( "tagName", Encode.string "INPUT" ) ] ) ]
                , [ ( "key", Encode.string "p" ), ( "target", Encode.object [ ( "isContentEditable", Encode.bool True ) ] ) ]
                ]
                    |> List.filterMap (shortcut False False)
                    |> Expect.equal []
        , test "Help can close with ?, panels with Escape, and neither allows gameplay shortcuts" <|
            \_ ->
                [ shortcut False True [ ( "key", Encode.string "?" ) ]
                , shortcut False True [ ( "key", Encode.string "Escape" ) ]
                , shortcut True False [ ( "key", Encode.string "Escape" ) ]
                , shortcut False True [ ( "key", Encode.string "*" ) ]
                , shortcut True False [ ( "key", Encode.string "ArrowRight" ) ]
                ]
                    |> Expect.equal [ Just Main.ToggleHelp, Just Main.ClosePanels, Just Main.ClosePanels, Nothing, Nothing ]
        , test "Stepping is bounded by the start and end, including passes" <|
            \_ ->
                let
                    record =
                        [ createPass Black, createPass White ]
                in
                Expect.equal
                    [ [], [], [ createPass Black ], List.reverse record, List.reverse record ]
                    (List.map (\move -> (Main.gameAt Board.default move record).record) [ -1, 0, 1, 2, 3 ])
        , test "Moving across a capture reconstructs the stones and turn" <|
            \_ ->
                PlayFixtures.withCase "capture-four.txt" <|
                    \tc ->
                        let
                            record =
                                tc.record ++ [ createPlay tc.playStone.color tc.playStone.spot ]

                            before =
                                Main.gameAt tc.board (List.length record - 1) record

                            after =
                                Main.gameAt tc.board (List.length record) record
                        in
                        Expect.equal ( -3, White, False )
                            ( Dict.size after.stones - Dict.size before.stones
                            , after.onMove
                            , Game.isFinished after
                            )
        , test "The last-move ring follows the displayed replay position" <|
            \_ ->
                let
                    record =
                        [ createPlay Black (Spot 200 200), createPlay White (Spot 500 500) ]
                in
                Expect.all
                    (List.map
                        (\( move, coordinate ) _ ->
                            let
                                game =
                                    Main.gameAt Board.default move record
                            in
                            BoardScene.viewLastMove game.board game.record
                                |> List.singleton
                                |> Svg.svg []
                                |> Query.fromHtml
                                |> Query.find [ Selector.id "last-move" ]
                                |> Query.has
                                    [ Selector.attribute (SA.cx (String.fromInt coordinate))
                                    , Selector.attribute (SA.cy (String.fromInt coordinate))
                                    , Selector.attribute (SA.r (String.fromFloat (toFloat (Board.radius game.board) / 4)))
                                    , Selector.attribute (SA.pointerEvents "none")
                                    ]
                        )
                        [ ( 1, 200 ), ( 2, 500 ) ]
                    )
                    ()
        , test "No last-move ring before the first move or after a pass" <|
            \_ ->
                Expect.all
                    (List.map
                        (\record _ ->
                            BoardScene.viewLastMove Board.default record
                                |> List.singleton
                                |> Svg.svg []
                                |> Query.fromHtml
                                |> Query.findAll [ Selector.id "last-move" ]
                                |> Query.count (Expect.equal 0)
                        )
                        [ [], [ createPass White, createPlay Black (Spot 200 200) ] ]
                    )
                    ()
        , describe "Current move badge"
            ([ ( 0, "No moves yet" ), ( 1, "bWdS" ), ( 2, "pass" ) ]
                |> List.map
                    (\( move, coordinates ) ->
                        test ("Shows move " ++ String.fromInt move ++ " and its coordinates") <|
                            \_ ->
                                let
                                    game =
                                        Main.gameAt Board.default move [ createPlay Black (Spot 100 200), createPass White ]
                                in
                                Main.viewMoveNumber game.record
                                    |> Query.fromHtml
                                    |> Expect.all
                                        [ Query.find [ Selector.id "move-number" ]
                                            >> Query.has [ Selector.text (String.fromInt move) ]
                                        , Query.find [ Selector.class "tooltip" ]
                                            >> Query.has [ Selector.text coordinates ]
                                        ]
                    )
            )
        , test "A saved session restores its board, metadata, cursor and future moves" <|
            \_ ->
                Main.readSession (snapshot "(;FF[9071]GM[9071]SZ[19]PB[Alice];B[bbbb];W[])" 1)
                    |> Maybe.map
                        (\( record, game ) ->
                            ( ( Board.size game.board, List.length game.record )
                            , List.length record.plays
                            , List.member ( "PB", [ "Alice" ] ) record.root
                            )
                        )
                    |> Expect.equal (Just ( ( 19, 1 ), 2, True ))
        , test "Saved cursors are bounded by the game" <|
            \_ ->
                [ -1, 99 ]
                    |> List.map
                        (\move ->
                            Main.readSession (snapshot "B[];W[]" move)
                                |> Maybe.map (Tuple.second >> .record >> List.length)
                        )
                    |> Expect.equal [ Just 0, Just 2 ]
        , test "Missing, malformed or invalid-board sessions are ignored" <|
            \_ ->
                [ Encode.null
                , Encode.string "bad"
                , snapshot "invalid" 0
                , snapshot "(;FF[9071]GM[9071]SZ[0])" 0
                ]
                    |> List.map (Main.readSession >> Maybe.map (Tuple.second >> .record))
                    |> Expect.equal [ Nothing, Nothing, Nothing, Nothing ]
        , test "Player pills show initials and full SGF names in black/white order" <|
            \_ ->
                Main.viewPlayers [ ( "PW", [ "White Player" ] ), ( "PB", [ "Black Player" ] ) ]
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.children []
                            >> Query.index 0
                            >> Query.has [ Selector.class "player-black", Selector.attribute (HA.attribute "aria-label" "Black: Black Player") ]
                        , Query.children []
                            >> Query.index 1
                            >> Query.has [ Selector.class "player-white", Selector.attribute (HA.attribute "aria-label" "White: White Player") ]
                        , Query.find [ Selector.class "player-black", Selector.class "icon" ]
                            >> Query.find [ Selector.class "pill-summary" ]
                            >> Query.has [ Selector.text "B" ]
                        , Query.find [ Selector.class "player-white", Selector.class "icon" ]
                            >> Query.find [ Selector.class "tooltip" ]
                            >> Query.has [ Selector.text "White Player" ]
                        ]
        , test "Explicit SGF komi appears below the players" <|
            \_ ->
                Main.viewPlayers [ ( "PB", [ "Black Player" ] ), ( "PW", [ "White Player" ] ), ( "KM", [ "6.3" ] ) ]
                    |> Query.fromHtml
                    |> Query.children []
                    |> Expect.all
                        [ Query.count (Expect.equal 3)
                        , Query.index 2
                            >> Query.has [ Selector.id "komi", Selector.class "pill-white", Selector.attribute (HA.attribute "aria-label" "Komi: 6.3") ]
                        , Query.index 2
                            >> Query.find [ Selector.class "pill-summary" ]
                            >> Query.has [ Selector.text "6.3" ]
                        ]
        , test "Explicit zero komi is shown even without player names" <|
            \_ ->
                Main.viewPlayers [ ( "KM", [ "0" ] ) ]
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "komi" ]
                    |> Query.has [ Selector.class "pill-white", Selector.text "0" ]
        , test "Negative komi uses Black's colour and an unsigned summary" <|
            \_ ->
                Main.viewPlayers [ ( "KM", [ "-6.3" ] ) ]
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "komi" ]
                    |> Expect.all
                        [ Query.has [ Selector.class "pill-black", Selector.attribute (HA.attribute "aria-label" "Komi: -6.3") ]
                        , Query.find [ Selector.class "pill-summary" ]
                            >> Query.has [ Selector.exactText "6.3" ]
                        , Query.find [ Selector.class "tooltip" ]
                            >> Query.has [ Selector.exactText "Komi: -6.3" ]
                        ]
        , test "Missing, blank, malformed and nonfinite komi have no pill" <|
            \_ ->
                Expect.all
                    (List.map
                        (\root _ ->
                            Main.viewPlayers root
                                |> Query.fromHtml
                                |> Query.findAll [ Selector.id "komi" ]
                                |> Query.count (Expect.equal 0)
                        )
                        [ [], [ ( "KM", [ " " ] ) ], [ ( "KM", [ "bad" ] ) ], [ ( "KM", [ "NaN" ] ) ], [ ( "KM", [ "Infinity" ] ) ] ]
                    )
                    ()
        , test "Missing and blank player names have no pills" <|
            \_ ->
                Main.viewPlayers [ ( "PB", [ "  " ] ) ]
                    |> Query.fromHtml
                    |> Query.children []
                    |> Query.count (Expect.equal 0)
        , test "A game with one named player only shows that player" <|
            \_ ->
                Main.viewPlayers [ ( "PW", [ "白" ] ) ]
                    |> Query.fromHtml
                    |> Query.children []
                    |> Expect.all
                        [ Query.count (Expect.equal 1)
                        , Query.index 0 >> Query.has [ Selector.class "player-white", Selector.text "白" ]
                        ]
        , test "Download includes all moves and root metadata" <|
            \_ ->
                let
                    record =
                        { root = Sgf.empty.root ++ [ ( "RE", [ "B+2.5" ] ) ]
                        , plays = [ createPlay Black (Spot 200 200), createPass White ]
                        , moveProperties = [ [ ( "C", [ "{\"win_pct\":49.9,\"score\":0.5}" ] ) ], [] ]
                        }
                in
                Main.viewDownload record.root record.plays record.moveProperties
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "a" ]
                    |> Query.has
                        [ Selector.attribute (HA.download "continugo.sgf")
                        , Selector.attribute (HA.href ("data:application/x-go-sgf;charset=utf-8," ++ Url.percentEncode (Sgf.encodeRecord record)))
                        ]
        ]


shortcut : Bool -> Bool -> List ( String, Encode.Value ) -> Maybe Main.Msg
shortcut showMenu showHelp fields =
    Decode.decodeValue (Main.menuKey { showMenu = showMenu, showHelp = showHelp }) (Encode.object fields)
        |> Result.toMaybe


snapshot : String -> Int -> Encode.Value
snapshot sgf move =
    Encode.object [ ( "sgf", Encode.string sgf ), ( "move", Encode.int move ) ]
