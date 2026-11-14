module ViewerTests exposing (tests)

import Dict
import Expect
import Rules.Board as Board
import Rules.Position exposing (Spot)
import Test exposing (..)
import Viewer


config : Viewer.Config
config =
    { sgf = "(;FF[9071]GM[9071]SZ[9];B[fOfO];W[nyny])"
    , move = Nothing
    , showPolicy = False
    , hover = True
    }


initial : Viewer.Model
initial =
    Viewer.init config |> Tuple.first


send : Viewer.Msg -> Viewer.Model -> Viewer.Model
send message model =
    Viewer.update message model |> Tuple.first


tests : Test
tests =
    describe "Standalone viewer"
        [ test "Defaults to the final position and respects the SGF board size" <|
            \_ ->
                Expect.equal ( ( 2, 2, 9 ), Nothing )
                    ( ( List.length initial.game.record, Dict.size initial.game.stones, Board.size initial.game.board ), initial.error )
        , test "Replay clamps the requested move and never changes the supplied record" <|
            \_ ->
                [ 0, 1, 99 ]
                    |> List.map
                        (\move ->
                            let
                                model =
                                    send (Viewer.Configure { config | move = Just move }) initial
                            in
                            ( List.length model.game.record, Dict.size model.game.stones, List.length model.record.plays )
                        )
                    |> Expect.equal [ ( 0, 0, 2 ), ( 1, 1, 2 ), ( 2, 2, 2 ) ]
        , test "Navigation reuses cached positions and changing SGF resets the cache" <|
            \_ ->
                let
                    back =
                        send (Viewer.Configure { config | move = Just 0 }) initial

                    forward =
                        send (Viewer.Configure { config | move = Just 1 }) back

                    replacement =
                        send (Viewer.Configure { config | sgf = "(;FF[9071]GM[9071]SZ[13];B[])", move = Just 0 }) forward
                in
                Expect.equal ( ( True, True ), [ 0 ], 13 )
                    ( ( initial.positions == back.positions, initial.positions == forward.positions )
                    , Dict.keys replacement.positions
                    , Board.size replacement.game.board
                    )
        , test "Hover previews a move without playing it, and leaving clears it" <|
            \_ ->
                let
                    hovered =
                        send (Viewer.Pointer (Just { x = 0.5, y = 0.5 })) initial

                    cleared =
                        send (Viewer.Pointer Nothing) hovered
                in
                Expect.equal ( Just (Spot 500 500), initial.game, Nothing )
                    ( Maybe.map (.stone >> .spot) hovered.ghostStone, hovered.game, cleared.ghostStone )
        , test "Hovering an existing stone highlights its group" <|
            \_ ->
                send (Viewer.Pointer (Just { x = 0.3, y = 0.3 })) initial
                    |> .highlightedGroup
                    |> Expect.equal [ Spot 300 300 ]
        , test "Disabling hover or changing position clears previews" <|
            \_ ->
                let
                    hovered =
                        send (Viewer.Pointer (Just { x = 0.5, y = 0.5 })) initial

                    disabled =
                        hovered
                            |> send (Viewer.Configure { config | hover = False })
                            |> send (Viewer.Pointer (Just { x = 0.5, y = 0.5 }))

                    replayed =
                        send (Viewer.Configure { config | move = Just 0 }) hovered
                in
                Expect.equal ( Nothing, Nothing, 0 )
                    ( disabled.ghostStone, replayed.ghostStone, Dict.size replayed.game.stones )
        , test "Bad input reports an error and a subsequent valid update recovers" <|
            \_ ->
                let
                    invalid =
                        send (Viewer.Configure { config | sgf = "not an SGF" }) initial

                    unsupported =
                        send (Viewer.Configure { config | sgf = "(;FF[9071]GM[9071]SZ[0])" }) initial

                    recovered =
                        send (Viewer.Configure config) invalid
                in
                Expect.equal ( ( True, True ), Nothing, 2 )
                    ( ( invalid.error /= Nothing, unsupported.error /= Nothing ), recovered.error, Dict.size recovered.game.stones )
        ]
