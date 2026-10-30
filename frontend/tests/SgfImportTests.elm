module SgfImportTests exposing (tests)

import Expect
import Html.Attributes as HA
import Main
import Rules.Board as Board
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


tests : Test
tests =
    describe "SGF file import"
        [ test "Imported games retain their size, full record and metadata" <|
            \_ ->
                [ 9, 13, 19 ]
                    |> List.map
                        (\size ->
                            Main.decodeImport ("(;FF[9071]GM[9071]SZ[" ++ String.fromInt size ++ "]PB[Trainer];B[bWdS];W[])")
                                |> Result.map
                                    (\( board, record ) ->
                                        ( Board.size board, List.length record.plays, List.member ( "PB", [ "Trainer" ] ) record.root )
                                    )
                        )
                    |> Expect.equal [ Ok ( 9, 2, True ), Ok ( 13, 2, True ), Ok ( 19, 2, True ) ]
        , test "Malformed SGF and invalid board sizes are rejected" <|
            \_ ->
                [ "invalid", "(;FF[9071]GM[9071]SZ[0])", "(;FF[9071]GM[9071]SZ[501])" ]
                    |> List.map (Main.decodeImport >> Result.toMaybe)
                    |> Expect.equal [ Nothing, Nothing, Nothing ]
        , test "The menu has a drop target and displays import errors" <|
            \_ ->
                Main.viewNewGames (Just "Bad SGF")
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.find [ Selector.id "sgf-dropzone" ]
                            >> Query.has [ Selector.text "Drop an SGF file here" ]
                        , Query.find [ Selector.attribute (HA.attribute "role" "alert") ]
                            >> Query.has [ Selector.text "Bad SGF" ]
                        ]
        ]
