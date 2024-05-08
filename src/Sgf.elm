module Sgf exposing (decode, encode)

import Go exposing (..)
import List.Extra
import Regex



-- "Sgf", now that's rich...


alphabet =
    "abcdefghijklmnopqrstuvwxyz"


indexes : List Char
indexes =
    -- 26 * 2 = 52 characters
    String.toList <| alphabet ++ String.toUpper alphabet


indexLen : Int
indexLen =
    List.length indexes


encodePlay : Play -> String
encodePlay play =
    let
        charFromIndex : Int -> String
        charFromIndex drop =
            List.drop drop indexes
                |> List.head
                |> Maybe.withDefault '*'
                |> String.fromChar

        coord : Int -> String
        coord c =
            (charFromIndex <| c // indexLen) ++ (charFromIndex <| modBy indexLen c)

        player =
            case play.player of
                Black ->
                    "B"

                White ->
                    "W"

        encodedPlay =
            case play.move of
                Place stone ->
                    "[" ++ coord stone.spot.x ++ coord stone.spot.y ++ "]"

                Pass ->
                    "[]"
    in
    player ++ encodedPlay


playRegex : Regex.Regex
playRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^([BW])\\[([a-zA-Z]{2})?([a-zA-Z]{2})?\\]$"


decodePlay : String -> Maybe Play
decodePlay move =
    let
        charToIndex : Char -> Maybe Int
        charToIndex c =
            List.Extra.findIndex ((==) c) indexes

        maybeCoord : String -> Maybe Int
        maybeCoord chars =
            case String.toList chars |> List.map charToIndex of
                [ Just a, Just b ] ->
                    Just <| a * indexLen + b

                _ ->
                    Nothing

        maybePlayer : String -> Maybe Player
        maybePlayer str =
            case str of
                "B" ->
                    Just Black

                "W" ->
                    Just White

                _ ->
                    Nothing

        maybePlay : List (Maybe String)
        maybePlay =
            Regex.find playRegex move
                |> List.concatMap .submatches
    in
    case maybePlay of
        [ Just player, Just xEncoded, Just yEncoded ] ->
            case ( maybePlayer player, maybeCoord xEncoded, maybeCoord yEncoded ) of
                ( Just p, Just x, Just y ) ->
                    Just { player = p, move = Place <| createStone p { x = x, y = y } }

                _ ->
                    Nothing

        [ Just player, Nothing, Nothing ] ->
            Maybe.map (\p -> { player = p, move = Pass }) (maybePlayer player)

        _ ->
            Nothing


encode : List Play -> String
encode plays =
    String.join ";" <| List.map encodePlay plays


decode : String -> List Play
decode sgf =
    String.split ";" sgf |> List.filterMap decodePlay
