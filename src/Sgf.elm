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


encodeMove : Stone -> String
encodeMove stone =
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
            case stone.player of
                Black ->
                    "B"

                White ->
                    "W"
    in
    player ++ "[" ++ coord stone.spot.x ++ coord stone.spot.y ++ "]"


moveRegex : Regex.Regex
moveRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^([BW])\\[([a-zA-Z]{2})([a-zA-Z]{2})\\]$"


decodeMove : String -> Maybe Stone
decodeMove move =
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

        maybeMatch : List (Maybe String)
        maybeMatch =
            Regex.find moveRegex move
                |> List.concatMap .submatches
    in
    case maybeMatch of
        [ Just player, Just xEncoded, Just yEncoded ] ->
            case ( maybePlayer player, maybeCoord xEncoded, maybeCoord yEncoded ) of
                ( Just p, Just x, Just y ) ->
                    Just <| createStone p { x = x, y = y }

                _ ->
                    Nothing

        _ ->
            Nothing


encode : List Stone -> String
encode stoneList =
    String.join ";" <| List.map encodeMove stoneList


decode : String -> List Stone
decode sgf =
    String.split ";" sgf |> List.filterMap decodeMove
