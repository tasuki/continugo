module Sgf exposing (decode, decodePlay, encode)

import List.Extra
import Regex
import Rules.Position exposing (..)



-- "Sgf", now that's rich...


lowercaseAlphabet =
    "abcdefghijklmnopqrstuvwxyz"


coordinateAlphabet : List Char
coordinateAlphabet =
    -- 26 * 2 = 52 characters
    String.toList <| lowercaseAlphabet ++ String.toUpper lowercaseAlphabet


alphabetSize : Int
alphabetSize =
    List.length coordinateAlphabet


encodePlay : Play -> String
encodePlay play =
    let
        charFromIndex : Int -> String
        charFromIndex index =
            List.drop index coordinateAlphabet
                |> List.head
                |> Maybe.withDefault '*'
                |> String.fromChar

        coord : Int -> String
        coord c =
            (charFromIndex <| c // alphabetSize) ++ (charFromIndex <| modBy alphabetSize c)

        colorCode =
            case play.color of
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
    colorCode ++ encodedPlay


playRegex : Regex.Regex
playRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^([BW])\\[([a-zA-Z]{2})?([a-zA-Z]{2})?\\]$"


decodePlay : String -> Maybe Play
decodePlay encodedPlay =
    let
        charToIndex : Char -> Maybe Int
        charToIndex c =
            List.Extra.findIndex ((==) c) coordinateAlphabet

        maybeCoord : String -> Maybe Int
        maybeCoord chars =
            case String.toList chars |> List.map charToIndex of
                [ Just a, Just b ] ->
                    Just <| a * alphabetSize + b

                _ ->
                    Nothing

        maybeColor : String -> Maybe Color
        maybeColor str =
            case str of
                "B" ->
                    Just Black

                "W" ->
                    Just White

                _ ->
                    Nothing

        submatches : List (Maybe String)
        submatches =
            Regex.find playRegex encodedPlay
                |> List.concatMap .submatches
    in
    case submatches of
        [ Just colorCode, Just xEncoded, Just yEncoded ] ->
            case ( maybeColor colorCode, maybeCoord xEncoded, maybeCoord yEncoded ) of
                ( Just color, Just x, Just y ) ->
                    Just { color = color, move = Place <| createStone color { x = x, y = y } }

                _ ->
                    Nothing

        [ Just colorCode, Nothing, Nothing ] ->
            Maybe.map (\color -> { color = color, move = Pass }) (maybeColor colorCode)

        _ ->
            Nothing


encode : List Play -> String
encode plays =
    String.join ";" <| List.map encodePlay plays


decode : String -> List Play
decode sgf =
    String.split ";" sgf |> List.filterMap decodePlay
