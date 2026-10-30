module Sgf exposing (Property, Record, boardFromRoot, decode, decodePlay, decodeRecord, empty, encode, encodeRecord, onBoard)

{-| Single-sequence ContinuGo SGF, with a move-free FF[9071]GM[9071] root.
Coordinates use two base-52 digits per axis. Variations are not supported.
The record API preserves root properties; the play-only API uses a default
13x13 root. Empty PB, PW and RE properties are omitted when writing.
Legacy compact records remain readable.
Decoding checks syntax, not move legality, and does not apply board metadata.
-}

import List.Extra
import Regex
import Rules.Board as Board exposing (Board)
import Rules.Position exposing (..)


type alias Property =
    ( String, List String )


type alias Record =
    { root : List Property
    , plays : List Play
    }


empty : Record
empty =
    onBoard Board.default


onBoard : Board -> Record
onBoard board =
    { root =
        [ ( "FF", [ "9071" ] )
        , ( "GM", [ "9071" ] )
        , ( "SZ", [ String.fromInt (Board.size board) ] )
        ]
    , plays = []
    }


boardFromRoot : List Property -> Maybe Board
boardFromRoot root =
    case List.Extra.find (Tuple.first >> (==) "SZ") root of
        Nothing ->
            Just Board.default

        Just ( _, [ value ] ) ->
            String.toInt value |> Maybe.andThen Board.fromSize

        _ ->
            Nothing


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
                |> List.filter (\match -> match.match == encodedPlay)
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
    encodeRecord { empty | plays = plays }


encodeRecord : Record -> String
encodeRecord record =
    let
        property ( name, values ) =
            name
                ++ String.concat
                    (List.map
                        (\value ->
                            "[" ++ (value |> String.replace "\\" "\\\\" |> String.replace "]" "\\]") ++ "]"
                        )
                        values
                    )

        metadata =
            record.root
                |> List.filter
                    (\( name, values ) ->
                        name
                            /= "FF"
                            && name
                            /= "GM"
                            && not (List.member name [ "PB", "PW", "RE" ] && List.all String.isEmpty values)
                    )
                |> List.map property
                |> String.concat
    in
    "(;FF[9071]GM[9071]"
        ++ metadata
        ++ String.concat (List.map (\play -> "\n;" ++ encodePlay play) record.plays)
        ++ "\n)"


decode : String -> Result String (List Play)
decode =
    decodeRecord >> Result.map .plays


decodeRecord : String -> Result String Record
decodeRecord sgf =
    if String.startsWith "(" (String.trimLeft sgf) then
        String.toList sgf
            |> expect '('
            |> Result.andThen (expect ';')
            |> Result.andThen parseProperties
            |> Result.andThen
                (\( root, rest ) ->
                    validateRoot root
                        |> Result.andThen (\_ -> parseNodes [] rest)
                        |> Result.map (\plays -> { root = root, plays = plays })
                )

    else
        decodeCompact sgf |> Result.map (\plays -> { empty | plays = plays })


decodeCompact : String -> Result String (List Play)
decodeCompact sgf =
    if String.isEmpty sgf then
        Ok []

    else
        String.split ";" sgf
            |> List.indexedMap
                (\index entry ->
                    decodePlay entry
                        |> Result.fromMaybe
                            ("Invalid SGF entry " ++ String.fromInt (index + 1) ++ ": " ++ entry)
                )
            |> List.foldr (Result.map2 (::)) (Ok [])


validateRoot : List Property -> Result String ()
validateRoot root =
    if not (List.member ( "FF", [ "9071" ] ) root) then
        Err "Expected FF[9071] in SGF root"

    else if not (List.member ( "GM", [ "9071" ] ) root) then
        Err "Expected GM[9071] in SGF root"

    else if List.any (\( name, _ ) -> name == "B" || name == "W") root then
        Err "SGF root must not contain a move"

    else
        Ok ()


skipWhitespace : List Char -> List Char
skipWhitespace =
    List.Extra.dropWhile (\char -> List.member char [ ' ', '\t', '\u{000D}', '\n', '\u{000B}', '\u{000C}' ])


expect : Char -> List Char -> Result String (List Char)
expect expected chars =
    case skipWhitespace chars of
        first :: rest ->
            if first == expected then
                Ok rest

            else
                Err ("Expected '" ++ String.fromChar expected ++ "' in SGF")

        [] ->
            Err ("Expected '" ++ String.fromChar expected ++ "' in SGF")


parseProperties : List Char -> Result String ( List Property, List Char )
parseProperties =
    parsePropertiesHelp []


parsePropertiesHelp : List Property -> List Char -> Result String ( List Property, List Char )
parsePropertiesHelp properties chars =
    let
        trimmed =
            skipWhitespace chars

        ( nameChars, rest ) =
            List.Extra.span (\char -> char >= 'A' && char <= 'Z') trimmed

        name =
            String.fromList nameChars
    in
    if List.isEmpty nameChars then
        Ok ( List.reverse properties, trimmed )

    else if List.any (Tuple.first >> (==) name) properties then
        Err ("Duplicate SGF property: " ++ name)

    else
        expect '[' rest
            |> Result.andThen (parseValue [])
            |> Result.andThen (\( value, remaining ) -> parseValues [ value ] remaining)
            |> Result.andThen
                (\( values, remaining ) ->
                    parsePropertiesHelp (( name, values ) :: properties) remaining
                )


parseValues : List String -> List Char -> Result String ( List String, List Char )
parseValues values chars =
    case skipWhitespace chars of
        '[' :: rest ->
            parseValue [] rest
                |> Result.andThen (\( value, remaining ) -> parseValues (value :: values) remaining)

        rest ->
            Ok ( List.reverse values, rest )


parseValue : List Char -> List Char -> Result String ( String, List Char )
parseValue value chars =
    case chars of
        ']' :: rest ->
            Ok ( String.fromList (List.reverse value), rest )

        '\\' :: '\u{000D}' :: '\n' :: rest ->
            parseValue value rest

        '\\' :: '\n' :: '\u{000D}' :: rest ->
            parseValue value rest

        '\\' :: '\u{000D}' :: rest ->
            parseValue value rest

        '\\' :: '\n' :: rest ->
            parseValue value rest

        '\\' :: escaped :: rest ->
            parseValue (escaped :: value) rest

        '\u{000D}' :: '\n' :: rest ->
            parseValue ('\n' :: value) rest

        '\u{000D}' :: rest ->
            parseValue ('\n' :: value) rest

        '\\' :: [] ->
            Err "Unterminated SGF property value"

        char :: rest ->
            parseValue (char :: value) rest

        [] ->
            Err "Unterminated SGF property value"


parseNodes : List Play -> List Char -> Result String (List Play)
parseNodes plays chars =
    case skipWhitespace chars of
        ';' :: rest ->
            parseProperties rest
                |> Result.andThen
                    (\( properties, remaining ) ->
                        nodePlay properties
                            |> Result.andThen (\play -> parseNodes (play :: plays) remaining)
                    )

        ')' :: rest ->
            if List.isEmpty (skipWhitespace rest) then
                Ok (List.reverse plays)

            else
                Err "Unexpected content after SGF tree"

        _ ->
            Err "Expected ')' or a move node in SGF (variations are not supported)"


nodePlay : List Property -> Result String Play
nodePlay properties =
    case List.filter (\( name, _ ) -> name == "B" || name == "W") properties of
        [ ( name, [ value ] ) ] ->
            decodePlay (name ++ "[" ++ value ++ "]")
                |> Result.fromMaybe ("Invalid SGF move: " ++ name ++ "[" ++ value ++ "]")

        _ ->
            Err "Each non-root SGF node must contain exactly one move with one value"
