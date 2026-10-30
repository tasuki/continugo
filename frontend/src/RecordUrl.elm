module RecordUrl exposing (decode, encode)

import Sgf
import Url


encode : Sgf.Record -> String
encode record =
    "?record=" ++ Url.percentEncode (Sgf.encodeRecord record)


decode : Url.Url -> Maybe Sgf.Record
decode url =
    url.query
        |> Maybe.andThen
            (\query ->
                if String.startsWith "record=" query then
                    String.dropLeft 7 query
                        |> Url.percentDecode
                        |> Maybe.andThen (Sgf.decodeRecord >> Result.toMaybe)

                else
                    Nothing
            )
