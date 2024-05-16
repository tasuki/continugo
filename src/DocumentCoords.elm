module DocumentCoords exposing (..)

import Json.Decode as D


type ActionSource
    = Mouse
    | Touch


type alias DocumentCoords =
    { x : Float, y : Float, source : ActionSource }


coordsDecoder : ActionSource -> D.Decoder DocumentCoords
coordsDecoder source =
    D.map3 DocumentCoords
        (D.field "pageX" D.float)
        (D.field "pageY" D.float)
        (D.succeed source)


decodeMouse : (DocumentCoords -> msg) -> D.Decoder msg
decodeMouse msg =
    D.map msg <| coordsDecoder Mouse


tupleDecoder : ( D.Decoder a, D.Decoder b ) -> D.Decoder ( a, b )
tupleDecoder ( a, b ) =
    D.map2 Tuple.pair a b


singleTouchDecoder : msg -> (DocumentCoords -> msg) -> D.Decoder ( msg, Bool )
singleTouchDecoder clearTouch actionMsg =
    let
        decodeTouches : Int -> ( D.Decoder msg, D.Decoder Bool )
        decodeTouches n =
            case n of
                1 ->
                    ( D.map actionMsg <| D.field "0" <| coordsDecoder Touch
                    , D.succeed True
                    )

                _ ->
                    ( D.succeed clearTouch
                    , D.succeed False
                    )
    in
    D.field "length" D.int |> D.andThen (decodeTouches >> tupleDecoder)


decodeSingleTouch : msg -> (DocumentCoords -> msg) -> D.Decoder ( msg, Bool )
decodeSingleTouch clearTouch actionMsg =
    D.field "touches" <| singleTouchDecoder clearTouch actionMsg


decodeTouch : (DocumentCoords -> msg) -> D.Decoder msg
decodeTouch actionMsg =
    D.at [ "touches", "0" ] <| D.map actionMsg <| coordsDecoder Touch


decodeChangedTouch : (DocumentCoords -> msg) -> D.Decoder msg
decodeChangedTouch actionMsg =
    D.at [ "changedTouches", "0" ] <| D.map actionMsg <| coordsDecoder Touch
