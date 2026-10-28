module TerritoryView exposing (formatArea, resultString, view)

import Rules.Board exposing (stoneRadius)
import Rules.Position exposing (Player(..), Stones)
import Rules.Territory as Territory exposing (Areas, Outcome(..))
import Svg exposing (Svg)
import Svg.Attributes as SA


stoneArea : Float
stoneArea =
    pi * toFloat stoneRadius ^ 2


{-| Express board area in stone-disc areas, to one decimal place.
-}
formatArea : Float -> String
formatArea area =
    String.fromFloat (toFloat (round (area / stoneArea * 10)) / 10)


resultString : Areas -> String
resultString territory =
    case Territory.result territory of
        Draw ->
            "jigo"

        Win player margin ->
            let
                displayedMargin =
                    if margin / stoneArea < 0.1 then
                        "<0.1"

                    else
                        formatArea margin

                prefix =
                    case player of
                        Black ->
                            "B+"

                        White ->
                            "W+"
            in
            prefix ++ displayedMargin


view : Stones -> Svg msg
view stones =
    let
        viewCell cell =
            Svg.polygon
                [ SA.points
                    (cell.polygon
                        |> List.map (\p -> String.fromFloat p.x ++ "," ++ String.fromFloat p.y)
                        |> String.join " "
                    )
                , SA.class
                    (case cell.player of
                        Black ->
                            "territory-black"

                        White ->
                            "territory-white"
                    )
                ]
                []
    in
    Svg.g [ SA.id "territory", SA.pointerEvents "none" ] (List.map viewCell (Territory.cells stones))
