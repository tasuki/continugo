module Territory exposing (Areas, areas, formatArea, resultString, view)

{-| Voronoi area scoring, independent of ContinuGo's connection and capture rules.
Each surviving stone owns the board points nearest to its centre. Its cell is
built by clipping the board against every other stone's perpendicular bisector.
The cells already include occupied space: there is no extra stone-count score.
-}

import Board exposing (coordRange, stoneRadius)
import Go exposing (Player(..), Spot, Stones, stoneList)
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias Point =
    { x : Float, y : Float }


type alias Areas =
    { black : Float, white : Float }


boardArea : Float
boardArea =
    toFloat coordRange ^ 2


board : List Point
board =
    let
        size =
            toFloat coordRange
    in
    [ Point 0 0, Point size 0, Point size size, Point 0 size ]


{-| Keep the half-plane closer to `site` than to `other`. Subtracting the
bisector's midpoint avoids subtracting large squared distances.
-}
clip : Spot -> Spot -> List Point -> List Point
clip site other polygon =
    case List.reverse polygon of
        [] ->
            []

        last :: _ ->
            let
                dx =
                    toFloat (other.x - site.x)

                dy =
                    toFloat (other.y - site.y)

                mx =
                    toFloat (site.x + other.x) / 2

                my =
                    toFloat (site.y + other.y) / 2

                side p =
                    (p.x - mx) * dx + (p.y - my) * dy

                crossing a b =
                    let
                        t =
                            side a / (side a - side b)
                    in
                    Point (a.x + t * (b.x - a.x)) (a.y + t * (b.y - a.y))

                step current ( previous, reversed ) =
                    ( current
                    , case ( side previous <= 0, side current <= 0 ) of
                        ( True, True ) ->
                            current :: reversed

                        ( True, False ) ->
                            crossing previous current :: reversed

                        ( False, True ) ->
                            current :: crossing previous current :: reversed

                        ( False, False ) ->
                            reversed
                    )
            in
            List.foldl step ( last, [] ) polygon
                |> Tuple.second
                |> List.reverse


polygonArea : List Point -> Float
polygonArea polygon =
    case polygon of
        [] ->
            0

        first :: rest ->
            List.map2 (\a b -> a.x * b.y - a.y * b.x) polygon (rest ++ [ first ])
                |> List.sum
                |> abs
                |> (\twiceArea -> twiceArea / 2)


type alias Cell =
    { player : Player, polygon : List Point }


cells : Stones -> List Cell
cells stones =
    let
        all =
            stoneList stones
    in
    List.map
        (\stone ->
            Cell stone.player (List.foldl (\other -> clip stone.spot other.spot) board all)
        )
        all


areas : Stones -> Areas
areas stones =
    let
        add cell total =
            let
                area =
                    polygonArea cell.polygon
            in
            case cell.player of
                Black ->
                    { total | black = total.black + area }

                White ->
                    { total | white = total.white + area }
    in
    List.foldl add (Areas 0 0) (cells stones)


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
    let
        margin =
            territory.black - territory.white

        displayedMargin =
            if abs margin / stoneArea < 0.1 then
                "<0.1"

            else
                formatArea (abs margin)
    in
    -- Ignore only floating-point noise in an otherwise tied diagram. Decide
    -- the winner before rounding the displayed margin, not after it.
    if abs margin <= boardArea * 0.0000000001 then
        "jigo"

    else if margin > 0 then
        "B+" ++ displayedMargin

    else
        "W+" ++ displayedMargin


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
    Svg.g [ SA.id "territory", SA.pointerEvents "none" ] (List.map viewCell (cells stones))
