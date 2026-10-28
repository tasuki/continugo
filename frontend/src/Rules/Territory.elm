module Rules.Territory exposing (Areas, Cell, Outcome(..), Point, areas, cells, result)

{-| Voronoi area scoring, independent of ContinuGo's connection and capture rules.
Each surviving stone owns the board points nearest to its centre. Its cell is
built by clipping the board against every other stone's perpendicular bisector.
The cells already include occupied space: there is no extra stone-count score.
-}

import Rules.Board exposing (coordRange)
import Rules.Position exposing (Color(..), Spot, Stones, stoneList)


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
    { color : Color, polygon : List Point }


cells : Stones -> List Cell
cells stones =
    let
        all =
            stoneList stones
    in
    List.map
        (\stone ->
            Cell stone.color (List.foldl (\other -> clip stone.spot other.spot) board all)
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
            case cell.color of
                Black ->
                    { total | black = total.black + area }

                White ->
                    { total | white = total.white + area }
    in
    List.foldl add (Areas 0 0) (cells stones)


type Outcome
    = Draw
    | Win Color Float


result : Areas -> Outcome
result territory =
    let
        margin =
            territory.black - territory.white
    in
    -- Ignore only floating-point noise in an otherwise tied diagram.
    if abs margin <= boardArea * 0.0000000001 then
        Draw

    else if margin > 0 then
        Win Black margin

    else
        Win White -margin
