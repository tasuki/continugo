module StoneView exposing (classJustPlayed, getStoneLinks, getUniqueLinks, spotBorderNearestTo, spotKey, viewGhostLink, viewGhostStone, viewHighlight, viewKeyedLineCover, viewKeyedLink, viewKeyedStone, viewLink, viewStagedStone, viewStone)

import BoardGeometry exposing (distance)
import Rules.Board as Board exposing (Board)
import Rules.Position exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy exposing (lazy, lazy2, lazy3)


getStoneLinks : Stone -> List ( Spot, Spot )
getStoneLinks stone =
    List.map (\spots -> ( stone.spot, spots )) stone.adjacent


getUniqueLinks : Stones -> List ( Spot, Spot )
getUniqueLinks stones =
    let
        allLinks : List ( Spot, Spot )
        allLinks =
            stoneList stones
                |> List.concatMap (\s -> List.map (\a -> ( s.spot, a )) s.adjacent)

        -- filter out half of the duplicate links, so as to show each only once
        isChosen : ( Spot, Spot ) -> Bool
        isChosen ( s1, s2 ) =
            if s1.y < s2.y then
                True

            else
                s1.y == s2.y && s1.x < s2.x
    in
    List.filter isChosen allLinks


spotBorderNearestTo : Board -> Spot -> Spot -> Spot
spotBorderNearestTo board otherSpot spot =
    let
        factor =
            toFloat (Board.radius board) / distance spot otherSpot

        roundAwayFromZero x =
            if x < 0 then
                floor x

            else
                ceiling x

        coord s os =
            s + (roundAwayFromZero <| factor * (toFloat <| os - s))
    in
    Spot (coord spot.x otherSpot.x) (coord spot.y otherSpot.y)


viewLineCover : Board -> Stone -> Svg msg
viewLineCover board { spot } =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt <| Board.radius board * 7
        ]
        []


viewStone : Board -> String -> Stone -> Svg msg
viewStone board extraClass { color, spot } =
    let
        class =
            case color of
                Black ->
                    "black"

                White ->
                    "white"
    in
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt (Board.radius board)
        , SA.class class
        , SA.class extraClass
        ]
        []


viewHighlight : Board -> Spot -> Svg msg
viewHighlight board spot =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt (Board.radius board)
        ]
        []


viewLink : ( Spot, Spot ) -> Svg msg
viewLink ( s1, s2 ) =
    Svg.line
        [ SA.x1 <| String.fromInt s1.x
        , SA.y1 <| String.fromInt s1.y
        , SA.x2 <| String.fromInt s2.x
        , SA.y2 <| String.fromInt s2.y
        ]
        []


viewStagedStone : Board -> Stone -> Svg msg
viewStagedStone board stone =
    viewStone board "" stone


viewGhostStone : Board -> Stone -> Svg msg
viewGhostStone board stone =
    viewStone board "" stone


viewGhostLink : Board -> ( Spot, Spot ) -> Svg msg
viewGhostLink board ( ghost, existing ) =
    viewLink ( spotBorderNearestTo board existing ghost, existing )


spotKey : Spot -> String
spotKey s =
    "-" ++ String.fromInt s.x ++ "-" ++ String.fromInt s.y


viewKeyedLineCover : Board -> Stone -> ( String, Svg msg )
viewKeyedLineCover board stone =
    ( "hide" ++ spotKey stone.spot
    , lazy2 viewLineCover board stone
    )


viewKeyedStone : Board -> String -> Stone -> ( String, Svg msg )
viewKeyedStone board extraClass stone =
    ( "stone" ++ spotKey stone.spot
    , lazy3 viewStone board extraClass stone
    )


viewKeyedLink : ( Spot, Spot ) -> ( String, Svg msg )
viewKeyedLink ( s1, s2 ) =
    ( "link" ++ spotKey s1 ++ spotKey s2
    , lazy viewLink ( s1, s2 )
    )


classJustPlayed : Maybe Spot -> Stone -> String
classJustPlayed justPlayed stone =
    if justPlayed == Just stone.spot then
        "just-played"

    else
        ""
