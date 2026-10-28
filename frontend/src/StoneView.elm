module StoneView exposing (classJustPlayed, getStoneLinks, getUniqueLinks, spotBorderNearestTo, spotKey, viewGhostLink, viewGhostStone, viewHighlight, viewKeyedLineCover, viewKeyedLink, viewKeyedStone, viewLink, viewStagedStone, viewStone)

import BoardGeometry exposing (distance)
import Rules.Board exposing (stoneRadius)
import Rules.Position exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Lazy exposing (lazy, lazy2)


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


spotBorderNearestTo : Spot -> Spot -> Spot
spotBorderNearestTo otherSpot spot =
    let
        factor =
            toFloat stoneRadius / distance spot otherSpot

        roundAwayFromZero x =
            if x < 0 then
                floor x

            else
                ceiling x

        coord s os =
            s + (roundAwayFromZero <| factor * (toFloat <| os - s))
    in
    Spot (coord spot.x otherSpot.x) (coord spot.y otherSpot.y)


viewLineCover : Stone -> Svg msg
viewLineCover { spot } =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt <| stoneRadius * 7
        ]
        []


viewStone : String -> Stone -> Svg msg
viewStone extraClass { color, spot } =
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
        , SA.r <| String.fromInt stoneRadius
        , SA.class class
        , SA.class extraClass
        ]
        []


viewHighlight : Spot -> Svg msg
viewHighlight spot =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneRadius
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


viewStagedStone : Stone -> Svg msg
viewStagedStone stone =
    viewStone "" stone


viewGhostStone : Stone -> Svg msg
viewGhostStone stone =
    viewStone "" stone


viewGhostLink : ( Spot, Spot ) -> Svg msg
viewGhostLink ( ghost, existing ) =
    viewLink ( spotBorderNearestTo existing ghost, existing )


spotKey : Spot -> String
spotKey s =
    "-" ++ String.fromInt s.x ++ "-" ++ String.fromInt s.y


viewKeyedLineCover : Stone -> ( String, Svg msg )
viewKeyedLineCover stone =
    ( "hide" ++ spotKey stone.spot
    , lazy viewLineCover stone
    )


viewKeyedStone : String -> Stone -> ( String, Svg msg )
viewKeyedStone extraClass stone =
    ( "stone" ++ spotKey stone.spot
    , lazy2 viewStone extraClass stone
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
