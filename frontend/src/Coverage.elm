module Coverage exposing (Geometry, Point, canCover, geometry, libertyGeometry, view, viewLiberties, viewSpots)

{-| Sweep the physically free integer stone centres by one stone radius.
The rules' row intervals preserve even isolated touching spots and discard
fractional pockets that cannot actually hold a stone. Liberty areas restrict
those same centres to the group's strict adjacency discs, without checking
suicide or ko.

For compact SVGs, consecutive centres on each row are joined by round strokes.
This smooths sub-coordinate scallops (less than 0.004 units at radius 38), but
never invents a row or an opening with no integer centre.

-}

import BoardGeometry
import Rules.Board as Board exposing (Board, coordRange)
import Rules.Liberties exposing (Span, openRowsOn)
import Rules.Position exposing (Spot, Stones, stoneList)
import StoneView exposing (spotKey)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed


type alias Point =
    { x : Float, y : Float }


type alias Geometry =
    { board : Board
    , reach : Maybe (List Spot)
    , spans : List Span
    }


geometry : Board -> List Spot -> Geometry
geometry board spots =
    { board = board, reach = Nothing, spans = openRowsOn board spots Nothing }


libertyGeometry : Board -> List Spot -> List Spot -> Geometry
libertyGeometry board spots group =
    let
        extent =
            toFloat (Board.diameter board) + BoardGeometry.adjacentDistance board

        nearby =
            List.filter (\s -> List.any (\member -> BoardGeometry.distance s member <= extent) group) spots
    in
    { board = board, reach = Just group, spans = openRowsOn board nearby (Just group) }


{-| Test the same smoothed sweep that the SVG draws, not move legality.
-}
canCover : Geometry -> Point -> Bool
canCover shape p =
    let
        near span =
            let
                dx =
                    p.x - clamp (toFloat span.left) (toFloat span.right) p.x

                dy =
                    p.y - toFloat span.y
            in
            dx * dx + dy * dy <= toFloat (Board.radius shape.board) ^ 2 + 0.0000001
    in
    p.x >= 0 && p.x <= toFloat coordRange && p.y >= 0 && p.y <= toFloat coordRange && List.any near shape.spans


boardRect : List (Svg.Attribute msg) -> Svg msg
boardRect attributes =
    Svg.rect (SA.x "0" :: SA.y "0" :: SA.width (String.fromInt coordRange) :: SA.height (String.fromInt coordRange) :: attributes) []


view : Board -> Stones -> Svg msg
view board stones =
    viewSpots board (List.map .spot (stoneList stones))


{-| Occupancy geometry is colour-independent, including neutral analysis circles.
-}
viewSpots : Board -> List Spot -> Svg msg
viewSpots board spots =
    let
        positionKey =
            String.fromInt (Board.size board) ++ "/" ++ (List.map spotKey spots |> String.join "")
    in
    -- Replacing the mask prevents browsers from retaining stale SVG mask tiles.
    Svg.Keyed.node "g"
        [ SA.id "dead-zones", SA.pointerEvents "none" ]
        [ ( positionKey, viewGeometry (geometry board spots) ) ]


{-| Bound liberty masks to the group's reach plus a stone radius. Full-board
mask textures become particularly expensive at mobile pinch zoom.
-}
regionBounds : Geometry -> List (Svg.Attribute msg)
regionBounds shape =
    let
        centers =
            Maybe.withDefault [] shape.reach

        extent =
            BoardGeometry.adjacentDistance shape.board + toFloat (Board.radius shape.board)

        lower field =
            List.map (\p -> toFloat (field p) - extent) centers |> List.minimum |> Maybe.withDefault 0 |> max 0

        upper field =
            List.map (\p -> toFloat (field p) + extent) centers |> List.maximum |> Maybe.withDefault (toFloat coordRange) |> min (toFloat coordRange)
    in
    [ SA.x (String.fromFloat (lower .x))
    , SA.y (String.fromFloat (lower .y))
    , SA.width (String.fromFloat (upper .x - lower .x))
    , SA.height (String.fromFloat (upper .y - lower .y))
    ]


sweptMask : Bool -> String -> Geometry -> Svg msg
sweptMask inverted name shape =
    let
        spanPath span =
            "M " ++ String.fromInt span.left ++ " " ++ String.fromInt span.y ++ " H " ++ String.fromInt span.right
    in
    Svg.mask (SA.id name :: SA.maskUnits "userSpaceOnUse" :: regionBounds shape)
        [ boardRect
            [ SA.fill
                (if inverted then
                    "white"

                 else
                    "black"
                )
            ]
        , Svg.path
            [ SA.d (String.join " " (List.map spanPath shape.spans))
            , SA.fill "none"
            , SA.stroke
                (if inverted then
                    "black"

                 else
                    "white"
                )
            , SA.strokeWidth (String.fromInt (Board.diameter shape.board))
            , SA.strokeLinecap "round"
            ]
            []
        ]


viewGeometry : Geometry -> Svg msg
viewGeometry shape =
    Svg.g []
        [ Svg.defs [] [ sweptMask True "stone-swept-area" shape ]
        , boardRect [ SA.id "dead-zone-fill", SA.mask "url(#stone-swept-area)" ]
        ]


viewLiberties : Board -> Stones -> List Spot -> Svg msg
viewLiberties board stones group =
    let
        spots =
            List.map .spot (stoneList stones)

        positionKey =
            String.fromInt (Board.size board) ++ "/" ++ (List.map spotKey spots |> String.join "") ++ "/" ++ (List.map spotKey group |> String.join "")
    in
    Svg.Keyed.node "g"
        [ SA.id "liberties", SA.pointerEvents "none" ]
        (if List.isEmpty group then
            []

         else
            let
                shape =
                    libertyGeometry board spots group
            in
            [ ( positionKey
              , Svg.g []
                    [ Svg.defs [] [ sweptMask False "liberty-swept-area" shape ]
                    , Svg.rect (SA.id "liberty-area-fill" :: SA.mask "url(#liberty-swept-area)" :: regionBounds shape) []
                    ]
              )
            ]
        )
