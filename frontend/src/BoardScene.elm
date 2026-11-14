module BoardScene exposing (PositionMode(..), boardViewBox, viewBackground, viewLastMove, viewPosition)

{-| Shared board rendering for the application and the standalone viewer.
-}

import Analysis
import BoardView
import Coverage
import Dict
import MovePreview
import Rules.Board as Board exposing (Board, coordRange)
import Rules.Position exposing (..)
import StoneView exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import Svg.Lazy exposing (..)
import TerritoryView


{-| Keep the unchanged blur in a separate SVG. Promoting individual SVG groups
can leave stale mask tiles after pinch zoom; the background needs no masks.
-}
viewBackground : Board -> Stones -> Svg msg
viewBackground board stones =
    Svg.svg [ SA.id "board-background", SA.viewBox boardViewBox ]
        [ Svg.defs []
            [ Svg.filter [ SA.id "blur-filter" ]
                [ Svg.feGaussianBlur [ SA.in_ "SourceGraphic", SA.stdDeviation "70" ] [] ]
            ]
        , lazy3 Svg.node "g" [ SA.id "lines" ] <| BoardView.viewLines board
        , lazy3 Svg.node "g" [ SA.id "stars" ] <| BoardView.viewStars board
        , lazy3 Svg.Keyed.node "g" [ SA.id "hide-lines", SA.filter "url(#blur-filter)" ] <|
            List.map (viewKeyedLineCover board) (stoneList stones)
        ]


boardViewBox : String
boardViewBox =
    "0 0 " ++ String.fromInt coordRange ++ " " ++ String.fromInt coordRange


type PositionMode
    = Normal
    | Staged
    | Scoring


{-| Pointer coordinates and drag bookkeeping do not change the position. Keep
this whole subtree lazy so those updates cannot rebuild previews or SVG masks.
-}
viewPosition : Board -> Stones -> Maybe MovePreview.Preview -> List Spot -> Maybe Spot -> List Stone -> PositionMode -> Analysis.Model -> Svg msg
viewPosition board stones candidate group justPlayed justRemoved mode analysis =
    let
        scoring =
            mode == Scoring

        preview =
            if scoring then
                Nothing

            else
                candidate

        previewStones =
            Maybe.map .stones preview |> Maybe.withDefault stones

        previewStone =
            Maybe.map .stone preview

        highlightedGroup =
            Maybe.map .group preview |> Maybe.withDefault group

        survivingLink ( a, b ) =
            Dict.member ( a.x, a.y ) previewStones && Dict.member ( b.x, b.y ) previewStones

        stoneClass stone =
            if Dict.member (stoneKey stone) previewStones then
                classJustPlayed justPlayed stone

            else
                "pending-capture"
    in
    Svg.g []
        [ if scoring then
            lazy TerritoryView.view stones

          else
            lazy4 Analysis.viewDeadZones board previewStones analysis.circles analysis.preview
        , if scoring then
            Svg.g [] []

          else
            lazy3 Coverage.viewLiberties board previewStones highlightedGroup
        , lazy3 Svg.node "g" [ SA.id "ghost-links" ] <|
            (previewStone
                |> Maybe.map (\stone -> getStoneLinks stone ++ getBorderLinks board stone)
                |> Maybe.withDefault []
                |> List.map (viewGhostLink board)
            )
        , lazy3 Svg.node "g" [ SA.id "ghost-stone" ] <|
            List.filterMap identity [ Maybe.map (viewGhostStone board) previewStone ]
        , lazy3 Svg.Keyed.node "g" [ SA.id "links" ] <|
            List.map viewKeyedLink
                (List.filter survivingLink (getUniqueLinks stones)
                    ++ (stoneList stones
                            |> List.filter (\stone -> Dict.member (stoneKey stone) previewStones)
                            |> List.concatMap (getBorderLinks board)
                       )
                )
        , lazy3 Svg.Keyed.node "g" [ SA.id "stones" ] <|
            (stoneList stones |> List.map (\s -> viewKeyedStone board (stoneClass s) s))
        , lazy3 Svg.node "g" [ SA.id "staged-stone" ] <|
            if mode == Staged then
                List.filterMap identity [ Maybe.map (viewStagedStone board) previewStone ]

            else
                []
        , lazy3 Svg.node "g" [ SA.id "capture-preview", SA.pointerEvents "none" ] <|
            List.map (viewCapturePreview board) (Maybe.map .captured preview |> Maybe.withDefault [])
        , lazy3 Svg.node "g" [ SA.id "removed-stones" ] <|
            (justRemoved |> List.map (viewStone board "removed"))
        , lazy3 Svg.node "g" [ SA.id "highlights" ] <|
            (highlightedGroup
                |> List.filter (\spot -> Just spot /= Maybe.map .spot previewStone)
                |> List.map (viewHighlight board)
            )
        ]


viewLastMove : Board -> List Play -> Svg msg
viewLastMove board record =
    case List.head record |> Maybe.map .move of
        Just (Place stone) ->
            Svg.circle
                [ SA.id "last-move"
                , SA.cx (String.fromInt stone.spot.x)
                , SA.cy (String.fromInt stone.spot.y)
                , SA.r (String.fromFloat (toFloat (Board.radius board) / 4))
                , SA.pointerEvents "none"
                ]
                []

        _ ->
            Svg.g [] []


viewCapturePreview : Board -> Stone -> Svg msg
viewCapturePreview board { spot } =
    let
        offset =
            Board.radius board // 2
    in
    Svg.g []
        [ viewLink ( Spot (spot.x - offset) (spot.y - offset), Spot (spot.x + offset) (spot.y + offset) )
        , viewLink ( Spot (spot.x - offset) (spot.y + offset), Spot (spot.x + offset) (spot.y - offset) )
        ]
