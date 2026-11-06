module Training exposing (Annotation, annotation, empty, viewEvaluation, viewMenu, viewPolicy, viewToggle)

{-| Evaluations belong to the move just played, from the mover's perspective.
Policy belongs to the position before that move, so replay shows the next node's
alternatives. Missing/null annotations are not estimates of zero.
-}

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Rules.Board as Board exposing (Board)
import Rules.Position exposing (..)
import Sgf
import StoneView
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias Annotation =
    { winPct : Maybe Float
    , score : Maybe Float
    , policy : List ( String, Int )
    }


empty : Annotation
empty =
    { winPct = Nothing, score = Nothing, policy = [] }


annotation : List Sgf.Property -> Annotation
annotation properties =
    properties
        |> List.filter (Tuple.first >> (==) "C")
        |> List.concatMap Tuple.second
        |> List.filterMap (Decode.decodeString decoder >> Result.toMaybe)
        |> List.head
        |> Maybe.withDefault empty


decoder : Decode.Decoder Annotation
decoder =
    Decode.map3 Annotation
        (Decode.maybe (Decode.field "win_pct" Decode.float))
        (Decode.maybe (Decode.field "score" Decode.float))
        (Decode.oneOf
            [ Decode.field "policy"
                (Decode.list
                    (Decode.map2 Tuple.pair
                        (Decode.index 0 Decode.string)
                        (Decode.index 1 Decode.int)
                    )
                )
            , Decode.succeed []
            ]
        )


viewEvaluation : Maybe Color -> Annotation -> H.Html msg
viewEvaluation mover evaluation =
    let
        player =
            case mover of
                Just Black ->
                    "Black"

                Just White ->
                    "White"

                Nothing ->
                    "Mover"

        badge id value tooltip =
            H.div [ HA.class "item" ]
                [ H.div
                    [ HA.id id
                    , HA.class "icon training-value"
                    , HA.attribute "aria-label" tooltip
                    ]
                    [ H.text value, H.span [ HA.class "tooltip" ] [ H.text (" " ++ tooltip) ] ]
                ]
    in
    H.div [ HA.class "training-evaluation" ]
        (List.filterMap identity
            [ Maybe.map (\value -> badge "training-win-pct" (String.fromFloat value ++ "%") (player ++ " win_pct")) evaluation.winPct
            , Maybe.map (\value -> badge "training-score" (String.fromFloat value) (player ++ " expected score margin")) evaluation.score
            ]
        )


viewToggle : msg -> Bool -> List ( String, Int ) -> H.Html msg
viewToggle toggle active policy =
    if List.isEmpty policy then
        H.text ""

    else
        H.div [ HA.class "item" ]
            [ H.button
                [ HA.id "policy-toggle"
                , HA.classList [ ( "icon", True ), ( "active", active ) ]
                , HA.attribute "aria-label" "Toggle policy alternatives"
                , HA.attribute "aria-pressed"
                    (if active then
                        "true"

                     else
                        "false"
                    )
                , HE.onClick toggle
                ]
                [ H.text "p", H.span [ HA.class "tooltip" ] [ H.text " policy alternatives" ] ]
            ]


viewMenu : msg -> Bool -> Color -> Annotation -> List ( String, Int ) -> H.Html msg
viewMenu toggle active color evaluation policy =
    if evaluation.winPct == Nothing && evaluation.score == Nothing && List.isEmpty policy then
        H.text ""

    else
        H.div [ HA.class "training-menu" ]
            [ H.div [ HA.class "item menu-spacer", HA.attribute "aria-hidden" "true" ] []
            , viewEvaluation (Just (otherColor color)) evaluation
            , viewToggle toggle active policy
            , if active then
                viewPass color policy

              else
                H.text ""
            ]


policyOpacity : List ( String, Int ) -> Int -> String
policyOpacity policy visits =
    let
        maximum =
            List.map Tuple.second policy |> List.maximum |> Maybe.withDefault 1 |> max 1
    in
    String.fromFloat (0.85 * toFloat (max 0 visits) / toFloat maximum)


viewPass : Color -> List ( String, Int ) -> H.Html msg
viewPass color policy =
    policy
        |> List.filter (Tuple.first >> String.isEmpty)
        |> List.head
        |> Maybe.map
            (\( _, visits ) ->
                H.div [ HA.class "item" ]
                    [ H.div
                        [ HA.id "policy-pass"
                        , HA.class "icon training-value policy-pass"
                        , HA.attribute "aria-label" ("Pass: " ++ String.fromInt visits ++ " playouts")
                        , HA.style "opacity" (policyOpacity policy visits)
                        , HA.style "background-color"
                            (if color == Black then
                                "#222"

                             else
                                "#FFF"
                            )
                        , HA.style "color"
                            (if color == Black then
                                "#FFF"

                             else
                                "#222"
                            )
                        ]
                        [ H.span [ HA.class "pass-label" ] [ H.text "pass" ]
                        , H.text (String.fromInt visits)
                        ]
                    ]
            )
        |> Maybe.withDefault (H.text "")


viewPolicy : Board -> Color -> List ( String, Int ) -> Svg msg
viewPolicy board color policy =
    let
        radius =
            Board.radius board

        alternative ( coordinate, visits ) =
            Sgf.decodePlay
                ((if color == Black then
                    "B"

                  else
                    "W"
                 )
                    ++ "["
                    ++ coordinate
                    ++ "]"
                )
                |> Maybe.andThen
                    (\play ->
                        case play.move of
                            Place stone ->
                                Just stone

                            Pass ->
                                Nothing
                    )
                |> Maybe.map
                    (\stone ->
                        Svg.g
                            [ SA.class
                                (if color == Black then
                                    "policy-black"

                                 else
                                    "policy-white"
                                )
                            , SA.opacity (policyOpacity policy visits)
                            ]
                            [ Svg.title [] [ Svg.text (coordinate ++ ": " ++ String.fromInt visits ++ " playouts") ]
                            , StoneView.viewStone board "" stone
                            , Svg.text_
                                [ SA.x (String.fromInt stone.spot.x)
                                , SA.y (String.fromInt stone.spot.y)
                                , SA.fontSize (String.fromFloat (toFloat radius * 0.75))
                                ]
                                [ Svg.text (String.fromInt visits) ]
                            ]
                    )
    in
    Svg.g [ SA.id "policy-alternatives", SA.pointerEvents "none" ]
        (policy |> List.sortBy Tuple.second |> List.filterMap alternative)
