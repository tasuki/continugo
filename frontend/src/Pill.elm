module Pill exposing (view)

import Html as H exposing (Attribute, Html)
import Html.Attributes as HA


view : String -> List (Attribute msg) -> String -> String -> Html msg
view tag attributes summary label =
    H.node tag
        ([ HA.class "icon", HA.tabindex 0, HA.attribute "aria-label" label ] ++ attributes)
        [ H.span [ HA.class "pill-summary", HA.attribute "aria-hidden" "true" ] [ H.text summary ]
        , H.span [ HA.class "tooltip", HA.attribute "aria-hidden" "true" ] [ H.text label ]
        ]
