module DocumentCoords exposing (DocumentCoords, toBoardSpot)

import Board
import Browser.Dom as BD
import Go


type alias DocumentCoords =
    { x : Float, y : Float }


toBoardSpot : DocumentCoords -> BD.Element -> Go.Spot
toBoardSpot documentCoords element =
    { x =
        round <|
            (documentCoords.x - element.element.x)
                * (toFloat Board.coordRange / element.element.width)
    , y =
        round <|
            (documentCoords.y - element.element.y)
                * (toFloat Board.coordRange / element.element.height)
    }
