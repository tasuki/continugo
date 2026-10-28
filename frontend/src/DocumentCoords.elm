module DocumentCoords exposing (DocumentCoords, toBoardSpot)

import Browser.Dom as BD
import Rules.Board as Board
import Rules.Position as Position


type alias DocumentCoords =
    { x : Float, y : Float }


toBoardSpot : DocumentCoords -> BD.Element -> Position.Spot
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
