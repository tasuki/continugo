module Liberties exposing (..)

import Go exposing (..)



-- settings and hardcoded things


steps =
    20


type Shift
    = Shift Spot


neighborShifts : List Shift
neighborShifts =
    let
        toSpot ( x, y ) =
            Shift
                { x = round <| 1.2 * x * stoneR
                , y = round <| 1.2 * y * stoneR
                }
    in
    List.map toSpot
        [ ( 0, -2 )
        , ( sqrt 3, -1 )
        , ( sqrt 3, 1 )
        , ( 0, 2 )
        , ( -(sqrt 3), 1 )
        , ( -(sqrt 3), -1 )
        ]



-- general spot manipulation


shift : Spot -> Shift -> Spot
shift spot (Shift shiftBy) =
    Spot (spot.x + shiftBy.x) (spot.y + shiftBy.y)


neighborSpots : Spot -> List Spot
neighborSpots spot =
    List.map (shift spot) neighborShifts


findShift : Spot -> Spot -> Shift
findShift from to =
    Shift { x = to.x - from.x, y = to.y - from.y }


scaleShift : Float -> Shift -> Shift
scaleShift factor (Shift s) =
    Shift
        { x = round <| toFloat s.x * factor
        , y = round <| toFloat s.y * factor
        }



-- the forces


stoneForce : Int -> Spot -> Spot -> Shift
stoneForce step suspect nearbySpot =
    -- the 1.2 and 1.1 are just arbitrary:
    -- we want the `1-x` shifted ever slightly up/right
    let
        relativeDistance =
            distance suspect nearbySpot / diameter

        factor =
            if relativeDistance < 1 then
                1.2 - relativeDistance * 1.1

            else
                0
    in
    findShift nearbySpot suspect
        |> scaleShift (factor * toFloat (step + steps) / (steps + steps))


boardForce : Spot -> Shift
boardForce spot =
    let
        outBy coord =
            if coord <= boardMin then
                boardMin - coord

            else if coord >= boardMax then
                boardMax - coord

            else
                0
    in
    Shift { x = outBy spot.x, y = outBy spot.y }


connectionForce : Spot -> Spot -> Shift
connectionForce original suspect =
    -- jump back to the vicinity of the original from wherever we got pushed to
    let
        actualDistance =
            distance original suspect
    in
    if actualDistance / diameter > adjacentDistance then
        findShift suspect original
            |> scaleShift (actualDistance / diameter - adjacentDistance)

    else
        Shift { x = 0, y = 0 }


applyForces : Int -> Spot -> List Spot -> Spot -> Spot
applyForces step original nearbySpots =
    let
        shiftFromOverlap s =
            List.map (stoneForce step s) nearbySpots
                |> List.foldl (\spot shiftBy -> shift shiftBy spot) s

        shiftToBoard s =
            boardForce s |> shift s

        shiftToConnection s =
            connectionForce original s |> shift s
    in
    shiftFromOverlap >> shiftToBoard >> shiftToConnection



-- filtering and sorting the results


overlapCount : List Spot -> Spot -> Int
overlapCount otherLibs liberty =
    List.map (\otherLib -> distance liberty otherLib < diameter) otherLibs
        |> List.filter identity
        |> List.length


sortByMostOverlaps : List Spot -> List Spot
sortByMostOverlaps liberties =
    -- we want to find the spots that take away as much liberty as possible
    List.sortBy (overlapCount liberties) liberties |> List.reverse


takeNonOverlapping : List Spot -> List Spot -> List Spot
takeNonOverlapping acc queue =
    -- take unique liverty spots
    case queue of
        [] ->
            acc

        head :: tail ->
            if overlaps head acc then
                takeNonOverlapping acc tail

            else
                takeNonOverlapping (head :: acc) tail



-- gluing it all together


findLiberty : List Spot -> Spot -> Int -> Spot -> Maybe Spot
findLiberty nearbySpots orig step suspect =
    let
        isAdjacent : Float -> Bool
        isAdjacent dst =
            diameter < dst && dst < diameter * adjacentDistance
    in
    if isWithinBoard suspect && isAdjacent (distance suspect orig) && (not <| overlaps suspect nearbySpots) then
        Just suspect

    else if step > 0 then
        findLiberty nearbySpots orig (step - 1) (applyForces step orig nearbySpots suspect)

    else
        Nothing


findLiberties : Stone -> List Spot -> List Spot
findLiberties { spot } nearbySpots =
    List.filterMap (findLiberty nearbySpots spot steps) (neighborSpots spot)
        |> sortByMostOverlaps
        |> takeNonOverlapping []
