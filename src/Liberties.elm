module Liberties exposing (..)

import Go exposing (..)



-- settings and hardcoded things


steps =
    20


neighborShifts : List Spot
neighborShifts =
    let
        toSpot ( x, y ) =
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


shift : Spot -> Spot -> Spot
shift shiftBy spot =
    { x = spot.x + shiftBy.x, y = spot.y + shiftBy.y }


neighborSpots : Spot -> List Spot
neighborSpots spot =
    List.map (shift spot) neighborShifts


findShift : Spot -> Spot -> Spot
findShift from to =
    { x = to.x - from.x, y = to.y - from.y }


scaleShift : Float -> Spot -> Spot
scaleShift factor spot =
    { x = round <| toFloat spot.x * factor
    , y = round <| toFloat spot.y * factor
    }



-- the forces


stoneForce : Int -> Spot -> Spot -> Spot
stoneForce step suspect stoneSpot =
    -- the 1.2 and 1.1 are just arbitrary:
    -- we want the `1-x` shifted ever slightly up/right
    let
        relativeDistance =
            distance suspect stoneSpot / diameter

        factor =
            if relativeDistance < 1 then
                1.2 - relativeDistance * 1.1

            else
                0
    in
    findShift stoneSpot suspect
        |> scaleShift (factor * toFloat step / steps)


connectionForce : Spot -> Spot -> Spot
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
        { x = 0, y = 0 }



-- filtering and sorting the results


overlapCount : List Spot -> Spot -> Int
overlapCount others lib =
    List.map (\o -> distance lib o < diameter) others
        |> List.filter identity
        |> List.length


sortByMostOverlaps : List Spot -> List Spot
sortByMostOverlaps liberties =
    -- we want to find the spots that take away as much liberty as possible
    List.sortBy (overlapCount liberties) liberties |> List.reverse


takeNonOverlapping : List Spot -> List Spot -> List Spot
takeNonOverlapping acc liberties =
    -- take unique liverty spots
    case liberties of
        [] ->
            acc

        head :: tail ->
            if overlaps head acc then
                takeNonOverlapping acc tail

            else
                takeNonOverlapping (head :: acc) tail



-- gluing it all together


findLiberties : Stone -> List Stone -> List Spot
findLiberties { player, spot } surroundingStones =
    let
        surroundingSpots : List Spot
        surroundingSpots =
            List.map (\s -> s.spot) surroundingStones

        applyForces : Int -> Spot -> Spot
        applyForces step suspect =
            let
                shiftByStones : Spot
                shiftByStones =
                    List.map (stoneForce step suspect) surroundingSpots
                        |> List.foldl shift suspect
            in
            connectionForce spot shiftByStones
                |> shift shiftByStones

        isLiberty : Spot -> Bool
        isLiberty suspect =
            distance suspect spot < diameter * adjacentDistance

        findLiberty : Int -> Spot -> Maybe Spot
        findLiberty step suspect =
            if isLiberty suspect && (not <| overlaps suspect surroundingSpots) then
                Just suspect

            else if step > 0 then
                findLiberty (step - 1) (applyForces step suspect)

            else
                Nothing
    in
    List.filterMap (findLiberty steps) (neighborSpots spot)
        |> sortByMostOverlaps
        |> takeNonOverlapping []
