module Liberties exposing (..)

import Go exposing (..)



-- settings and hardcoded things


steps =
    20


neighborShifts : List Coords
neighborShifts =
    let
        toCoords ( x, y ) =
            { x = round <| 1.2 * x * stoneR
            , y = round <| 1.2 * y * stoneR
            }
    in
    List.map toCoords
        [ ( 0, -2 )
        , ( sqrt 3, -1 )
        , ( sqrt 3, 1 )
        , ( 0, 2 )
        , ( -(sqrt 3), 1 )
        , ( -(sqrt 3), -1 )
        ]



-- general coord manipulation


shift : Coords -> Coords -> Coords
shift shiftBy coords =
    { x = coords.x + shiftBy.x, y = coords.y + shiftBy.y }


neighborCoords : Coords -> List Coords
neighborCoords coords =
    List.map (shift coords) neighborShifts


findShift : Coords -> Coords -> Coords
findShift from to =
    { x = to.x - from.x, y = to.y - from.y }


scaleShift : Float -> Coords -> Coords
scaleShift factor coords =
    { x = round <| toFloat coords.x * factor
    , y = round <| toFloat coords.y * factor
    }



-- the forces


stoneForce : Int -> Coords -> Coords -> Coords
stoneForce step suspect coord =
    -- the 1.2 and 1.1 are just arbitrary:
    -- we want the `1-x` shifted ever slightly up/right
    let
        relativeDistance =
            distance suspect coord / diameter

        factor =
            if relativeDistance < 1 then
                1.2 - relativeDistance * 1.1

            else
                0
    in
    findShift coord suspect
        |> scaleShift (factor * toFloat step / steps)


connectionForce : Coords -> Coords -> Coords
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


overlapCount : List Coords -> Coords -> Int
overlapCount others lib =
    List.map (\o -> distance lib o < diameter) others
        |> List.filter identity
        |> List.length


sortByMostOverlaps : List Coords -> List Coords
sortByMostOverlaps liberties =
    -- we want to find the spots that take away as much liberty as possible
    List.sortBy (overlapCount liberties) liberties |> List.reverse


takeNonOverlapping : List Coords -> List Coords -> List Coords
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


findLiberties : Stone -> List Stone -> List Coords
findLiberties { player, coords } surroundingStones =
    let
        surroundingCoords : List Coords
        surroundingCoords =
            List.map (\s -> s.coords) surroundingStones

        applyForces : Int -> Coords -> Coords
        applyForces step suspect =
            let
                shiftByStones : Coords
                shiftByStones =
                    List.map (stoneForce step suspect) surroundingCoords
                        |> List.foldl shift suspect
            in
            connectionForce coords shiftByStones
                |> shift shiftByStones

        isLiberty : Coords -> Bool
        isLiberty suspect =
            distance suspect coords < diameter * adjacentDistance

        findLiberty : Int -> Coords -> Maybe Coords
        findLiberty step suspect =
            if isLiberty suspect && (not <| overlaps suspect surroundingCoords) then
                Just suspect

            else if step > 0 then
                findLiberty (step - 1) (applyForces step suspect)

            else
                Nothing
    in
    List.filterMap (findLiberty steps) (neighborCoords coords)
        |> sortByMostOverlaps
        |> takeNonOverlapping []
