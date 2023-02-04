module Liberties exposing (..)

import Go exposing (..)



-- settings and hardcoded things


libertiesSettings =
    { steps = 20
    , shifts = shifts12
    , stoneForce = \x -> 1.2 - x * 1.1
    , scaleFactor = \step -> toFloat step / toFloat libertiesSettings.steps
    }


nearestPlayableSettings =
    { steps = 50
    , originForce = 0.2
    , scaleFactor = \step -> toFloat step / toFloat nearestPlayableSettings.steps
    , maxDistance = stoneR
    }



-- shifts


type alias Shift =
    { x : Float, y : Float }


shifts6 =
    [ ( 0, -2 )
    , ( sqrt 3, -1 )
    , ( sqrt 3, 1 )
    , ( 0, 2 )
    , ( -(sqrt 3), 1 )
    , ( -(sqrt 3), -1 )
    ]


shifts12 =
    [ ( 0, -2 )
    , ( 1, -(sqrt 3) )
    , ( sqrt 3, -1 )
    , ( 2, 0 )
    , ( sqrt 3, 1 )
    , ( 1, sqrt 3 )
    , ( 0, 2 )
    , ( -1, sqrt 3 )
    , ( -(sqrt 3), 1 )
    , ( -2, 0 )
    , ( -(sqrt 3), -1 )
    , ( -1, -(sqrt 3) )
    ]


neighborShifts : List Shift
neighborShifts =
    let
        toShift ( x, y ) =
            { x = 1.2 * x * stoneR
            , y = 1.2 * y * stoneR
            }
    in
    List.map toShift libertiesSettings.shifts



-- general spot manipulation


roundAwayFromZero : Float -> Int
roundAwayFromZero x =
    if x < 0 then
        floor x

    else
        ceiling x


shift : Spot -> Shift -> Spot
shift spot shiftBy =
    { x = spot.x + roundAwayFromZero shiftBy.x
    , y = spot.y + roundAwayFromZero shiftBy.y
    }


sumShifts : List Shift -> Shift
sumShifts shifts =
    List.foldl
        (\s acc -> Shift (s.x + acc.x) (s.y + acc.y))
        (Shift 0 0)
        shifts


neighborSpots : Spot -> List Spot
neighborSpots spot =
    List.map (shift spot) neighborShifts


findShift : Spot -> Spot -> Shift
findShift from to =
    Shift (toFloat <| to.x - from.x) (toFloat <| to.y - from.y)


scaleShift : Float -> Shift -> Shift
scaleShift factor s =
    Shift (s.x * factor) (s.y * factor)



-- the forces


stoneForce : Spot -> Spot -> Shift
stoneForce suspect nearbySpot =
    let
        relativeDistance =
            distance suspect nearbySpot / diameter

        factor =
            if relativeDistance < 1 then
                libertiesSettings.stoneForce relativeDistance

            else
                0
    in
    findShift nearbySpot suspect
        |> scaleShift factor


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
    Shift (toFloat <| outBy spot.x) (toFloat <| outBy spot.y)


connectionForce : Spot -> Spot -> Shift
connectionForce original suspect =
    let
        actualDistance =
            distance original suspect
    in
    if actualDistance / diameter > adjacentDistance then
        findShift suspect original
            |> scaleShift (actualDistance / diameter - adjacentDistance)

    else
        Shift 0 0


libertyForces : Int -> Spot -> List Spot -> Spot -> Spot
libertyForces step original nearbySpots suspect =
    let
        shiftFromOverlap =
            List.map (stoneForce suspect) nearbySpots

        shiftToBoard =
            boardForce suspect

        shiftToConnection =
            connectionForce original suspect
    in
    sumShifts (shiftToBoard :: shiftToConnection :: shiftFromOverlap)
        |> scaleShift (libertiesSettings.scaleFactor step)
        |> shift suspect


playableForces : Int -> Spot -> List Spot -> Spot -> Spot
playableForces step origin nearbySpots suspect =
    let
        shiftFromOverlap =
            List.map (stoneForce suspect) nearbySpots

        shiftToBoard =
            boardForce suspect

        shiftToOrigin =
            findShift suspect origin |> scaleShift nearestPlayableSettings.originForce
    in
    sumShifts (shiftToBoard :: shiftToOrigin :: shiftFromOverlap)
        |> scaleShift (nearestPlayableSettings.scaleFactor step)
        |> shift suspect



-- filtering and sorting liberties


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


uniqueLiberties : List Spot -> List Spot
uniqueLiberties liberties =
    sortByMostOverlaps liberties |> takeNonOverlapping []



-- finding liberties


findLiberty : List Spot -> Spot -> Int -> Spot -> Maybe Spot
findLiberty nearbySpots orig step suspect =
    let
        isAdjacent : Bool
        isAdjacent =
            let
                dst =
                    distance orig suspect
            in
            diameter < dst && dst < diameter * adjacentDistance

        overlapsNearby : Bool
        overlapsNearby =
            overlaps suspect nearbySpots
    in
    if isWithinBoard suspect && isAdjacent && not overlapsNearby then
        Just suspect

    else if step > 0 then
        findLiberty
            nearbySpots
            orig
            (step - 1)
            (libertyForces step orig nearbySpots suspect)

    else
        Nothing


findLiberties : Stone -> List Spot -> List Spot
findLiberties { spot } nearbySpots =
    List.filterMap (findLiberty nearbySpots spot libertiesSettings.steps) (neighborSpots spot)
        |> uniqueLiberties



-- nearest playable


nearestPlayable : List Spot -> Spot -> Int -> Spot -> List Spot -> List Spot
nearestPlayable nearbySpots orig step suspect prevCandidates =
    let
        overlapsNearby =
            overlaps suspect nearbySpots

        closeEnough =
            distance orig suspect < nearestPlayableSettings.maxDistance

        candidates =
            if isWithinBoard suspect && not overlapsNearby && closeEnough then
                suspect :: prevCandidates

            else
                prevCandidates
    in
    if step > 0 then
        nearestPlayable
            nearbySpots
            orig
            (step - 1)
            (playableForces step orig nearbySpots suspect)
            candidates

    else
        candidates


findNearestPlayable : Spot -> List Spot -> Maybe Spot
findNearestPlayable spot nearbySpots =
    nearestPlayable nearbySpots spot nearestPlayableSettings.steps spot []
        |> List.head
