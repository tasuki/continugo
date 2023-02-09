module Play exposing (groupAndItsLiberties, play, playIfLegal, playStones)

import Dict
import Go exposing (..)
import Liberties exposing (findLiberties, uniqueLiberties)
import List.Extra
import Set exposing (Set)


type alias Open =
    List Stone


type alias Closed =
    Set ( Int, Int )


spotsToStones : Stones -> List Spot -> List Stone
spotsToStones stones =
    List.filterMap (\spot -> Dict.get ( spot.x, spot.y ) stones)


newOpenClosed : Stones -> Stone -> Open -> Closed -> ( Open, Closed )
newOpenClosed stones toExplore open closed =
    let
        notClosed neighbor =
            Set.member (stoneKey neighbor) closed

        toOpen : List Stone
        toOpen =
            toExplore.adjacent
                |> spotsToStones stones
                |> List.Extra.filterNot notClosed

        newOpen : List Stone
        newOpen =
            toOpen ++ open

        newClosed =
            List.foldl (stoneKey >> Set.insert) closed toOpen
    in
    ( newOpen, newClosed )


findLibertyless : Stones -> ( Open, Closed ) -> List Spot
findLibertyless stones ( open, closed ) =
    case open of
        [] ->
            -- libertyless, return group
            Set.toList closed |> List.map (\( x, y ) -> Spot x y)

        toExplore :: exploreLater ->
            if findLiberties toExplore toExplore.nearby /= [] then
                -- has at least one liberty, stop and return nothing
                []

            else
                -- has no liberties but has neighbors
                findLibertyless stones <|
                    newOpenClosed stones toExplore exploreLater closed


findGroupAndItsLiberties : Stones -> List Spot -> ( Open, Closed ) -> ( List Spot, List Spot )
findGroupAndItsLiberties stones liberties ( open, closed ) =
    case open of
        [] ->
            -- that's it, we found the whole group
            ( Set.toList closed |> List.map (\( x, y ) -> Spot x y)
            , uniqueLiberties liberties
            )

        toExplore :: exploreLater ->
            -- wait there's more
            let
                newLiberties =
                    findLiberties toExplore toExplore.nearby
            in
            findGroupAndItsLiberties stones (newLiberties ++ liberties) <|
                newOpenClosed stones toExplore exploreLater closed


findGroupWithoutLiberties : Stones -> Stone -> List Spot
findGroupWithoutLiberties stones stone =
    findLibertyless stones ( [ stone ], Set.singleton <| stoneKey stone )


groupAndItsLiberties : Stones -> Stone -> ( List Spot, List Spot )
groupAndItsLiberties stones stone =
    findGroupAndItsLiberties stones [] ( [ stone ], Set.singleton <| stoneKey stone )


takeAll : List Spot -> Stones -> Stones
takeAll spots stones =
    let
        toRemove : List Stone
        toRemove =
            spotsToStones stones spots
                |> List.concatMap (findGroupWithoutLiberties stones)
                |> spotsToStones stones
    in
    List.foldl (\s acc -> removeStone s acc) stones toRemove


maybePlay : Stone -> Stones -> Maybe Stones
maybePlay stone stones =
    let
        stonesAfterTake =
            takeAll stone.nearby stones

        stonesIfHasLibertiesAfterTake : Stone -> Maybe Stones
        stonesIfHasLibertiesAfterTake newStone =
            if findGroupWithoutLiberties stonesAfterTake newStone == [] then
                Just stonesAfterTake

            else
                Nothing
    in
    Dict.get (stoneKey stone) stonesAfterTake
        |> Maybe.andThen stonesIfHasLibertiesAfterTake


playIfLegal : Stones -> Stone -> Maybe Stones
playIfLegal stones bareStone =
    let
        stone : Stone
        stone =
            enhanceInfo stones bareStone
    in
    if isWithinBoard stone.spot && (not <| overlaps stone.spot stone.nearby) then
        Dict.insert (stoneKey stone) stone stones
            |> addStones (addNearby stone) stone.nearby
            |> addStones (addAdjacent stone) stone.adjacent
            |> maybePlay stone

    else
        Nothing


playStones : List Stone -> Stones -> Stones
playStones toPlay stones =
    let
        playStone stone acc =
            playIfLegal acc stone |> Maybe.withDefault acc
    in
    List.foldl playStone stones toPlay


play : Player -> Stones -> Spot -> Maybe ( Stones, Stone )
play onMove stones spot =
    let
        nearby : List Stone
        nearby =
            nearbyStones stones spot

        maybeStone : Maybe Stone
        maybeStone =
            Liberties.findNearestPlayable spot (List.map .spot nearby)
                |> Maybe.map (createStone onMove)

        maybeStones : Maybe Stones
        maybeStones =
            Maybe.andThen (playIfLegal stones) maybeStone
    in
    Maybe.map2 Tuple.pair maybeStones maybeStone



-- Cleanup


removeSingle : (Stone -> Stone) -> Spot -> Stones -> Stones
removeSingle removeFun removeFrom =
    Dict.update ( removeFrom.x, removeFrom.y ) (Maybe.map removeFun)


remove : (Stone -> Stone) -> List Spot -> Stones -> Stones
remove removeFun fromSpots stones =
    List.foldl (removeSingle removeFun) stones fromSpots


removeStone : Stone -> Stones -> Stones
removeStone toRemove stones =
    Dict.remove (stoneKey toRemove) stones
        |> remove (removeNearby toRemove) toRemove.nearby
        |> remove (removeAdjacent toRemove) toRemove.adjacent
