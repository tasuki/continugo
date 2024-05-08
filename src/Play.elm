module Play exposing (groupAndItsLiberties, play, playIfLegal, playStones)

import Dict
import Go exposing (..)
import Liberties


type alias Open =
    List Stone


type alias Closed =
    List Stone


spotsToStones : Stones -> List Spot -> List Stone
spotsToStones stones =
    List.filterMap (\spot -> Dict.get ( spot.x, spot.y ) stones)


newOpenClosed : Stones -> Stone -> Open -> Closed -> ( Open, Closed )
newOpenClosed stones toExplore open closed =
    let
        isClosed : Stone -> Bool
        isClosed neighbor =
            List.all ((/=) neighbor) closed

        toOpen : List Stone
        toOpen =
            toExplore.adjacent
                |> spotsToStones stones
                |> List.filter isClosed

        newOpen : List Stone
        newOpen =
            toOpen ++ open

        newClosed =
            toOpen ++ closed
    in
    ( newOpen, newClosed )



-- Group and its liberties


findGroupAndItsLiberties : Stones -> List Spot -> ( Open, Closed ) -> ( List Stone, List Spot )
findGroupAndItsLiberties stones liberties ( open, closed ) =
    case open of
        [] ->
            -- that's it, we found the whole group
            ( closed
            , Liberties.uniqueLiberties liberties
            )

        toExplore :: exploreLater ->
            -- wait there's more
            let
                newLiberties =
                    Liberties.findLiberties toExplore toExplore.nearby
            in
            findGroupAndItsLiberties stones (newLiberties ++ liberties) <|
                newOpenClosed stones toExplore exploreLater closed


groupAndItsLiberties : Stones -> Stone -> ( List Stone, List Spot )
groupAndItsLiberties stones stone =
    findGroupAndItsLiberties stones [] ( [ stone ], [ stone ] )



-- Play


findLibertyless : Stones -> ( Open, Closed ) -> List Stone
findLibertyless stones ( open, closed ) =
    case open of
        [] ->
            -- libertyless, return group
            closed

        toExplore :: exploreLater ->
            if Liberties.hasLiberties toExplore toExplore.nearby then
                -- has at least one liberty, stop and return nothing
                []

            else
                -- has no liberties but has neighbors
                findLibertyless stones <|
                    newOpenClosed stones toExplore exploreLater closed


findGroupWithoutLiberties : Stones -> Stone -> List Stone
findGroupWithoutLiberties stones stone =
    findLibertyless stones ( [ stone ], [ stone ] )


takeAll : List Spot -> Stones -> Stones
takeAll spots stones =
    let
        toRemove : List Stone
        toRemove =
            spotsToStones stones spots
                |> List.concatMap (findGroupWithoutLiberties stones)
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
    if isWithinBoard stone.spot && (not <| overlapsAny stone.spot stone.nearby) then
        Dict.insert (stoneKey stone) stone stones
            |> addStones (addNearby stone) stone.nearby
            |> addStones (addAdjacent stone) stone.adjacent
            |> maybePlay stone

    else
        Nothing


playStones : List Play -> Stones -> Stones
playStones toPlay stones =
    let
        playStone stone acc =
            playIfLegal acc stone |> Maybe.withDefault acc
    in
    List.foldl playStone stones (stonesFromPlays toPlay)


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

        freshenStone : ( Stones, Stone ) -> Maybe ( Stones, Stone )
        freshenStone ( freshStones, stone ) =
            Dict.get ( stone.spot.x, stone.spot.y ) freshStones
                |> Maybe.map (Tuple.pair freshStones)
    in
    Maybe.map2 Tuple.pair maybeStones maybeStone
        |> Maybe.andThen freshenStone



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
