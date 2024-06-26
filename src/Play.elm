module Play exposing (groupAndItsLiberties, playIfLegal, playNearby, playStones)

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
    in
    ( toOpen ++ open, toOpen ++ closed )



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


takeAll : Player -> List Spot -> Stones -> Stones
takeAll player spots stones =
    let
        toRemove : List Stone
        toRemove =
            spotsToStones stones spots
                |> List.filter (\s -> s.player == player)
                |> List.concatMap (findGroupWithoutLiberties stones)
    in
    List.foldl removeStone stones toRemove


maybePlay : Stone -> Stones -> Maybe Stones
maybePlay stone stones =
    let
        stonesAfterTake : Stones
        stonesAfterTake =
            stones
                -- take enemy stones
                |> takeAll (otherPlayer stone.player) stone.nearby
                -- also take own stones if necessary
                |> takeAll stone.player stone.nearby

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
    -- recreates the exact list of plays
    let
        playStone stone acc =
            playIfLegal acc stone |> Maybe.withDefault acc
    in
    List.foldl playStone stones (stonesFromPlays toPlay)


playNearby : Player -> Stones -> Spot -> Maybe ( Stones, Stone )
playNearby onMove stones spot =
    -- if spot unplayable, determines nearby spot to play
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
