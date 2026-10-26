module Play exposing (group, playIfLegal, playNearby, playStones)

import Dict
import Go exposing (..)
import Liberties
import Set exposing (Set)


type alias Open =
    List Stone


type alias Closed =
    { members : List Stone
    , keys : Set ( Int, Int )
    }


startGroup : Stone -> ( Open, Closed )
startGroup stone =
    ( [ stone ], { members = [ stone ], keys = Set.singleton (stoneKey stone) } )


spotsToStones : Stones -> List Spot -> List Stone
spotsToStones stones =
    List.filterMap (\spot -> Dict.get ( spot.x, spot.y ) stones)


newOpenClosed : Stones -> Stone -> Open -> Closed -> ( Open, Closed )
newOpenClosed stones toExplore open closed =
    let
        isClosed : Stone -> Bool
        isClosed neighbor =
            not (Set.member (stoneKey neighbor) closed.keys)

        toOpen : List Stone
        toOpen =
            toExplore.adjacent
                |> spotsToStones stones
                |> List.filter isClosed
    in
    ( toOpen ++ open
    , { members = toOpen ++ closed.members
      , keys = List.foldl (stoneKey >> Set.insert) closed.keys toOpen
      }
    )



-- Group


findGroup : Stones -> ( Open, Closed ) -> List Stone
findGroup stones ( open, closed ) =
    case open of
        [] ->
            closed.members

        toExplore :: exploreLater ->
            findGroup stones (newOpenClosed stones toExplore exploreLater closed)


group : Stones -> Stone -> List Stone
group stones stone =
    findGroup stones (startGroup stone)



-- Play


findLibertyless : Stones -> ( Open, Closed ) -> ( Bool, Closed )
findLibertyless stones ( open, closed ) =
    case open of
        [] ->
            -- libertyless, return group
            ( True, closed )

        toExplore :: exploreLater ->
            if Liberties.hasLiberties toExplore toExplore.nearby then
                -- Every discovered member belongs to this living group.
                ( False, closed )

            else
                -- has no liberties but has neighbors
                findLibertyless stones <|
                    newOpenClosed stones toExplore exploreLater closed


findGroupWithoutLiberties : Stones -> Stone -> List Stone
findGroupWithoutLiberties stones stone =
    case findLibertyless stones (startGroup stone) of
        ( True, closed ) ->
            closed.members

        _ ->
            []


takeAll : Player -> List Spot -> Stones -> Stones
takeAll player spots stones =
    let
        check stone ( seen, dead ) =
            if stone.player /= player || Set.member (stoneKey stone) seen then
                ( seen, dead )

            else
                let
                    ( libertyless, closed ) =
                        findLibertyless stones (startGroup stone)
                in
                ( Set.union seen closed.keys
                , if libertyless then
                    closed.members ++ dead

                  else
                    dead
                )

        ( _, toRemove ) =
            List.foldl check ( Set.empty, [] ) (spotsToStones stones spots)
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
