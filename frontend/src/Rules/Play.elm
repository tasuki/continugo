module Rules.Play exposing (group, playIfLegal, playIfLegalOn, replayPlacements, replayPlacementsOn)

import Dict
import Rules.Board as Board exposing (Board)
import Rules.Liberties as Liberties
import Rules.Position exposing (..)
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
        isUnvisited : Stone -> Bool
        isUnvisited neighbor =
            not (Set.member (stoneKey neighbor) closed.keys)

        toOpen : List Stone
        toOpen =
            toExplore.adjacent
                |> spotsToStones stones
                |> List.filter isUnvisited
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


findLibertyless : Board -> Stones -> ( Open, Closed ) -> ( Bool, Closed )
findLibertyless board stones ( open, closed ) =
    case open of
        [] ->
            -- libertyless, return group
            ( True, closed )

        toExplore :: exploreLater ->
            if Liberties.hasLibertiesOn board toExplore toExplore.nearby then
                -- Every discovered member belongs to this living group.
                ( False, closed )

            else
                -- has no liberties but has neighbors
                findLibertyless board stones <|
                    newOpenClosed stones toExplore exploreLater closed


findGroupWithoutLiberties : Board -> Stones -> Stone -> List Stone
findGroupWithoutLiberties board stones stone =
    case findLibertyless board stones (startGroup stone) of
        ( True, closed ) ->
            closed.members

        _ ->
            []


captureGroupsWithoutLiberties : Board -> Color -> List Spot -> Stones -> Stones
captureGroupsWithoutLiberties board color spots stones =
    let
        check stone ( seen, dead ) =
            if stone.color /= color || Set.member (stoneKey stone) seen then
                ( seen, dead )

            else
                let
                    ( libertyless, closed ) =
                        findLibertyless board stones (startGroup stone)
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


resolveCaptures : Board -> Stone -> Stones -> Maybe Stones
resolveCaptures board stone stones =
    let
        stonesAfterTake : Stones
        stonesAfterTake =
            stones
                -- take enemy stones
                |> captureGroupsWithoutLiberties board (otherColor stone.color) stone.nearby
                -- also take own stones if necessary
                |> captureGroupsWithoutLiberties board stone.color stone.nearby

        stonesIfHasLibertiesAfterTake : Stone -> Maybe Stones
        stonesIfHasLibertiesAfterTake newStone =
            if findGroupWithoutLiberties board stonesAfterTake newStone == [] then
                Just stonesAfterTake

            else
                Nothing
    in
    Dict.get (stoneKey stone) stonesAfterTake
        |> Maybe.andThen stonesIfHasLibertiesAfterTake


playIfLegal : Stones -> Stone -> Maybe Stones
playIfLegal =
    playIfLegalOn Board.default


playIfLegalOn : Board -> Stones -> Stone -> Maybe Stones
playIfLegalOn board stones bareStone =
    let
        stone : Stone
        stone =
            withNeighborsOn board stones bareStone
    in
    if isWithinBoardOn board stone.spot && (not <| overlapsAnyOn board stone.spot stone.nearby) then
        Dict.insert (stoneKey stone) stone stones
            |> updateStones (addNearby stone) stone.nearby
            |> updateStones (addAdjacent stone) stone.adjacent
            |> resolveCaptures board stone

    else
        Nothing


{-| Replay placements in chronological order, skipping passes and illegal placements.
Turn order and game completion are not checked here.
-}
replayPlacements : List Play -> Stones -> Stones
replayPlacements =
    replayPlacementsOn Board.default


replayPlacementsOn : Board -> List Play -> Stones -> Stones
replayPlacementsOn board toPlay stones =
    let
        playStone stone acc =
            playIfLegalOn board acc stone |> Maybe.withDefault acc
    in
    List.foldl playStone stones (stonesFromPlays toPlay)



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
