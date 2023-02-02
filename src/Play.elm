module Play exposing (..)

import Dict
import Go exposing (..)
import Liberties exposing (findLiberties)
import List.Extra
import Set exposing (Set)


type alias Open =
    List Stone


type alias Closed =
    Set ( Int, Int )


spotsToStones : Stones -> List Spot -> List Stone
spotsToStones stones =
    List.filterMap (\spot -> Dict.get ( spot.x, spot.y ) stones)


findGroupWithoutLiberties : Stones -> Stone -> List Spot
findGroupWithoutLiberties stones stone =
    let
        newOpenClosed : Stone -> Open -> Closed -> ( Open, Closed )
        newOpenClosed toExplore open closed =
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

        findLibertyless : ( Open, Closed ) -> List Spot
        findLibertyless ( open, closed ) =
            case open of
                [] ->
                    -- libertyless
                    Set.toList closed |> List.map (\( x, y ) -> Spot x y)

                toExplore :: exploreLater ->
                    if findLiberties toExplore toExplore.nearby /= [] then
                        -- has at least one liberty
                        []

                    else
                        findLibertyless <| newOpenClosed toExplore exploreLater closed
    in
    findLibertyless ( [ stone ], Set.singleton <| stoneKey stone )


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
    in
    if findGroupWithoutLiberties stonesAfterTake stone == [] then
        Just stonesAfterTake

    else
        Nothing


playIfLegal : Stone -> Stones -> Maybe Stones
playIfLegal bareStone stones =
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
