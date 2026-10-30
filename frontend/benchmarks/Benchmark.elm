port module Benchmark exposing (main)

import Coverage
import Dict
import Placement
import Platform
import Rules.Board as Board
import Rules.Liberties as Liberties
import Rules.Play as Play
import Rules.Position exposing (..)
import Sgf


port run : ({ scenario : String, iterations : Int } -> msg) -> Sub msg


port done : Int -> Cmd msg


port failed : String -> Cmd msg


main =
    Platform.worker
        { init = \() -> ( (), Cmd.none )
        , update =
            \request model ->
                case Result.map2 (\board stone -> benchmark board stone request.scenario request.iterations) stones capture of
                    Ok result ->
                        ( model, done result )

                    Err error ->
                        ( model, failed error )
        , subscriptions = \_ -> run identity
        }


stones : Result String Stones
stones =
    Sgf.decode "B[oifh];W[fgoc];B[odpz];W[fhfm];B[ihdL];W[lfem];B[ekpj];W[fsqe];B[dWnk];W[eSmd];B[dGll];W[dsqt];B[fHkR];W[iogz];B[jAfl];W[kPgM];B[gMfX];W[gUez];B[jnhQ];W[hIit];B[lLfD];W[mRgQ];B[mLdR];W[jPcR];B[iHbT];W[hqcz];B[kJbI];W[mbcC];B[nrbT];W[hLaM];B[lZaM];W[jKjx];B[lfiC];W[lCko];B[mVjo];W[nAkG];B[oQjC];W[pCkQ];B[cQim];W[qrmH];B[pYii];W[qNju];B[regx];W[oshO];B[codi];W[dhfM];B[hNlD];W[pvgk]"
        |> Result.map (\record -> Play.replayPlacements record Dict.empty)


capture : Result String Stone
capture =
    case Sgf.decodePlay "B[qheG]" |> Maybe.map .move of
        Just (Place stone) ->
            Ok stone

        _ ->
            Err "Invalid capture fixture"


benchmark : Stones -> Stone -> String -> Int -> Int
benchmark board stone scenario iterations =
    List.range 1 iterations
        |> List.foldl
            (\i total ->
                total
                    + (case scenario of
                        "blocked-liberties" ->
                            if Liberties.hasLiberties (createStone Black (Spot (949 + modBy 2 i) 51)) [ Spot 859 70, Spot 930 141 ] then
                                1

                            else
                                0

                        "capture" ->
                            Play.playIfLegal board { stone | spot = Spot (stone.spot.x + modBy 2 i) stone.spot.y }
                                |> Maybe.map Dict.size
                                |> Maybe.withDefault 0

                        "hover" ->
                            Placement.playNearby Board.default Black board (Spot (850 + modBy 30 i) 850)
                                |> Maybe.map (Tuple.first >> Dict.size)
                                |> Maybe.withDefault 0

                        "coverage" ->
                            Coverage.geometry Board.default (Spot (500 + modBy 30 i) 500 :: List.map .spot (stoneList board))
                                |> (\g -> List.length g.arcs + List.length g.lines + List.length g.points)

                        "liberty-geometry" ->
                            Coverage.libertyGeometry Board.default (Spot (500 + modBy 30 i) 500 :: List.map .spot (stoneList board)) [ Spot (500 + modBy 30 i) 500 ]
                                |> (\g -> List.length g.arcs + List.length g.lines + List.length g.points)

                        _ ->
                            0
                      )
            )
            0
