port module Benchmark exposing (main)

import Coverage
import Dict
import Placement
import Platform
import Rules.Liberties as Liberties
import Rules.Play as Play
import Rules.Position exposing (..)
import Sgf


port run : ({ scenario : String, iterations : Int } -> msg) -> Sub msg


port done : Int -> Cmd msg


main =
    Platform.worker
        { init = \() -> ( (), Cmd.none )
        , update = \request model -> ( model, done (benchmark request.scenario request.iterations) )
        , subscriptions = \_ -> run identity
        }


stones : Stones
stones =
    Play.replayPlacements
        (Sgf.decode "B[oifh];W[fgoc];B[odpz];W[fhfm];B[ihdL];W[lfem];B[ekpj];W[fsqe];B[dWnk];W[eSmd];B[dGll];W[dsqt];B[fHkR];W[iogz];B[jAfl];W[kPgM];B[gMfX];W[gUez];B[jnhQ];W[hIit];B[lLfD];W[mRgQ];B[mLdR];W[jPcR];B[iHbT];W[hqcz];B[kJbI];W[mbcC];B[nrbT];W[hLaM];B[lZaM];W[jKjx];B[lfiC];W[lCko];B[mVjo];W[nAkG];B[oQjC];W[pCkQ];B[cQim];W[qrmH];B[pYii];W[qNju];B[regx];W[oshO];B[codi];W[dhfM];B[hNlD];W[pvgk]")
        Dict.empty


capture : Stone
capture =
    Sgf.decode "B[qheG]"
        |> stonesFromPlays
        |> List.head
        |> Maybe.withDefault (createStone Black (Spot 500 500))


benchmark : String -> Int -> Int
benchmark scenario iterations =
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
                            Play.playIfLegal stones { capture | spot = Spot (capture.spot.x + modBy 2 i) capture.spot.y }
                                |> Maybe.map Dict.size
                                |> Maybe.withDefault 0

                        "hover" ->
                            Placement.playNearby Black stones (Spot (850 + modBy 30 i) 850)
                                |> Maybe.map (Tuple.first >> Dict.size)
                                |> Maybe.withDefault 0

                        "coverage" ->
                            Coverage.geometry (Spot (500 + modBy 30 i) 500 :: List.map .spot (stoneList stones))
                                |> (\g -> List.length g.arcs + List.length g.lines + List.length g.points)

                        "liberty-geometry" ->
                            Coverage.libertyGeometry (Spot (500 + modBy 30 i) 500 :: List.map .spot (stoneList stones)) [ Spot (500 + modBy 30 i) 500 ]
                                |> (\g -> List.length g.arcs + List.length g.lines + List.length g.points)

                        _ ->
                            0
                      )
            )
            0
