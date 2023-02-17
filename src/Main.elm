module Main exposing (..)

import Board
import Browser
import Browser.Dom as BD
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Go exposing (..)
import Html as H
import Html.Attributes as HA
import Json.Decode as D
import Liberties
import Play
import Process
import Regex
import Sgf
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import Svg.Lazy exposing (..)
import Task
import Url exposing (Url)


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { record : List Stone
    , stones : Stones
    , ghostStone : Maybe Stone
    , highlightedGroup : List Spot
    , highlightedLiberties : List Spot
    , justPlayed : Maybe Spot
    , justRemoved : List Stone
    , onMove : Player
    , navKey : Nav.Key
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    changeRouteTo url
        { record = []
        , stones = Dict.empty
        , ghostStone = Nothing
        , highlightedGroup = []
        , highlightedLiberties = []
        , justPlayed = Nothing
        , justRemoved = []
        , onMove = Black
        , navKey = navKey
        }



-- UPDATE


type Msg
    = Clicked Spot
    | MouseMoved Spot
    | PlayIfLegal Spot (Result BD.Error BD.Element)
    | Hover Spot (Result BD.Error BD.Element)
    | RemoveStones
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


handleHover : Model -> Spot -> Model
handleHover model hoverSpot =
    let
        maybeOverStone : Maybe Stone
        maybeOverStone =
            stoneList model.stones
                |> List.filter (\s -> distance hoverSpot s.spot < stoneR)
                |> List.head

        memberOfHighlighted : Stone -> Bool
        memberOfHighlighted stone =
            List.member stone.spot model.highlightedGroup

        overAlreadyShown : Bool
        overAlreadyShown =
            Maybe.map memberOfHighlighted maybeOverStone |> Maybe.withDefault False

        overJustPlayed : Bool
        overJustPlayed =
            Maybe.map .spot maybeOverStone == model.justPlayed
    in
    case ( maybeOverStone, overJustPlayed || overAlreadyShown ) of
        ( Just _, True ) ->
            -- hovering over the stone which was just played OR
            -- hovering over the stone whose liberties are already shown
            -- show nothing
            model

        ( Just over, False ) ->
            -- hovering over a stone: show liberties
            let
                ( group, liberties ) =
                    Play.groupAndItsLiberties model.stones over
            in
            { model
                | highlightedGroup = List.map .spot group
                , highlightedLiberties = liberties
                , ghostStone = Nothing
                , justPlayed = Nothing
            }

        ( Nothing, _ ) ->
            -- hovering over an empty spot: show ghost move if possible
            { model
                | ghostStone =
                    Play.play model.onMove model.stones hoverSpot
                        |> Maybe.map Tuple.second
                , highlightedGroup = []
                , highlightedLiberties = []
                , justPlayed = Nothing
            }


handlePlay : Model -> Spot -> ( Model, Cmd Msg )
handlePlay model playSpot =
    case Play.play model.onMove model.stones playSpot of
        Just ( stones, played ) ->
            -- play!
            let
                stonesWithPotential =
                    Liberties.addPotential played stones

                newModel =
                    { model
                        | record = played :: model.record
                        , stones = Liberties.recalculate (played.spot :: played.nearby) stonesWithPotential
                        , onMove = otherPlayer model.onMove
                        , justPlayed = Just played.spot
                        , justRemoved = removedStones model.stones stones
                    }
            in
            ( newModel
            , Cmd.batch
                [ pushUrl newModel.navKey <| newModel.record
                , Process.sleep 500 |> Task.perform (\_ -> RemoveStones)
                ]
            )

        _ ->
            -- illegal move
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Clicked clickedCoords ->
            ( model
            , BD.getElement "board" |> Task.attempt (PlayIfLegal clickedCoords)
            )

        PlayIfLegal clickedCoords (Ok element) ->
            handlePlay model (toBoardCoords clickedCoords element)

        MouseMoved hoverCoords ->
            ( model
            , BD.getElement "board" |> Task.attempt (Hover hoverCoords)
            )

        Hover hoverCoords (Ok element) ->
            ( handleHover model (toBoardCoords hoverCoords element)
            , Cmd.none
            )

        RemoveStones ->
            ( { model | justRemoved = [] }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            ( model, Cmd.none )

        UrlChanged url ->
            changeRouteTo url model

        _ ->
            ( model, Cmd.none )


toBoardCoords : Spot -> BD.Element -> Spot
toBoardCoords windowCoords element =
    { x =
        round <|
            (toFloat windowCoords.x - element.element.x)
                * (toFloat coordRange / element.element.width)
    , y =
        round <|
            (toFloat windowCoords.y - element.element.y)
                * (toFloat coordRange / element.element.height)
    }



-- ROUTING / URL


recordRegex : Regex.Regex
recordRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^record=(.*)$"


changeRouteTo : Url.Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    let
        maybeRecord : Maybe (List Stone)
        maybeRecord =
            Maybe.map (Regex.find recordRegex) url.query
                |> Maybe.andThen List.head
                |> Maybe.map .submatches
                |> Maybe.andThen List.head
                |> Maybe.andThen identity
                |> Maybe.map (Sgf.decode >> List.reverse)

        addPotentialLiberties : Stones -> Stones
        addPotentialLiberties stones =
            List.foldl Liberties.addPotential stones (stoneList stones)

        createModel : List Stone -> Model
        createModel record =
            { model
                | record = record
                , stones =
                    Play.playStones (List.reverse record) Dict.empty
                        |> addPotentialLiberties
                , onMove =
                    List.head record
                        |> Maybe.map .player
                        |> Maybe.map otherPlayer
                        |> Maybe.withDefault Black
            }

        maybeNewSpots =
            Maybe.map (List.map .spot) maybeRecord

        newModel =
            if maybeNewSpots == Just (List.map .spot model.record) then
                model

            else
                Maybe.map createModel maybeRecord |> Maybe.withDefault model
    in
    ( newModel, Cmd.none )


pushUrl : Nav.Key -> List Stone -> Cmd msg
pushUrl navKey record =
    -- Bravely go where no legal URL has gone before.
    -- I prefer URLs to look nice rather than be valid.
    Nav.pushUrl navKey <| "/game?record=" ++ (Sgf.encode <| List.reverse record)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onClick
            (D.map Clicked
                (D.map2 Spot
                    (D.field "pageX" D.int)
                    (D.field "pageY" D.int)
                )
            )
        , Browser.Events.onMouseMove
            (D.map MouseMoved
                (D.map2 Spot
                    (D.field "pageX" D.int)
                    (D.field "pageY" D.int)
                )
            )
        ]



-- VIEW


hideLines : Stone -> Svg Msg
hideLines { spot } =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt <| stoneR * 7
        ]
        []


viewStone : String -> Stone -> Svg Msg
viewStone extraClass { player, spot } =
    let
        class =
            case player of
                Black ->
                    "black"

                White ->
                    "white"
    in
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        , SA.class class
        , SA.class extraClass
        ]
        []


viewHighlight : Spot -> Svg Msg
viewHighlight spot =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        ]
        []


viewLiberty : Spot -> Svg Msg
viewLiberty spot =
    Svg.circle
        [ SA.cx <| String.fromInt spot.x
        , SA.cy <| String.fromInt spot.y
        , SA.r <| String.fromInt stoneR
        ]
        []


viewLink : ( Spot, Spot ) -> Svg Msg
viewLink ( s1, s2 ) =
    Svg.line
        [ SA.x1 <| String.fromInt s1.x
        , SA.y1 <| String.fromInt s1.y
        , SA.x2 <| String.fromInt s2.x
        , SA.y2 <| String.fromInt s2.y
        ]
        []


viewGhostStone : Stone -> Svg Msg
viewGhostStone stone =
    viewStone "" stone


viewGhostLink : ( Spot, Spot ) -> Svg Msg
viewGhostLink ( ghost, existing ) =
    viewLink ( Go.spotBorderNearestTo existing ghost, existing )



-- Keyed


spotKeyStr : Spot -> String
spotKeyStr s =
    "-" ++ String.fromInt s.x ++ "-" ++ String.fromInt s.y


hideLinesKeyed : Stone -> ( String, Svg Msg )
hideLinesKeyed stone =
    ( "hide" ++ spotKeyStr stone.spot
    , lazy hideLines stone
    )


viewKeyedStone : String -> Stone -> ( String, Svg Msg )
viewKeyedStone extraClass stone =
    ( "stone" ++ spotKeyStr stone.spot
    , lazy2 viewStone extraClass stone
    )


viewKeyedLink : ( Spot, Spot ) -> ( String, Svg Msg )
viewKeyedLink ( s1, s2 ) =
    ( "link" ++ spotKeyStr s1 ++ spotKeyStr s2
    , lazy viewLink ( s1, s2 )
    )


classJustPlayed : Maybe Spot -> Stone -> String
classJustPlayed justPlayed stone =
    if justPlayed == Just stone.spot then
        "just-played"

    else
        ""



-- Main view


viewSvg : Model -> List (Svg Msg)
viewSvg model =
    [ Svg.defs []
        [ Svg.filter [ SA.id "blur-filter" ]
            [ Svg.feGaussianBlur [ SA.in_ "SourceGraphic", SA.stdDeviation "70" ] [] ]
        ]
    , lazy3 Svg.node "g" [ SA.id "lines" ] <| Board.viewLines
    , lazy3 Svg.node "g" [ SA.id "stars" ] <| Board.viewStars
    , lazy3 Svg.Keyed.node "g" [ SA.id "hide-lines", SA.filter "url(#blur-filter)" ] <|
        List.map hideLinesKeyed (stoneList model.stones)
    , lazy3 Svg.node "g" [ SA.id "liberties" ] <|
        (model.highlightedLiberties |> List.map viewLiberty)
    , lazy3 Svg.node "g" [ SA.id "ghost-links" ] <|
        List.map viewGhostLink (Maybe.map getStoneLinks model.ghostStone |> Maybe.withDefault [])
    , lazy3 Svg.node "g" [ SA.id "ghost-stone" ] <|
        List.filterMap identity [ Maybe.map viewGhostStone model.ghostStone ]
    , lazy3 Svg.Keyed.node "g" [ SA.id "links" ] <|
        List.map viewKeyedLink (getUniqueLinks model.stones)
    , lazy3 Svg.Keyed.node "g" [ SA.id "stones" ] <|
        (stoneList model.stones |> List.map (\s -> viewKeyedStone (classJustPlayed model.justPlayed s) s))
    , lazy3 Svg.node "g" [ SA.id "removed-stones" ] <|
        (model.justRemoved |> List.map (viewStone "removed"))
    , lazy3 Svg.node "g" [ SA.id "highlights" ] <|
        (model.highlightedGroup |> List.map viewHighlight)
    ]


view : Model -> Browser.Document Msg
view model =
    let
        intsToStr ints =
            List.map String.fromInt ints |> String.join " "
    in
    { title = "#" ++ (String.fromInt <| List.length model.record) ++ " – ContinuGo"
    , body =
        [ H.div [ HA.id "board" ]
            [ Svg.svg
                [ SA.viewBox <| intsToStr [ 0, 0, coordRange, coordRange ] ]
                (viewSvg model)
            ]
        ]
    }
