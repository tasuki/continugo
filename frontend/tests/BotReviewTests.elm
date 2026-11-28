module BotReviewTests exposing (tests)

import Expect
import Main
import Review
import Rules.Game as Game
import Rules.Position exposing (Color(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


live =
    { bot = Just White
    , botReview = Nothing
    , botPending = Just 12
    , game = Game.pass Game.empty |> Maybe.withDefault Game.empty
    , review = Review.empty
    }


tests : Test
tests =
    describe "Browsing a bot game"
        [ test "Browsing remembers the live position and colour, and invalidates the pending reply" <|
            \_ ->
                let
                    paused =
                        Main.pauseBotReview live
                in
                Expect.equal
                    ( Just White, Just { game = live.game, path = live.review.selected }, Nothing )
                    ( paused.bot, paused.botReview, paused.botPending )
        , test "Further browsing never replaces the live snapshot" <|
            \_ ->
                let
                    paused =
                        Main.pauseBotReview live
                in
                Main.pauseBotReview { paused | game = Game.empty }
                    |> .botReview
                    |> Expect.equal paused.botReview
        , test "Imported and human games do not acquire a bot resume position" <|
            \_ ->
                Main.pauseBotReview { live | bot = Nothing }
                    |> .botReview
                    |> Expect.equal Nothing
        , test "The thinking circle belongs to the right-hand player menu" <|
            \_ ->
                Main.viewPlayerMenu [] (Just 12) Nothing
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "bot-thinking" ]
                    |> Query.has [ Selector.class "icon" ]
        ]
