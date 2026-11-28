module PlayMenuTests exposing (tests)

import Expect
import Html.Attributes as HA
import Main
import Rules.Position exposing (Color(..))
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


menu bot available =
    Main.viewNewGames
        { setup = { board = Main.defaultPlaySetup.board, bot = bot, customSize = Nothing, komi = "7", simulations = 128 }
        , botAvailable = available
        , sgfError = Nothing
        }
        |> Query.fromHtml


tests : Test
tests =
    describe "Play setup"
        [ test "Thinking uses an accessible menu circle" <|
            \_ ->
                Main.viewBotStatus (Just 1) Nothing
                    |> Query.fromHtml
                    |> Query.has [ Selector.id "bot-thinking", Selector.class "icon", Selector.attribute (HA.attribute "role" "status"), Selector.attribute (HA.attribute "aria-label" "Bot thinking") ]
        , test "Idle bots have no status circle" <|
            \_ ->
                Main.viewBotStatus Nothing Nothing
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.class "icon" ]
                    |> Query.count (Expect.equal 0)
        , test "Bot errors use an alert circle instead of a spinner" <|
            \_ ->
                Main.viewBotStatus Nothing (Just "Engine stopped")
                    |> Query.fromHtml
                    |> Query.has [ Selector.id "bot-error", Selector.class "icon", Selector.attribute (HA.attribute "role" "alert"), Selector.attribute (HA.attribute "aria-label" "Engine stopped") ]
        , test "Play and Replay are separate sections" <|
            \_ ->
                menu Nothing True
                    |> Expect.all
                        [ Query.find [ Selector.id "play-setup" ] >> Query.has [ Selector.tag "h2", Selector.text "Play" ]
                        , Query.find [ Selector.id "replay-setup" ] >> Query.has [ Selector.tag "h2", Selector.text "Replay" ]
                        ]
        , test "Selecting bot stages a White opponent without starting the game" <|
            \_ ->
                menu Nothing True
                    |> Query.find [ Selector.id "play-opponent-bot" ]
                    |> Event.simulate Event.click
                    |> Event.expect (Main.SetBot (Just White))
        , test "Human mode provides both moves" <|
            \_ ->
                menu (Just White) True
                    |> Query.find [ Selector.id "play-opponent-human" ]
                    |> Event.simulate Event.click
                    |> Event.expect (Main.SetBot Nothing)
        , test "Choosing White gives the bot Black" <|
            \_ ->
                menu (Just White) True
                    |> Query.find [ Selector.id "play-colour-white" ]
                    |> Event.simulate Event.click
                    |> Event.expect (Main.SetBot (Just Black))
        , test "Human mode hides the colour selection" <|
            \_ ->
                menu Nothing True
                    |> Query.findAll [ Selector.id "play-colour" ]
                    |> Query.count (Expect.equal 0)
        , test "Bot is disabled without an engine" <|
            \_ ->
                menu Nothing False
                    |> Query.find [ Selector.attribute (HA.value "bot") ]
                    |> Query.has [ Selector.disabled True ]
        , test "Custom size input stages the requested size" <|
            \_ ->
                Main.viewNewGames { setup = { board = Main.defaultPlaySetup.board, bot = Nothing, customSize = Just "", komi = "7", simulations = 128 }, botAvailable = True, sgfError = Nothing }
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "play-custom-size" ]
                    |> Event.simulate (Event.input "25")
                    |> Event.expect (Main.SetCustomSize "25")
        , test "Setup uses radio buttons rather than dropdowns" <|
            \_ ->
                menu (Just White) True
                    |> Expect.all
                        [ Query.findAll [ Selector.tag "select" ] >> Query.count (Expect.equal 0)
                        , Query.findAll [ Selector.attribute (HA.type_ "radio") ] >> Query.count (Expect.equal 8)
                        ]
        , test "Custom board sizes are limited to 7 through 25" <|
            \_ ->
                [ "7", "9", "25", "6", "26", "500", "9.5", "" ]
                    |> List.map (\value -> Main.setupBoard { board = Main.defaultPlaySetup.board, bot = Nothing, customSize = Just value, komi = "7", simulations = 128 } /= Nothing)
                    |> Expect.equal [ True, True, True, False, False, False, False, False ]
        , test "Komi defaults to seven and supports decimals and negatives" <|
            \_ ->
                [ "7", "6.5", "-2.5", "", "NaN", "Infinity" ]
                    |> List.map (\value -> Main.setupKomi { board = Main.defaultPlaySetup.board, bot = Nothing, customSize = Nothing, komi = value, simulations = 128 })
                    |> Expect.equal [ Just 7, Just 6.5, Just -2.5, Nothing, Nothing, Nothing ]
        , test "Invalid custom sizes disable Start" <|
            \_ ->
                Main.viewNewGames { setup = { board = Main.defaultPlaySetup.board, bot = Nothing, customSize = Just "0", komi = "7", simulations = 128 }, botAvailable = True, sgfError = Nothing }
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "play-start" ]
                    |> Query.has [ Selector.disabled True ]
        , test "Invalid komi disables Start" <|
            \_ ->
                Main.viewNewGames { setup = { board = Main.defaultPlaySetup.board, bot = Nothing, customSize = Nothing, komi = "", simulations = 128 }, botAvailable = True, sgfError = Nothing }
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "play-start" ]
                    |> Query.has [ Selector.disabled True ]
        , test "Komi input stages the requested value" <|
            \_ ->
                menu Nothing True
                    |> Query.find [ Selector.id "play-komi" ]
                    |> Event.simulate (Event.input "6.5")
                    |> Event.expect (Main.SetKomi "6.5")
        , test "Bot setup shows a bounded simulations slider" <|
            \_ ->
                menu (Just White) True
                    |> Query.find [ Selector.id "play-simulations" ]
                    |> Query.has [ Selector.attribute (HA.type_ "range"), Selector.attribute (HA.min "1"), Selector.attribute (HA.max "1024"), Selector.attribute (HA.value "128") ]
        , test "Slider stages a new simulation budget" <|
            \_ ->
                menu (Just White) True
                    |> Query.find [ Selector.id "play-simulations" ]
                    |> Event.simulate (Event.input "256")
                    |> Event.expect (Main.SetSimulations 256)
        , test "Human setup hides the simulations slider" <|
            \_ ->
                menu Nothing True
                    |> Query.findAll [ Selector.id "play-simulations" ]
                    |> Query.count (Expect.equal 0)
        , test "Start explicitly launches the configured game" <|
            \_ ->
                menu (Just Black) True
                    |> Query.find [ Selector.id "play-start" ]
                    |> Event.simulate Event.click
                    |> Event.expect Main.StartGame
        ]
