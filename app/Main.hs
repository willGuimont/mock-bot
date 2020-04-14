module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import qualified Data.Text.IO as TIO

import Discord
import qualified Discord.Requests as R
import Discord.Types

import Lib

main :: IO ()
main = startDiscordBot

startDiscordBot :: IO ()
startDiscordBot = do
  tok <- TIO.readFile "./auth-token.secret"
  t <-
    runDiscord $
    def
      { discordToken = tok
      , discordOnStart = startHandler
      , discordOnEnd = putStrLn "Ended"
      , discordOnEvent = eventHandler
      , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
      }
  threadDelay (microsecondsFromMilliseconds 10)
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandle -> IO ()
startHandler dis = do
  Right partialGuilds <- restCall dis R.GetCurrentUserGuilds
  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall dis $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall dis $ R.GetGuildChannels (guildId guild)
    case filter isTextChannel chans of
      (c:_) -> do
        _ <- helloMessage dis c
        pure ()
      _ -> pure ()

helloMessage :: DiscordHandle -> Channel -> IO ()
helloMessage dis c = do
  _ <- restCall dis $ R.CreateMessage (channelId c) "you caN'T coNVeY sArCASM tROugH WRitTeN tExT"
  pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event =
  case event of
    MessageCreate m -> when (not (fromBot m) && isMock (messageText m)) $
                         mockMessage dis m >> pure ()
    _ -> pure ()

mockMessage :: DiscordHandle -> Message -> IO ()
mockMessage dis m = do
    let cid = messageChannel m
    message <- mock (messageText m)
    _ <- restCall dis $ R.CreateMessage cid message
    pure ()