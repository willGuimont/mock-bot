module Lib
  ( microsecondsFromSeconds
  , microsecondsFromMilliseconds
  , isTextChannel
  , fromBot
  , isMock
  , mock
  ) where

import Data.Char (toLower, toUpper)
import qualified Data.Text as T
import System.Random

import Discord.Types

microsecondsFromSeconds :: Int -> Int
microsecondsFromSeconds = (*) (10 ^ 6)

microsecondsFromMilliseconds :: Int -> Int
microsecondsFromMilliseconds = (*) (10 ^ 3)

isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

mockCmd :: T.Text
mockCmd = "!mock"

isMock :: T.Text -> Bool
isMock = (mockCmd `T.isPrefixOf`) . T.map toLower

toRandomCase :: Char -> IO Char
toRandomCase c = do
  p <- randomRIO (0, 1) :: IO Int
  let f =
        if p == 0
          then toLower
          else toUpper
  return $ f c

mock :: T.Text -> IO T.Text
mock x = T.pack <$> (mapM toRandomCase . T.unpack . T.replace mockCmd "" . T.map toLower) x
