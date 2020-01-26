module Command.Commands where

import Bot

import Bot.Irc.Send
import Bot.Random
import Command
import Conc
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T (pack, unpack, words)
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)
import Data.List (intercalate)

import qualified Data.HashMap.Strict as M

-- TODO: check cooldowns, add cooldowns
runCommand :: Message -> Conc App
runCommand message =
  Pure $ do
    cmd <- getCommand message
    case cmd of
      Nothing -> return End
      Just command -> return (action command message)

simpleCommands :: Commands
simpleCommands =
  M.fromList $
  concatMap
    (\command -> map (\str -> (str, command)) (commands command))
    commandList

commandList :: [Command App]
commandList =
  [ Command
      "dicegolf"
      ["!dicegolf"]
      (CommandOptions 2 2 True True (const True))
      dicegolfCommand
  ]

dicegolfCommand :: (Composable n App, RandomGenerator n) => Message -> Conc n
dicegolfCommand (Message msg _) =
  () &
  (const $
   case msg !? 1 of
     Nothing -> dicegolf 100
     Just start -> dicegolf (fromMaybe 100 . readMaybe . T.unpack $ start)) >>+
  (privmsg . formatDicegolfResult :: [Int] -> App ()) >>.
  End

golf :: StringType
golf = TE.decodeUtf8 "\195\162\194\155\194\179"

formatDicegolfResult :: [Int] -> StringType
formatDicegolfResult throws =
  "Dicegolf " <> golf <> t <> golf <> (T.pack . show . pred . length $ throws)
  where
    t = T.pack . intercalate ", " . map show $ throws
