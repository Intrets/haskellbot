module Command.Commands where

import Bot
import Bot.Irc
import Bot.Irc.Send
import Bot.Random
import Command
import Conc
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (unpack, words)
import Text.Read (readMaybe)

import qualified Data.HashMap.Strict as M

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

dicegolfCommand (Message msg _) =
  () &
  (const $
   case msg !? 1 of
     Nothing -> dicegolf 100
     Just start -> dicegolf (fromMaybe 100 . readMaybe . T.unpack $ start)) >>+
  (privmsg . formatDicegolfResult :: [Int] -> App ()) >>.
  End
