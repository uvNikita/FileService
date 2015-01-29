-----------------------------------------------------------------------------
--
-- Module      :  Util
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Util (
      maybeRead
    , parseCommand
) where

import Data.Maybe (listToMaybe)
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
import Text.ParserCombinators.Parsec (parse, many, many1, char, digit, (<?>),
                                      noneOf, letter, spaces, ParseError, GenParser)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads


command :: GenParser Char st (String, [String])
command = (,) <$> comName <*> args
    where comName = spaces *> commandName <* spaces <?> "command name"
          args = many (argument <* spaces)

commandName :: GenParser Char st String
commandName = many1 $ letter <|> digit

argument :: GenParser Char st String
argument = quotedArgument <|> many1 (letter <|> digit)

quotedArgument :: GenParser Char st String
quotedArgument = char '"' *> many (noneOf "\"") <* char '"'

parseCommand :: String -> Either ParseError (String, [String])
parseCommand = parse command "unknown command"
