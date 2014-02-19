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

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads


parseCommand :: String -> (String, [String])
parseCommand "" = ("", [""])
parseCommand line =
    (command, args)
    where (command, rawArgs) = break (== ' ') line
          args = filter (not . null) $ parse rawArgs
          parse "" = [""]
          parse (' ' : cs) = [] : parse cs
          parse ('"' : cs) = arg : parse cs'
                               where (arg, cs') = case break (== '"') cs of
                                                      (arg, '"' : cs'') -> (arg, cs'')
                                                      (arg, cs'') -> (arg, cs'')
          parse (c : rest) = (c : arg) : args
                             where arg : args = parse rest
