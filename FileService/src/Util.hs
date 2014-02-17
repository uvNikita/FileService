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
      validateCalc
    , maybeRead
) where

import Data.Maybe (listToMaybe)

validateCalc :: Int -> Int -> Maybe Int -> Bool
validateCalc _ _ Nothing = False
validateCalc x1 x2 (Just res) = res == x1 + x2

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads
