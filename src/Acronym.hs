module Acronym (abbreviate) where

import Data.Text (Text)
import qualified Data.Text as T

abbreviate :: Text -> Text
abbreviate xs =
  let ws = T.words (T.map (\c -> if c == '-' || c == ',' then ' ' else c) xs)
   in T.concat $ map (T.toUpper . T.take 1) ws
