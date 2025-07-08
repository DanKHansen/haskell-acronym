module Acronym (abbreviate) where

import Data.Char (isAsciiUpper)
import Data.Text (Text)
import qualified Data.Text as T

abbreviate :: Text -> Text
abbreviate xs =
  let ws = T.words (T.map (\c -> if c == '-' || c == '_' then ' ' else c) xs)
      is = concatMap firstAndUpper ws
   in T.toUpper $ T.concat is

firstAndUpper :: Text -> [Text]
firstAndUpper txt
  | T.all isAsciiUpper txt && not (T.null txt) = [T.take 1 txt]
  | otherwise =
      case T.uncons txt of
        Nothing -> []
        Just (h, t) ->
          T.singleton h : map T.singleton (T.unpack $ T.filter isUpper t)
  where
    isUpper = isAsciiUpper
