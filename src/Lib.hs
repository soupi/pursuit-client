{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( search
    , Result(..)
    ) where

import Data.Maybe (mapMaybe)
import Network.Wreq
import Text.Taggy.Lens
import Control.Lens
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)

search :: String -> IO [Result]
search str = do
    con <- find str
    pure (results con)

find :: String -> IO [Element]
find s = do
    r <- get ("https://pursuit.purescript.org/search?q=" ++ s)
    let txt = r ^. responseBody . to decodeUtf8
    let res = txt ^.. html . allAttributed (folded . only "search-result")
    pure res

getUrl :: Element -> Maybe T.Text
getUrl r = r ^. attrs . at "href"

getContent :: Node -> T.Text
getContent c = mconcat $ reverse (c ^.. to universe . traverse . Text.Taggy.Lens.contents)

result :: Element -> Maybe Result
result r = do
    url <- getUrl r
    let [sig, m] = (map getContent . eltChildren) r
    pure $ Result url m sig

results :: [Element] -> [Result]
results = mapMaybe result

data Result = Result
  { rUrl :: T.Text
  , rMod :: T.Text
  , rSig :: T.Text
  } deriving (Show, Eq)
