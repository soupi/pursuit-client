{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module PursuitClient
    ( search
    , showContent
    , Content(..)
    , Result(..)
    ) where

import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Control.Exception (catch)
import Network.Wreq
import Network.HTTP.Client (HttpException)
import Text.Taggy.Lens as TTL
import Control.Lens
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)


-- | A single result of a query
data Result = Result
  { rCont :: Content -- | content of the result
  , rUrl  :: T.Text  -- | url for more info
  } deriving (Show, Eq)

-- | Different types of results
data Content
  = Value T.Text T.Text T.Text -- name, signature, package
  | Type T.Text [T.Text] T.Text T.Text -- name, type args, body, package
  | Data T.Text [T.Text] T.Text -- name, type args, package
  | Class T.Text [T.Text] T.Text -- name, type args, package
  | Module T.Text T.Text -- | name, package
  | Package T.Text -- | package
    deriving (Show, Eq)

-- | Pretty print the contents of a result
showContent :: Content -> T.Text
showContent = \case
  Value nm sig pkg -> nm <> " :: " <> sig <> "\n" <> pkg
  Type nm args body pkg -> T.intercalate " " (nm:args) <> " = " <> body <> "\n" <> pkg
  Data nm args pkg -> T.intercalate " " (nm:args) <> "\n" <> pkg
  Class nm args pkg -> "data " <> T.intercalate " " (nm:args) <> "\n" <> pkg
  Module nm pkg -> "module " <> nm <> "\n" <> pkg
  Package pkg -> "package " <> pkg

-- | search in pursuit
search :: String -> IO (Either String [Result])
search str =
    fmap pure (results <$> find str) `catchHttp` (pure . Left . show)

catchHttp :: IO a -> (HttpException -> IO a) -> IO a
catchHttp = catch

find :: String -> IO [Element]
find s = do
    r <- get ("https://pursuit.purescript.org/search?q=" ++ s)
    let txt = r ^. responseBody . to decodeUtf8
    let res = txt ^.. html . allAttributed (folded . only "search-result")
    pure res

results :: [Element] -> [Result]
results = mapMaybe result

result :: Element -> Maybe Result
result r = do
    url <- getUrl r
    let cont = (parseContent . getContent . NodeElement) r
    pure $ Result cont url

getUrl :: Element -> Maybe T.Text
getUrl r = r ^. attrs . at "href"

getContent :: Node -> [T.Text]
getContent c = c ^.. to universe . traverse . content


parseContent :: [T.Text] -> Content
parseContent ["package",pkg] = Package pkg
parseContent (reverse -> pkg:cont)
  | T.take 4 (head cont) == " :: " =
    Value (mconcat $ reverse $ tail cont) (T.drop 4 $ head cont) pkg

  | last cont == "type" && T.any (=='=') (head cont) =
    Type
        (mconcat $ reverse $ tail $ init cont)
        (T.words  $ T.takeWhile (/='=') $ head cont)
        (T.drop 2 $ T.dropWhile (/='=') $ head cont)
        pkg

  | T.take 5 (T.reverse (head cont)) == T.reverse "where" && last cont == "class" =
    Class
        (mconcat $ reverse $ tail $ init cont)
        (init $ T.words $ head cont)
        pkg

  | last cont == "module" =
    Module
        (mconcat $ reverse $ init cont)
        pkg

  | last cont == "data" =
    Data
        (mconcat $ reverse $ init cont)
        (T.words $ head cont)
        pkg

parseContent x = error ("No rule to parse: " ++ show x)
