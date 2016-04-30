{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module PursuitClient
    ( search
    , showContent
    , Content(..)
    , Result(..)
    ) where

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
  | NewType T.Text [T.Text] T.Text -- name, type args, package
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
  Data nm args pkg -> "data " <> T.intercalate " " (nm:args) <> "\n" <> pkg
  NewType nm args pkg -> "newtype " <> T.intercalate " " (nm:args) <> "\n" <> pkg
  Class nm args pkg -> "class " <> T.intercalate " " (nm:args) <> "\n" <> pkg
  Module nm pkg -> "module " <> nm <> "\n" <> pkg
  Package pkg -> "package " <> pkg

-- | search in pursuit
search :: String -> IO (Either String [Result])
search str =
    (results <$> find str) `catchHttp` (pure . Left . show)

catchHttp :: IO a -> (HttpException -> IO a) -> IO a
catchHttp = catch

find :: String -> IO [Element]
find s = do
    r <- get ("https://pursuit.purescript.org/search?q=" ++ s)
    let txt = r ^. responseBody . to decodeUtf8
    let res = txt ^.. html . allAttributed (folded . only "search-result")
    pure res

results :: [Element] -> Either String [Result]
results = traverse result

result :: Element -> Either String Result
result r = do
    url <- maybe (Left "Unable to parse element. please report this.") pure $ getUrl r
    cont <- (parseContent . getContent . NodeElement) r
    pure $ Result cont url

getUrl :: Element -> Maybe T.Text
getUrl r = r ^. attrs . at "href"

getContent :: Node -> [T.Text]
getContent c = c ^.. to universe . traverse . content


parseContent :: [T.Text] -> Either String Content
parseContent ["package",pkg] = pure $ Package pkg
parseContent (reverse -> pkg:cont)
  | T.take 4 (head cont) == " :: " =
    pure $ Value (mconcat $ reverse $ tail cont) (T.drop 4 $ head cont) pkg

  | last cont == "type" && T.any (=='=') (head cont) =
    pure $ Type
        (mconcat $ reverse $ tail $ init cont)
        (T.words  $ T.takeWhile (/='=') $ head cont)
        (T.drop 2 $ T.dropWhile (/='=') $ head cont)
        pkg

  | T.take 5 (T.reverse (head cont)) == T.reverse "where" && last cont == "class" =
    pure $ Class
        (mconcat $ reverse $ tail $ init cont)
        (init $ T.words $ head cont)
        pkg

  | last cont == "module" =
    pure $ Module
        (mconcat $ reverse $ init cont)
        pkg

  | last cont == "data" =
    pure $ Data
        (mconcat $ reverse $ tail $ init cont)
        (T.words $ head cont)
        pkg


  | last cont == "newtype" =
    pure $ NewType
        (mconcat $ reverse $ tail $ init cont)
        (T.words $ head cont)
        pkg


parseContent x =
  Left $ unlines
      ["Error: No rule to parse: " ++ show x
      ,"Please report this."
      ]
