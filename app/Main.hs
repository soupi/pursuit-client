{-# Language LambdaCase #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Lib

main :: IO ()
main =
    getArgs >>= \case
        [] -> putStrLn "usage: pursuit-search <search-string>"
        xs -> T.putStrLn . showResults =<< search (concat xs)


showResults :: [Result] -> T.Text
showResults =
    T.unlines . map showResult

showResult :: Result -> T.Text
showResult result =
    T.unlines $ map ($ result)
        [ rSig
        , rMod
        , rUrl
        ]
