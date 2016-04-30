{-# Language OverloadedStrings, LambdaCase #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import PursuitClient

main :: IO ()
main =
    getArgs >>= \case
        [] -> putStrLn "usage: pursuit-search <search-string>"
        xs -> T.putStrLn . either T.pack showResults =<< search (concat xs)


showResults :: [Result] -> T.Text
showResults =
    T.unlines . map showResult

showResult :: Result -> T.Text
showResult result =
    T.unlines $ map ($ result)
        [ showContent . rCont
        , rUrl
        ]
