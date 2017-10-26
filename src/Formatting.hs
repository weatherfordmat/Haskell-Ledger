{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}

module Formatting (
    clearConsole,
    colorPutPartial,
    colorPutStr,
    getWidth,
    format
) where

import System.Console.ANSI
import System.Console.Terminal.Size
import Data.Maybe
import Control.Monad
import Control.Concurrent

colorPutPartial :: String -> Color -> IO ()
colorPutPartial x color = do
    setSGR [SetColor Foreground Vivid color]
    putStr x
    setSGR [Reset]


clearConsole :: IO ()
clearConsole = do
    clearScreen
    setCursorPosition 1 0

getWidth :: IO ()
getWidth = do
    result <- size
    let val = width $ fromJust result
    format val "-"

format :: Int -> String -> IO ()
format n s = do
    forM_ [1..(n-1)] $ (\x -> do
        threadDelay (x*5)
        putStr s)
    putStrLn ""

colorPutStr :: String -> Color -> IO ()
colorPutStr x color = do
    setSGR [SetColor Foreground Vivid color]
    putStrLn x
    setSGR [Reset]