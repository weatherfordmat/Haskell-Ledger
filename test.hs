{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}

import System.Console.Terminal.Size
import Data.Maybe
import Control.Monad
import Language.Haskell.TH
import Control.Lens


file = "./transactions.txt"

-- Basic Bank Transaction
data Transaction = Transaction{
    _uid :: String,
    _user :: String,
    _accountNum :: String,
    _routingNumber :: String,
    _category :: String,
    _description :: String,
    _amount :: String,
    _updatedAt :: String
} deriving (Read, Show, Eq, Ord) 
makeLenses ''Transaction

getWidth = do
    result <- size
    let val = width $ fromJust result
    format val "-"

main = do
    let t = Transaction {_uid = "1506906301", _user = "MBW", _accountNum = "7506781205", _routingNumber = "1164459322", _category = "Rent", _description = "Rent", _amount = "-1224.12", _updatedAt = "1506906301"}
    cols _user (_user t)
    cols _user "User"

cols n o = do
    contents <- readFile file
    xs <- mapM (\x -> do
        let transaction = (read x :: Transaction)
        return transaction
        ) (lines contents)
    let result = map(\x -> n x) xs
    let s = length $ maximum $ result
    let st = s - length o
    let output = "| " ++ o ++ (replicate st ' ') ++ "|"
    print output

format :: Int -> String -> IO ()
format n s = do
    forM_ [1..(n-1)] $ (\x -> do
        putStr s)
    putStrLn ""