{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}

module Main where
    
-- Custom Libs    
import Lib
import Formatting
import CRUD

-- Packages
import Control.Lens

import System.Console.ANSI
import Language.Haskell.TH
import System.Directory
import Control.Concurrent
import Control.Monad
import System.IO
import Data.UnixTime
import Data.Char
import Data.Bits
import Data.List.Split
import Data.List (sort, sortBy)
import Data.Maybe
import System.Console.Terminal.Size

file = "./.transactions.txt"

headers = [(_uid, "ID"), (_amount, "AMOUNT ($)"), (_user, "USER"), (_routingNumber, "RTNG #"), (_accountNum, "ACCT #"), (_updatedAt, "UPDATED AT"), (_description, "DESC")]

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

match' :: Functor f => [Char] -> (String -> f String) -> Transaction -> f Transaction
match' x = case x of
    "uid" -> uid
    "user" -> user
    "accountNum" -> accountNum
    "routingNumber" -> routingNumber
    "category" -> category
    "description" -> description
    "amount" -> amount
    "updatedAt" -> updatedAt

cols :: (Foldable t, Ord (t a)) => (Transaction -> t a) -> [Char] -> Color -> IO ()
cols n o c = do
    xs <- getTransactionRecords
    let result = map(\x -> n x) xs
    let s = length $ maximum $ result
    let st = s - (length o) + 5
    let output = "| " ++ o ++ (replicate st ' ')
    colorPutPartial output c

-- fullUpdate t = foldl (\x y -> x .y) (uid .~ (_uid t)) [(user .~ "MBW")] $ t
update :: [Char] -> String -> Transaction -> Transaction
update field val t = ((match' field) .~ val) t

sec :: (t, t1) -> t1
sec (x,y) = y

getTransactionRecords :: IO [Transaction]
getTransactionRecords = do
    contents <- readFile file
    xs <- mapM (\x -> do
        let transaction = (read x :: Transaction)
        return transaction
        ) (lines contents)
    return xs

formatHeaders :: IO ()
formatHeaders = do
    mapM_ (\x -> cols (fst x) (sec x) Red) headers
    putStrLn ""

colorTrans :: Transaction -> Color -> IO ()
colorTrans t c = do
    mapM_ (\x -> cols (fst x) ((fst x) t) c) headers
    putStrLn ""

formatTransactionOutput :: Transaction -> IO ()
formatTransactionOutput t 
    | (read (_amount t)::Float) < 0 = colorTrans t Black
    | otherwise = colorTrans t Green

change' :: (Transaction -> Transaction) -> String -> [Transaction] -> IO ()
change' l uidValue = mergeData' ((uid .~ uidValue) . l) uidValue

mergeData' :: (Transaction -> Transaction) -> String -> [Transaction] -> IO ()
mergeData' f _ = mapM_ (print . f)

getVals :: IO [String]
getVals = do
    putStrLn "What is the new value?"
    val <- getLine
    putStrLn "Which record do you want to edit? (Enter ID #)"
    uid <- getLine
    time <- getUnixTime
    let updatedAt = show $ utSeconds time
    return [val, uid, updatedAt]

getDest :: Bool -> IO ()
getDest x 
    | x == False = continue
    | otherwise = do
        f <- readFile file
        putStrLn ""
        getWidth
        formatHeaders
        z <- sortIt
        mapM_ (formatTransactionOutput) (z)
        getWidth
        let uid = (length $ lines f) + 1
        continue

continue :: IO ()
continue = do
    putStrLn ""
    colorPutStr "Transaction Menu" Red
    putStrLn "Options: (A) Add | (C) Clear All Records | (D) Delete Transaction | (E) Edit Record | (S) Sum | (Q) Quit  "
    c <- getLine
    nextMove c

nextMove :: [Char] -> IO ()
nextMove c
        | c == "\\a" = addTransaction
        | c == "\\d" = deleteLine
        | c == "\\s" = getSum
        | c == "\\c" = clearFile
        | c == "\\i" = getIncome
        | c == "\\e" = editTransaction
        | c == "\\q" = colorPutStr "CLOSING APPLICATION." Red
        | otherwise = do
            colorPutStr ("COMMAND " ++ c ++ " NOT RECOGNIZED.") Red
            continue

addTransaction :: IO ()
addTransaction = do
    time <- getUnixTime
    putStrLn ""
    putStrLn "Enter Transaction Amount:"
    total <- getLine
    putStrLn "Enter Category"
    category <- getLine
    putStrLn "Enter Description"
    desc <- getLine
    let transfer = Transaction{
        _uid = (show $ utSeconds time),
        _user = "Matthew Weatherford",
        _accountNum = "7506781205",
        _routingNumber = "1164459322",
        _category = category,
        _description = desc,
        _amount = total,
        _updatedAt = (show $ utSeconds time)
    }
    updateFile (show transfer)
    getDest True

sortIt :: IO [Transaction]
sortIt = do
    xs <- getTransactionRecords
    let result = map(\x -> ((read (_amount x) :: Float), x)) xs
    let y = sort result
    let z = map (sec) y
    return z


delete :: String -> [Transaction] -> IO ()
delete del y = do
    writeFile file ""
    let xs = filter (\x -> (_uid x) /= del) y
    forM_ xs (\x -> do
        updateFile $ show x
        )
    getDest True

updateFile :: String -> IO ()
updateFile s = do
    appendFile file s
    appendFile file "\n"
    colorPutStr "RECORDED TRANSACTION." Green
    clearConsole

clearFile :: IO ()
clearFile = do
    putStrLn "Are you sure you want to delete your transactions? (Y|N)"
    confirm <- getLine
    clearingFile confirm

clearingFile :: [Char] -> IO ()
clearingFile answer
    | answer == "Y" = do
        colorPutStr "Deleting File" Red
        removeFile file
    | otherwise = getDest True

deleteLine :: IO ()
deleteLine = do
        contents <- readFile file
        putStrLn "Which Line do you want to delete?"
        del <- getLine
        y <- mapM (\x -> do
            let transaction = (read x :: Transaction)
            return transaction
            ) (lines contents)
        delete del y

routeEditTransaction :: [Transaction] -> Transaction -> String -> IO ()
routeEditTransaction xs t idT = do
    putStrLn "Which field do you want to edit?"
    field <- getLine
    let n = map (toLower) field
    putStrLn "What is the new value?"
    val <- getLine
    let r = update n val t
    delete idT xs
    updateFile $ show r
    getDest True

editTransaction :: IO()
editTransaction = do
    contents <- readFile file
    xs <- getTransactionRecords
    putStrLn "Which record do you want to edit?"
    idT <- getLine
    let item = filter (\x -> (_uid x) == idT) xs
    case item of
        [] -> colorPutStr "No Records Available" Red
        (first:_) -> do
            formatHeaders
            formatTransactionOutput first
            routeEditTransaction xs first idT
    
getSum :: IO ()
getSum = do
    contents <- readFile file
    y <- mapM (\x -> do
        let transaction = (read x :: Transaction)
        return $ (read (_amount transaction) :: Float)
        ) (lines contents)
    putStr "Total: $"
    print $ sum y
    continue

getIncome :: IO ()
getIncome = do
    contents <- readFile file
    y <- mapM (\x -> do
        let transaction = (read x :: Transaction)
        return $ (read (_amount transaction) :: Float)
        ) (lines contents)
    let income = filter (>0) y
    putStr "Total: $"
    print $ sum income
    continue

names :: [[Char]]
names = do
    let x = $(fieldNames ''Transaction)
    getFields x
    where getFields xs = map (\x -> last $ splitOn "_" x) xs

main :: IO ()
main = do
    setTitle "Banking Transactions"
    lookForFile file getDest
