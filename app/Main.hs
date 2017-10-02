{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}

module Main where
    
import Lib

import Control.Lens
import System.Directory
import System.Console.ANSI
import Language.Haskell.TH
import Control.Concurrent
import Control.Monad
import System.IO
import Data.UnixTime
import Data.Char
import Data.Bits
import Data.List.Split

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

match' x = case x of
    "uid" -> uid
    "user" -> user
    "accountNum" -> accountNum
    "routingNumber" -> routingNumber
    "category" -> category
    "description" -> description
    "amount" -> amount
    "updatedAt" -> updatedAt

-- fullUpdate t = foldl (\x y -> x .y) (uid .~ (_uid t)) [(user .~ "MBW")] $ t
update :: [Char] -> String -> Transaction -> Transaction
update field val t = ((match' field) .~ val) t

sec :: (t, t1) -> t1
sec (x,y) = y

formatHeaders :: IO ()
formatHeaders = do
    let str = id ("ID | Amount ($) | User | Routing # | Category | Description | Account # | Updated At ")
    colorPutStr str Red

formatTransactionOutput :: Transaction -> IO ()
formatTransactionOutput t
    | (read (_amount t)::Float) < 0 = putStrLn str
    | otherwise = colorPutStr str Green
    where str = id (_uid t ++ " | " ++ _amount t ++ " | " ++ _user t  ++ " | " ++ _routingNumber t++" | "++ _category t ++" | " ++_description t ++" | " ++ _accountNum t ++" | " ++ _updatedAt t)

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

colorPutStr :: String -> Color -> IO ()
colorPutStr x color = do
    setSGR [SetColor Foreground Vivid color]
    putStrLn x
    setSGR [Reset]

lookForFile :: IO ()
lookForFile = do 
    x <- doesFileExist file
    getDest x

getDest :: Bool -> IO ()
getDest x 
    | x == False = continue
    | otherwise = do
        f <- readFile file
        putStrLn ""
        colorPutStr "READING FILE" Blue
        format 250 "-"
        formatHeaders
        mapM (\x -> do
            let transaction = (read x :: Transaction)
            formatTransactionOutput transaction
            ) (lines f)
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
        | c == "A" = getTransaction
        | c == "D" = deleteLine
        | c == "S" = getSum
        | c == "C" = clearFile
        | c == "I" = getIncome
        | c == "E" = editTransaction
        | otherwise = do
            colorPutStr "END OF SESSION." Red

getTransaction :: IO ()
getTransaction = do
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

write :: String -> IO ()
write s = do
    writeFile file s
    writeFile file "\n"
    colorPutStr "CREATED FILE." Green
    colorPutStr "RECORDED TRANSACTION." Green
    clearScreen

updateFile :: String -> IO ()
updateFile s = do
    appendFile file s
    appendFile file "\n"
    colorPutStr "RECORDED TRANSACTION." Green
    clearScreen

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

delete :: String -> [Transaction] -> IO ()
delete del y = do
    writeFile file ""
    let xs = filter (\x -> (_uid x) /= del) y
    forM_ xs (\x -> do
        updateFile $ show x
        )

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

editTransaction :: IO()
editTransaction = do
    contents <- readFile file
    xs <- mapM (\x -> do
        let transaction = (read x :: Transaction)
        return transaction
        ) (lines contents)
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

format :: Int -> String -> IO ()
format n s = do
    forM_ [1..(n-1)] $ (\x -> do
        threadDelay (x*5)
        putStr s)
    putStrLn ""

names :: [[Char]]
names = do
    let x = $(fieldNames ''Transaction)
    getFields x
    where getFields xs = map (\x -> last $ splitOn "_" x) xs

main :: IO ()
main = do
    let x = names
    setTitle "Banking Transactions"
    lookForFile
