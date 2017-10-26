{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}

module CRUD (
    lookForFile
) where

import System.Directory

lookForFile :: FilePath -> (Bool -> IO b) -> IO b
lookForFile file c = do 
    x <- doesFileExist file
    c x

