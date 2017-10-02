{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}

module Lib (
    fieldNames,
    listFields
) where

import Control.Lens
import Data.List (nub)
import Language.Haskell.TH

-- map (\x -> last $ splitOn "_" x) x
-- $(listE . map stringE =<< listFields ''Transaction)
listFields :: Name -> Q [String]
listFields name = do
  TyConI (DataD _ _ _ _ cons _) <- reify name
  return [nameBase fieldName
         | RecC conName fields <- cons
         , (fieldName, _, _) <- fields]

fieldNames :: Name -> ExpQ
fieldNames t = do
    TyConI (DataD _ _ _ _ constructors _) <- reify t
    let ns = nub (concatMap names constructors)
    l <- [| ns |]
    return l
    where
        names :: Con -> [String]
        names (RecC _ fields)   = map (nameBase . fst3) fields
        names (ForallC _ _ con) = names con
        names _ = []
        fst3 (x,_,_) = x
