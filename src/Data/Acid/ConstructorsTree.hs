{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Acid.ConstructorsTree where

import Data.Proxy
import GHC.Generics
import Data.Data
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type TypeRepStr = String

data Foo = Bar {x :: Int} | Baz {y :: Float} deriving Generic

data Simple = Simple {s :: Int} deriving Generic

class Constructors (f :: * -> *) where
  constructorNames :: proxy f -> [String]

instance Constructors f => Constructors (D1 c f) where
  constructorNames _ = constructorNames (Proxy :: Proxy f)

instance (Constructors x, Constructors y) => Constructors (x :+: y) where
  constructorNames _ = constructorNames (Proxy :: Proxy x) ++ constructorNames (Proxy :: Proxy y)

instance Constructor c => Constructors (C1 c f) where
  constructorNames _ = [conName (undefined :: t c f a)]

class Selectors rep where
  selectors :: Proxy rep -> [[(String, TypeRepStr)]]

instance (Selectors a, Selectors b) => Selectors (a :+: b) where
  selectors _ = selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b)

instance Selectors f => Selectors (D1 c f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance Selectors f => Selectors (C1 x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors _ = [join (selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b))]

instance (Selector s, Typeable t) => Selectors (S1 s (K1 R t)) where
  selectors _ =
    [[ ( selName (undefined :: M1 S s (K1 R t) ()) , show (typeOf (undefined :: t)) ) ]]

instance Selectors U1 where
  selectors _ = [[]]

data ConstructorInfo = ConstructorInfo {consName :: String, varNames :: [String]}
  deriving (Show, Read, Eq)

type ConstructorsInfo = [ConstructorInfo]

getConstructorsInfo :: (Generic a, Selectors (Rep a), Constructors (Rep a)) => Proxy a -> ConstructorsInfo
getConstructorsInfo (x :: Proxy a) = let proxy = Proxy :: Proxy (Rep a)
                                         cons = constructorNames proxy
                                         sels = selectors proxy
                                         in zipWith ConstructorInfo cons ((map show) <$> ((map snd) <$> sels))

migrationIsPossible :: ConstructorsInfo -> ConstructorsInfo -> Bool
migrationIsPossible c1 c2 = all (`elem` c2) c1

genProxy :: Name -> Q Exp
genProxy = return . SigE (ConE 'Proxy) . AppT (ConT ''Proxy) . ConT

main :: IO ()
main = do
  print $ "Howdy" -- selectors (Proxy :: Proxy (Rep Foo)) -- constructorNames (Proxy :: Proxy (Rep Foo))
  return ()

