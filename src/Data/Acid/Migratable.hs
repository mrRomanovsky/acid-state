{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Data.Acid.Migratable where

class Migratable a b | b -> a, a -> b where
  readTmpFromFile :: String -> IO b
  migrate :: b -> a
  completnessTests :: [b]
  completnessTests = []
