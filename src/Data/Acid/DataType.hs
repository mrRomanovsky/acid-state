{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module DataType where

--import GenDataType
import GHC.Generics
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.SafeCopy
import           Data.Typeable


data MyData = MyDataCons1 Int | MyDataCons2 Float deriving (Generic, Typeable)

$(deriveSafeCopy 0 'base ''MyData)

writeState :: Int -> Update MyData ()
writeState newValue
    = put (MyDataCons1 newValue)

queryState :: Query MyData Int
queryState = do MyDataCons1 x <- ask
                return x

-- $(makeAcidic ''MyData ['writeState, 'queryState])
