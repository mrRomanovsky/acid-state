{-# LANGUAGE TemplateHaskell #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Data.Acid.GenDataType where

import Data.Acid.Local
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Proxy
import Data.Data
import Data.Foldable (asum)
import System.Directory

import Data.Acid.ConstructorsTree
import Data.Acid.Migratable
import GHC.Generics hiding (NoSourceStrictness, NoSourceUnpackedness)

import Data.Acid.Archive
import Data.Acid.Log as Log
import Data.Acid.Core
import Data.Acid.Common
import Data.Acid.Abstract

import Control.Monad                  ( join )
import Control.Applicative            ( (<$>), (<*>) )
import Control.Exception

import Data.SafeCopy                  ( SafeCopy(..), safeGet, safePut
                                      , primitive, contain )
import Data.Typeable                  ( Typeable, typeOf )
import System.FilePath                ( (</>), takeDirectory )
import System.FileLock

prepareMigration :: (Generic a, Selectors (Rep a), Constructors (Rep a)) => Proxy a -> Name -> Q [Dec]
prepareMigration p tName = do
  let constructorsInfo = getConstructorsInfo p
  tmpType <- buildTempType tName constructorsInfo
  runIO $ writeFile "oldTypeDesc" $ nameBase tName ++ "\n" ++ (show constructorsInfo)
  tmpMigration <- genTmpMigration tName
  migrateToTmpFunc <- genMigrateToTmp
  let tempableInstType = ((ConT $ mkName "Tempable") `AppT` (ConT tName)) `AppT` (ConT $ mkName $ nameBase tName ++ "tmp")
      tempableInst = InstanceD Nothing [] tempableInstType [migrateToTmpFunc]
  return $ tmpType ++ tmpMigration ++ [tempableInst]

loadTmpType :: Q [Dec]
loadTmpType = do
  (tNameStr : consInfoStr : _) <- runIO $ lines <$> readFile "oldTypeDesc"
  let tName = mkName tNameStr
      constructorsInfo = read consInfoStr :: ConstructorsInfo 
  tmpType <- buildTempType tName constructorsInfo
  return tmpType

prepareMigratable :: Q [Dec]
prepareMigratable = do
  (tNameStr : _ : _) <- runIO $ lines <$> readFile "oldTypeDesc"
  let tName = mkName tNameStr
  migratableInstance <- genMigratable tName
  return $ migratableInstance

buildTempType :: Name -> [ConstructorInfo] -> Q [Dec]
buildTempType tName consInfo = do
  let ty_name = mkName $ nameBase tName ++ "tmp"
  constructors <- sequence $ buildCons <$> consInfo
  let derShowClause = DerivClause Nothing [ConT ''Show] --[AppT (ConT ''Show) (ConT ty_name)]
      derReadClause = DerivClause Nothing [ConT ''Read] --[AppT (ConT ''Read) (ConT ty_name)]
  let dataType = DataD [] ty_name [] Nothing constructors [derShowClause, derReadClause]
  return [dataType]
    where
      buildCons (ConstructorInfo nm vars) = do
        let consName = mkName $ nm ++ "tmp"
            varTypes = (ConT . mkName . filter (/= '"')) <$> vars
            consVars = (\t -> ((Bang NoSourceUnpackedness NoSourceStrictness), t)) <$> varTypes
        return $ NormalC consName consVars

genTmpMigration :: Name -> Q [Dec]
genTmpMigration ty = do
  (TyConI tyCon) <- reify ty
  (tyConName, tyVars, cs) <- case tyCon of
    DataD _ nm tyVars _ cs _   -> return (nm, tyVars, cs)
    NewtypeD _ nm tyVars _ c _ -> return (nm, tyVars, [c])
    _ -> fail "genCons: tyCon may not be a type synonym."
  lams <- sequence $ buildFuncBody <$> cs
  migrateFunc <- funD (mkName "migrateToTmp") lams
  return [migrateFunc]
  where
    buildFuncBody c@(NormalC name fieldTypes) = do
      fieldNames <- replicateM (length fieldTypes) (newName "x")
      let pats = [conP name (map varP fieldNames)]
          body = normalB $ appsE $ conE (mkName $ nameBase name ++ "tmp") : map varE fieldNames
      return $ clause pats body []

restoreData :: (IsAcidic st, Migratable st tmp, SafeCopy st, Typeable st) => Proxy st -> Q [Dec]
restoreData (p :: Proxy st) = do
  runIO $ catch (checkCompletness p) completnessHandler
  tmp <- runIO $ readTmpFromFile "tmpMigrated"
  let stData = migrate tmp :: st
  !_ <- runIO $ openLocalState stData
  return []

checkCompletness :: Migratable st tmp => Proxy st -> IO ()
checkCompletness (p :: Proxy st) = do
  let !_ = map (\x -> let !y = (migrate x :: st) in y) completnessTests
  return ()

completnessHandler :: SomeException -> IO ()
completnessHandler e =  do
    case fromException e of
        Just (x:: PatternMatchFail) -> putStrLn "Migrate function is not complete! Please append clauses to userMigrate!"
                                       >> throwIO e
        _ -> return ()

genMigratable :: Name -> Q [Dec]
genMigratable tName = do
  let tmpTypeName = mkName $ nameBase tName ++ "tmp"
      instanceType = ((ConT $ mkName "Migratable") `AppT` (ConT tName)) `AppT` (ConT $ tmpTypeName)
  readTmpFunc <- genReadTmp
  migrateFunc <- genMigrationFromTmp tName tmpTypeName
  completnessTests <- prepareCompletnessTests tmpTypeName
  let inst = InstanceD Nothing [] instanceType [readTmpFunc, migrateFunc, completnessTests]
  return [inst]

genReadTmp :: Q Dec
genReadTmp = do
  body <- [| \fpath -> read <$> readFile fpath |]
  return $ FunD (mkName "readTmpFromFile")
                [Clause [] (NormalB body) []]

genMigrateToTmp :: Q Dec
genMigrateToTmp = do
  body <- [| \x -> migrateToTmp x |]
  return $ FunD (mkName "dataToTemp") [Clause [] (NormalB body) []]


genMigrationFromTmp :: Name -> Name -> Q Dec
genMigrationFromTmp actualType tmpType = do
  (tyConName, tyVars, cs) <- getDataConstructors actualType
  (tmpTyConName, tmpTyVars, tmpCs) <- getDataConstructors tmpType
  lamsPairs <- sequence $ buildFuncBody cs <$> tmpCs
  let builtMaps = fst <$> filter snd lamsPairs
  lams <- if any (not . snd) lamsPairs
    then do
      usrMigration <- userMigration
      return $ builtMaps ++ [usrMigration]
    else return builtMaps
  migrateFunc <- funD (mkName "migrate") lams
  return migrateFunc
  where
    buildFuncBody cs c@(NormalC name fieldTypes) = do
      let actualConsName = reverse $ drop 3 $ reverse $ nameBase name
      if findCons (NormalC (mkName actualConsName) fieldTypes) cs
        then do
          fieldNames <- replicateM (length fieldTypes) (newName "x")
          let pats = [conP name (map varP fieldNames)]
              body = normalB $ appsE $
                conE (mkName $ actualConsName) : map varE fieldNames
          return (clause pats body [], True)
        else
          return (undefined, False)
    userMigration = do
      pName <- newName "x"
      let pats = [varP pName]
          body = normalB $ appE (varE (mkName "userMigrate")) (varE pName)
      return $ clause pats body []

prepareCompletnessTests :: Name -> Q Dec
prepareCompletnessTests tName = do
    (_, _, cs) <- getDataConstructors tName
    let body = normalB $ listE $ buildUndefTest <$> cs
    completnessTests <- funD (mkName "testCompletness")
      [clause [] body []]
    return completnessTests
    where
        buildUndefTest c@(NormalC name fieldTypes) = do
            let fields = replicate (length fieldTypes) $ varE $ mkName "undefined"
            appsE $ conE name : fields

findCons :: Con -> [Con] -> Bool
findCons c cs = any (consMatch c) cs
  where
    consMatch c1@(NormalC name1 fieldTypes1) c2@(NormalC name2 fieldTypes2) =
      nameBase name1 == nameBase name2 && fieldTypes1 == fieldTypes2

getDataConstructors :: Name -> Q (Name, [TyVarBndr], [Con])
getDataConstructors tName = do
  (TyConI tyCon) <- reify tName
  (tyConName, tyVars, cs) <- case tyCon of
    DataD _ nm tyVars _ cs _   -> return (nm, tyVars, cs)
    NewtypeD _ nm tyVars _ c _ -> return (nm, tyVars, [c])
    _ -> fail "genCons: tyCon may not be a type synonym."
  return (tyConName, tyVars, cs)

class Tempable a b | b -> a, a -> b where
  dataToTemp :: a -> b

saveOldData :: (Tempable a b, Show b, Typeable a, IsAcidic a, SafeCopy a) => a -> Q [Dec]
saveOldData (x :: a) = do
  oldAcid <- runIO $ openLocalState x --readSavedState x defaultSerialisationLayer
  runIO $ createCheckpoint oldAcid
  oldData <- runIO $ readSavedState x defaultSerialisationLayer
  let tmp = dataToTemp oldData
  runIO $ writeFile "tmpMigrated" $ show tmp
  --runIO $ removeDirectory "state"
  return []

readSavedState :: SafeCopy a => a -> SerialisationLayer a -> IO a
readSavedState defaultVal serialisationLayer = do
  let checkpointsLogKey = LogKey { logDirectory = "state"
                                 , logPrefix = "checkpoints"
                                 , logSerialiser = checkpointSerialiser serialisationLayer
                                 , logArchiver = archiver serialisationLayer }
  loadCheckpointWithDefault checkpointsLogKey
    where
      loadCheckpointWithDefault cLogKey = do
        mbLastCheckpoint <- Log.newestEntry cLogKey
        case mbLastCheckpoint of
          Nothing ->
            return defaultVal
          Just (Checkpoint _ !val) ->
            return val
