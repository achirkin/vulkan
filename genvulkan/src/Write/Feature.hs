{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Feature
  ( genFeature, genRequire, showExts
  ) where

import           Control.Monad
import           Control.Monad.Morph              (hoist)
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class        (MonadState (..))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.Map                         as Map
import           Data.Semigroup
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Language.Haskell.Exts.Syntax

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Commands          as Cs
import           VkXml.Sections.Types             as Ts
-- import           VkXml.Sections.Enums as Es
import           VkXml.Sections.Feature

import           Write.Commands
import           Write.ModuleWriter
import           Write.Types
import           Write.Types.Enum
import           Write.Types.Struct
import           Write.Util.DeclaredNames


genFeature :: Monad m => VkFeature -> ModuleWriter m ClassDeclarations
genFeature VkFeature {..} = hoist (`evalStateT` mempty) $ do
    curlvl <- getCurrentSecLvl
    vkXml <- ask
    let tps = globTypes vkXml
        cmds = globCommands vkXml
        -- ens = Map.fromList
        --     . map (\e -> (_vkEnumName e, e))
        --     $ Map.elems (globEnums vkXml) >>= items . _vkEnumsMembers . unInorder
            -- .. vkEnumMembers . traverse items
    writeSection curlvl $ T.unlines
      [ comment
      , ""
      , "@api = " <> api <> "@"
      , ""
      , "@name = " <> name <> "@"
      , ""
      , "@number = " <> number <> "@"
      ]
    pushSecLvl $ \lvl -> mconcat <$> mapM (genRequire lvl tps cmds) reqList


genRequire :: MonadState (Set VkTypeName) m
          => Int
          -> Map.Map VkTypeName VkType
          -> Map.Map VkCommandName VkCommand
          -- -> Map.Map VkEnumName VkEnum
          -> VkRequire
          -> ModuleWriter m ClassDeclarations
genRequire curlvl tps cmds VkRequire {..} = do
  writeOptionsPragma (Just HADDOCK) "not-home"
  writeSection curlvl $
    comment <:> showExts requireExts
  cds <- fmap mconcat $
    forM requireTypes $ \tname -> case Map.lookup tname tps of
        Nothing -> pure mempty
        Just t  -> do
          otn <- lift get
          if Set.member tname otn
          then pure mempty
          else do
            lift $ put (Set.insert tname otn)
            genType t
  tNames <- fmap mconcat $
    forM requireComms $ \cname -> case Map.lookup cname cmds of
        Nothing -> pure mempty
        Just t  -> genCommand t


  curM <- getCurrentModuleName
  otNames <- lift get
  lift $ put (tNames `Set.union` otNames)
  forM_ (tNames Set.\\ otNames) $ \(VkTypeName t) -> do
    let tnameDeclared = DIThing t DITAll
    mdecm <- lookupDiModuleImports tnameDeclared
    case mdecm of
      Nothing -> pure ()
      Just (m, ispecs)  -> when (m /= curM) $ do
        writeImport tnameDeclared
        if "FlagBits" `T.isInfixOf` t && length ispecs > 1
        then
          writeExportSpec . diToExportSpec $ DIThing t DITNo
        else
          mapM_ (writeExportSpec . i2espec) ispecs

  forM_ requireEnums $ enumPattern >=> mapM_ (writeExport . DIPat)
  return cds


showExts :: [VkExtensionName] -> T.Text
showExts [] = ""
showExts as = "Required extensions: " <> showExts' as
  where
    showExts' []     = "."
    showExts' [x]    = "'" <> unVkExtensionName x <> "'."
    showExts' (x:xs) = "'" <> unVkExtensionName x <> "', " <> showExts' xs
