{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Feature
  ( genFeature, genRequire, showExts
  ) where

import           Control.Monad                        (forM_, forM, (>=>))
import           Control.Monad.Reader.Class
import qualified Data.Map                             as Map
import           Data.Semigroup
import qualified Data.Text                            as T

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Types as Ts
import           VkXml.Sections.Commands as Cs
-- import           VkXml.Sections.Enums as Es
import           VkXml.Sections.Feature

import           Write.Commands
import           Write.ModuleWriter
import           Write.Types
import           Write.Types.Enum
import           Write.Types.Struct


genFeature :: Monad m => VkFeature -> ModuleWriter m ClassDeclarations
genFeature VkFeature {..} = do
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


genRequire :: Monad m
          => Int
          -> Map.Map VkTypeName VkType
          -> Map.Map VkCommandName VkCommand
          -- -> Map.Map VkEnumName VkEnum
          -> VkRequire
          -> ModuleWriter m ClassDeclarations
genRequire curlvl tps cmds VkRequire {..} = do
  writeSection curlvl $
    comment <:> showExts requireExts
  cds <- fmap mconcat $
    forM requireTypes $ \tname -> case Map.lookup tname tps of
        Nothing -> pure mempty
        Just t  -> genType t
  forM_ requireComms $ \cname -> case Map.lookup cname cmds of
        Nothing -> pure ()
        Just t  -> genCommand t
  forM_ requireEnums $ enumPattern >=> mapM_ (writeExport . DIPat)
  return cds


showExts :: [VkExtensionName] -> T.Text
showExts [] = ""
showExts as = "Required extensions: " <> showExts' as
  where
    showExts' [] = "."
    showExts' [x] = "'" <> unVkExtensionName x <> "'."
    showExts' (x:xs) = "'" <> unVkExtensionName x <> "', " <> showExts' xs
