{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Extension
  ( genExtension
  ) where

import           Control.Monad.Reader.Class
import qualified Data.Map                             as Map
import           Data.Semigroup
import qualified Data.Text                            as T

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Types as Ts
import           VkXml.Sections.Commands as Cs
import           VkXml.Sections.Feature
import           VkXml.Sections.Extensions

import           Write.ModuleWriter
import           Write.Feature


genExtension :: Monad m => VkExtension -> ModuleWriter m (Maybe T.Text)
genExtension (VkExtension VkExtAttrs{..} ereqs) = do
    curlvl <- getCurrentSecLvl
    vkXml <- ask
    let VkFeature {..} = unInorder $ globFeature vkXml
        tps = Map.fromList
               . map (\t -> ((Ts.name :: VkType -> VkTypeName) t, t))
               . items . types . unInorder $ globTypes vkXml
        cmds = Map.fromList
               . map (\c -> ((Cs.name :: VkCommand -> VkCommandName) c, c))
               . commands . unInorder $ globCommands vkXml
    writeSection curlvl $ "Vulkan extension: @" <> unVkExtensionName extName <> "@"
       <:> ("supported: @" <> extSupported <> "@")
       <:> maybe mempty (\s -> "contact: @" <> s <> "@") extContact
       <:> maybe mempty (\s -> "author: @" <> unVkTagName s <> "@") extAuthor
       <:> maybe mempty (\s -> "type: @" <> s <> "@") extType
       <:> ("Extension number: @" <> T.pack (show extNumber) <> "@")
       <:> showExts extReqExts
       <:> maybe mempty (\s -> "Protected by CPP ifdef: @" <> s <> "@") extProtect

    pushSecLvl $ \lvl -> mapM_ (genRequire lvl tps cmds) ereqs

    if extSupported == "disabled"
    then return $ Just "DISABLED_EXTENSIONS_STUB"
    else return extProtect
