{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Extension
  ( genExtension
  ) where

import           Control.Monad.Reader.Class
-- import qualified Data.Map                             as Map
import           Data.Semigroup
import qualified Data.Text                            as T

import           VkXml.CommonTypes
import           VkXml.Sections
-- import           VkXml.Sections.Types as Ts
-- import           VkXml.Sections.Commands as Cs
-- import           VkXml.Sections.Feature
import           VkXml.Sections.Extensions

import           Write.ModuleWriter
import           Write.Feature
import           Write.Types.Struct


genExtension :: Monad m => VkExtension
             -> ModuleWriter m (ClassDeclarations, Maybe ProtectDef)
genExtension (VkExtension VkExtAttrs{..} ereqs) = do
    curlvl <- getCurrentSecLvl
    vkXml <- ask
    let tps = globTypes vkXml
        cmds = globCommands vkXml
    writeSection curlvl $ "Vulkan extension: @" <> unVkExtensionName extName <> "@"
       <:> ("supported: @" <> extSupported <> "@")
       <:> maybe mempty (\s -> "contact: @" <> s <> "@") extContact
       <:> maybe mempty (\s -> "author: @" <> unVkTagName s <> "@") extAuthor
       <:> maybe mempty (\s -> "type: @" <> s <> "@") extType
       <:> ("Extension number: @" <> T.pack (show extNumber) <> "@")
       <:> showExts extReqExts
       <:> maybe mempty
          (\s -> "Protected by CPP ifdef: @" <> unProtectCPP (protectCPP s) <> "@")
          extProtect

    cds <- pushSecLvl $ \lvl -> mconcat <$> mapM (genRequire lvl tps cmds) ereqs

    fmap ((,) cds) $
      if extSupported == "disabled"
      then pure $ Just (ProtectDef "DISABLED_EXTENSIONS_STUB"  "disabledExtensionsStub")
      else pure extProtect
