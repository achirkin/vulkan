{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Extension
  ( genExtension
  ) where

import           Control.Monad.Morph              (hoist)
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.State.Strict (evalStateT)
import           Data.Semigroup
import qualified Data.Text                        as T

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Extensions
import           Write.Feature
import           Write.ModuleWriter


genExtension :: Monad m => VkExtension
             -> ModuleWriter m (Maybe ProtectDef)
genExtension (VkExtension VkExtAttrs{..} ereqs) = hoist (`evalStateT` mempty) $ do
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

    _ <- pushSecLvl $ \lvl -> mconcat <$> mapM (genRequire lvl tps cmds) ereqs

    pure extProtect
