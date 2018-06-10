{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Extension
  ( genExtension
  ) where

import           Control.Applicative              ((<|>))
import           Control.Monad.Morph              (hoist)
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.Map.Strict                  as Map
import           Data.Semigroup
import qualified Data.Text                        as T

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Extensions
import           VkXml.Sections.Platforms
import           Write.Commands
import           Write.Feature
import           Write.ModuleWriter


genExtension :: Monad m
             => ProtectDef
             -> VkExtension
             -> ModuleWriter m (Maybe ProtectDef)
genExtension unsafeFFIDefaultDef (VkExtension VkExtAttrs{..} ereqs) = hoist (`evalStateT` mempty) $ do
    curlvl <- getCurrentSecLvl
    vkXml <- ask
    let tps = globTypes vkXml
        cmds = globCommands vkXml
        pfs = platforms $ globPlatforms vkXml
        mplatform = extPlatform >>= \pn -> Map.lookup pn pfs
        genFFI = if extName `elem` whitelistedExtensions
                 then Nothing
                 else Just NFFIDisable

    writeSection curlvl $ "Vulkan extension: @" <> unVkExtensionName extName <> "@"
       <:> ("supported: @" <> extSupported <> "@")
       <:> maybe mempty (\s -> "contact: @" <> s <> "@") extContact
       <:> maybe mempty (\s -> "author: @" <> unVkTagName s <> "@") extAuthor
       <:> maybe mempty (\s -> "type: @" <> s <> "@") extType
       <:> maybe mempty (\s -> "platform: @" <> s <> "@") (unVkPlatformName <$> extPlatform)
       <:> ("Extension number: @" <> T.pack (show extNumber) <> "@")
       <:> showExts extReqExts
       <:> maybe mempty
          (\s -> "Protected by CPP ifdef: @" <> unProtectCPP (protectCPP s) <> "@")
          extProtect

    _ <- pushSecLvl $ \lvl -> mconcat <$> mapM (genRequire unsafeFFIDefaultDef genFFI lvl tps cmds) ereqs

    pure $ extProtect <|> protect <$> mplatform


-- | Vulkan loader does not expose all vulkan functions statically,
--   because different platforms and vendors have different extensions enabled
--    and different functions implemented.
--   However, Vulkan loader provides all core functions statically.
--   On top of this, vulkan provides symbols for some WSI extensions.
--
--   This list contains extensions that are guarenteed to be present in the
--   loader library.
whitelistedExtensions :: [VkExtensionName]
whitelistedExtensions = fmap VkExtensionName
  [ -- Vulkan loader provides symbols for WSI extensions functions
    "VK_KHR_surface"
  , "VK_KHR_swapchain"
  , "VK_KHR_display"
  , "VK_KHR_display_swapchain"
  , "VK_KHR_android_surface"
  , "VK_KHR_mir_surface"
  , "VK_KHR_wayland_surface"
  , "VK_KHR_win32_surface"
  , "VK_KHR_xcb_surface"
  , "VK_KHR_xlib_surface"
  ]
