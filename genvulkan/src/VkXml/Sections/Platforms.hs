{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}

module VkXml.Sections.Platforms
  ( parsePlatforms
  , VkPlatforms (..), VkPlatform (..)
  ) where

import           Control.Monad.Trans.Class
import           Data.Conduit
import           Data.Text             (Text)
import           Data.XML.Types
import           Text.XML.Stream.Parse
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe

import           VkXml.CommonTypes
import           VkXml.Parser

{- |
Folowing this registry page

<https://www.khronos.org/registry/vulkan/specs/1.1/registry.html#platform-platforms 8. Platform names>

-}


data VkPlatforms
  = VkPlatforms
  { comment   :: Text
  , platforms :: Map VkPlatformName VkPlatform
  } deriving Show

data VkPlatform
  = VkPlatform
  { name     :: VkPlatformName
  , protect  :: ProtectDef
  , comment  :: Text
  } deriving Show

-- | Try to parse current platform as being "platforms",
--
--   * If platform name does not match, return events upstream as leftovers
--   * If failed to parse platform "platforms", throw an exception
parsePlatforms :: VkXmlParser m => Sink Event m (Maybe VkPlatforms)
parsePlatforms =
  parseTagForceAttrs "platforms" (lift $ attr "comment") $ \mcomment -> do
    let comment = fromMaybe mempty mcomment
    platforms <- Map.fromList . map (\p -> (name p, p)) <$> many parseVkPlatform
    pure VkPlatforms {..}

parseVkPlatform :: VkXmlParser m => Sink Event m (Maybe VkPlatform)
parseVkPlatform = parseTagForceAttrs "platform" parseIt pure
  where
    parseIt = do
      name    <- VkPlatformName <$> forceAttr "name"
      protect <- forceAttr "protect" >>= toProtectDef
      comment <- lift $ fromMaybe mempty <$> attr "comment"
      pure VkPlatform {..}
