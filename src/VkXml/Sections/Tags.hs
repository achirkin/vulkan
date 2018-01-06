{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}

module VkXml.Sections.Tags
  ( parseTags
  , VkTags (..), VkTag (..)
  ) where

import           Data.Conduit
import           Data.Text             (Text)
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser

data VkTags
  = VkTags
  { comment :: Text
  , tags    :: [VkTag]
  } deriving Show

data VkTag
  = VkTag
  { name    :: VkTagName
  , author  :: Text
  , contact :: Text
  } deriving Show

-- | Try to parse current tag as being "tags",
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "tags", throw an exception
parseTags :: VkXmlParser m => Sink Event m (Maybe VkTags)
parseTags =
  parseTagForceAttrs "tags" (forceAttr "comment") $ \comment -> do
    tags <- many parseVkTag
    pure VkTags {..}

parseVkTag :: VkXmlParser m => Sink Event m (Maybe VkTag)
parseVkTag = parseTagForceAttrs "tag" parseIt pure
  where
    parseIt = do
      name    <- VkTagName <$> forceAttr "name"
      author  <- forceAttr "author"
      contact <- forceAttr "contact"
      pure VkTag {..}
