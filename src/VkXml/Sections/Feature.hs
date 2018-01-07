{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}

module VkXml.Sections.Feature
  ( parseFeature
  , VkFeature (..), VkRequire (..)
  ) where

import           Data.Conduit
import           Data.Text             (Text)
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser


data VkFeature
  = VkFeature
  { api     :: Text
  , name    :: Text
  , number  :: Text
  , comment :: Text
  , reqList :: [VkRequire]
  } deriving Show

data VkRequire
  = VkRequire
  { comment      :: Text
  , requireTypes :: [VkTypeName]
  , requireEnums :: [VkEnumValueName]
  , requireComms :: [VkCommandName]
  } deriving Show

-- | Try to parse current tag as being "feature",
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "feature", throw an exception
parseFeature :: VkXmlParser m => Sink Event m (Maybe VkFeature)
parseFeature = parseTagForceAttrs "feature"
    ((,,,) <$> forceAttr "api"
           <*> forceAttr "name"
           <*> forceAttr "number"
           <*> forceAttr "comment"
    ) $ \(api, name, number, comment) -> do
  reqList <- many parseVkRequire
  pure VkFeature {..}

parseVkRequire :: VkXmlParser m => Sink Event m (Maybe VkRequire)
parseVkRequire = parseTagForceAttrs "require" (forceAttr "comment") $ \comm ->
    foldr ($) (VkRequire comm [] [] []) <$> many parseIt
  where
    parseIt = choose
        [ parseTagForceAttrs "type"
            (VkTypeName <$> forceAttr "name")
            (\x -> pure $ \r -> r {requireTypes = x : requireTypes r})
        , parseTagForceAttrs "command"
            (VkCommandName <$> forceAttr "name")
            (\x -> pure $ \r -> r {requireComms = x : requireComms r})
        , parseTagForceAttrs "enum"
            (VkEnumValueName <$> forceAttr "name")
            (\x -> pure $ \r -> r {requireEnums = x : requireEnums r})
        ]
