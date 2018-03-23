{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}

module VkXml.Sections.Feature
  ( parseFeature, parseVkRequire
  , VkFeature (..), VkRequire (..)
  ) where


import           Control.Monad.Trans.Class
import           Data.Conduit
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                 (Text)
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser
import           VkXml.Sections.Enums


data VkFeature
  = VkFeature
  { api     :: Text
  , name    :: Text
  , number  :: Text
  , comment :: Text
  , reqList :: [VkRequire]
  } deriving Show

--  "require" tags
-- https://www.khronos.org/registry/vulkan/specs/1.1/registry.html#tag-required


data VkRequire
  = VkRequire
  { comment        :: Text
  , requireFeature :: Maybe Text
    -- ^ require feature="VK_VERSION_XXX"
    --   (section 21.2)
  , requireExts    :: [VkExtensionName]
  , requireTypes   :: [VkTypeName]
  , requireEnums   :: [VkEnum]
  , requireComms   :: [VkCommandName]
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
  reqList <- many (parseVkRequire 0 [])
  pure VkFeature {..}

parseVkRequire :: VkXmlParser m
               => Int
               -> [VkExtensionName] -- ^ Extension names that are already required
               -> Sink Event m (Maybe VkRequire)
parseVkRequire extN baseExtReqs
    = parseTagForceAttrs "require" parseAttrs $ \iReq ->
        foldr ($) iReq <$> manyIgnore
            parseIt
          ( ignoreTreeContent "comment" )

  where
    -- https://www.khronos.org/registry/vulkan/specs/1.0/registry.html 19.1
    parseAttrs = do
      comm     <- lift $ fromMaybe mempty <$> attr "comment"
      mfeature <- lift $ attr "feature"
      mext     <- lift (attr "extension") >>= mapM toHaskellExt
      mprofile <- lift $ attr "profile"
      mapi     <- lift $ attr "api"
      return
        VkRequire
          { comment      = comm
              <:> maybe mempty (\s -> "profile = @" <> s <> "@") mprofile
              <:> maybe mempty (\s -> "api = @" <> s <> "@") mapi
          , requireFeature = mfeature
          , requireExts  = maybe baseExtReqs (:baseExtReqs) mext
          , requireTypes = []
          , requireEnums = []
          , requireComms = []
          }

    parseIt = choose
        [ parseTagForceAttrs "type"
            (forceAttr "name" >>= toHaskellType)
            (\x -> pure $ \r -> r {requireTypes = x : requireTypes r})
        , parseTagForceAttrs "command"
            (forceAttr "name" >>= toHaskellComm)
            (\x -> pure $ \r -> r {requireComms = x : requireComms r})
        , parseVkEnum extN Nothing >>= \case
              [] -> pure Nothing
              es -> pure . Just $ \r -> r {requireEnums = es ++ requireEnums r}
        ]
