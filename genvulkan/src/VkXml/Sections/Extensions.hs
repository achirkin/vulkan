{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}

module VkXml.Sections.Extensions
  ( parseExtensions
  , VkExtensions (..), VkExtension (..), VkExtAttrs (..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Conduit
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser
import           VkXml.Sections.Feature

-- extension naming conventions
-- http://vulkan-spec-chunked.ahcox.com/apcs03.html



data VkExtensions
  = VkExtensions
  { comment    :: Text
  , extensions :: [VkExtension]
  } deriving Show

data VkExtension
  = VkExtension
  { attributes  :: VkExtAttrs
  , extRequires :: [VkRequire]
  } deriving Show

data VkExtAttrs
  = VkExtAttrs
  { extName      :: VkExtensionName
  , extSupported :: Text
  , extContact   :: Maybe Text
  , extAuthor    :: Maybe VkTagName
  , extType      :: Maybe Text
  , extNumber    :: Int
  , extReqExts   :: [VkExtensionName]
  , extProtect   :: Maybe Text
  } deriving Show


-- | Try to parse current tag as being "vendorids",
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "vendorids", throw an exception
parseExtensions :: VkXmlParser m => Sink Event m (Maybe VkExtensions)
parseExtensions =
  parseTagForceAttrs "extensions" (forceAttr "comment") $ \comment -> do
    extensions <- many parseVkExtension
    pure VkExtensions {..}

parseVkExtension :: VkXmlParser m => Sink Event m (Maybe VkExtension)
parseVkExtension =
    parseTagForceAttrs "extension" parseVkExtAttrs $ \attributes -> do
      extRequires <- many $ parseVkRequire
                              (extNumber attributes)
                              (extReqExts attributes)
      pure VkExtension {..}


parseVkExtAttrs :: ReaderT ParseLoc AttrParser VkExtAttrs
parseVkExtAttrs = do
  extName      <- VkExtensionName <$> forceAttr "name"
  extSupported <- forceAttr "supported"
  extContact   <- lift $ attr "contact"
  extAuthor    <- lift $ fmap VkTagName <$> attr "author"
  extType      <- lift $ attr "type"
  extProtect   <- lift $ attr "protect"
  extReqExts   <- map VkExtensionName . maybe [] (T.split (',' ==))
              <$> lift (attr "requires")
  eextNumber   <- decOrHex <$> forceAttr "number"
  case eextNumber of
    Left err -> parseFailed $ "Could not parse extension.number: " ++ err
    Right (extNumber,_) ->  pure VkExtAttrs {..}
