{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}

module VkXml.Sections.Extensions
  ( parseExtensions
  , VkExtensions, VkExtension (..), VkExtAttrs (..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Conduit
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser
import           VkXml.Sections.Feature

-- extension naming conventions
-- http://vulkan-spec-chunked.ahcox.com/apcs03.html



type VkExtensions = Map VkExtensionName VkExtension


data VkExtension
  = VkExtension
  { extAttributes :: VkExtAttrs
  , extRequires   :: [VkRequire]
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
  , extProtect   :: Maybe ProtectDef
  } deriving Show


-- | Try to parse current tag as being "vendorids",
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "vendorids", throw an exception
parseExtensions :: VkXmlParser m => Sink Event m (Maybe VkExtensions)
parseExtensions =
  parseTagForceAttrs "extensions" (forceAttr "comment") $ \_ ->
    Map.fromList
    . fmap (\e -> (extName $ extAttributes e,e))
    <$> many parseVkExtension


parseVkExtension :: VkXmlParser m => Sink Event m (Maybe VkExtension)
parseVkExtension =
    parseTagForceAttrs "extension" parseVkExtAttrs $ \extAttributes -> do
      extRequires <- many $ parseVkRequire
                              (extNumber extAttributes)
                              (extReqExts extAttributes)
      pure VkExtension {..}


parseVkExtAttrs :: ReaderT ParseLoc AttrParser VkExtAttrs
parseVkExtAttrs = do
  extName      <- forceAttr "name" >>= toHaskellExt
  extSupported <- forceAttr "supported"
  extContact   <- lift $ attr "contact"
  extAuthor    <- lift $ fmap VkTagName <$> attr "author"
  extType      <- lift $ attr "type"
  extProtect   <- lift (attr "protect") >>= mapM toProtectDef
  extReqExts   <- commaSeparated <$> lift (attr "requires")
                  >>= mapM toHaskellExt
  eextNumber   <- decOrHex <$> forceAttr "number"
  case eextNumber of
    Left err -> parseFailed $ "Could not parse extension.number: " ++ err
    Right (extNumber,_) ->  pure VkExtAttrs {..}
