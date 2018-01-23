{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}

module VkXml.Sections.VendorIds
  ( parseVendorIds
  , VendorIds (..), VendorId (..)
  ) where

import           Data.Conduit
import           Data.Text             (Text)
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.Parser


data VendorIds
  = VendorIds
  { comment   :: Text
  , vendorids :: [VendorId]
  } deriving Show

data VendorId
  = VendorId
  { name    :: Text
  , comment :: Text
  , vid     :: Word
  } deriving Show

-- | Try to parse current tag as being "vendorids",
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "vendorids", throw an exception
parseVendorIds :: VkXmlParser m => Sink Event m (Maybe VendorIds)
parseVendorIds =
  parseTagForceAttrs "vendorids" (forceAttr "comment") $ \comment -> do
    vendorids <- many parseVendorId
    pure VendorIds {..}

parseVendorId :: VkXmlParser m => Sink Event m (Maybe VendorId)
parseVendorId = parseTagForceAttrs "vendorid" parseIt pure
  where
    parseIt = do
      name <- forceAttr "name"
      comment <- forceAttr "comment"
      evid <- decOrHex <$> forceAttr "id"
      case evid of
        Left err      -> parseFailed $ "Could not parse vendorid.id " ++ err
        Right (vid,_) -> pure VendorId {..}
