{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}

module VkXml.Sections.SPIRV
  ( parseSPIRVExtensions, parseSPIRVExtension
  , parseSPIRVCapabilities, parseSPIRVCapability
  , SPIRVExtensions, SPIRVExtension (..)
  , SPIRVCapabilities, SPIRVCapability (..)
  ) where

import           Control.Applicative        ((<|>))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Conduit
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                  (Text)
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser
import           VkXml.Sections.Enums

type SPIRVExtensions   = Map SPIRVExtensionName  SPIRVExtension
type SPIRVCapabilities = Map SPIRVCapabilityName SPIRVCapability

data SPIRVExtension
  = SPIRVExtension
  { extEnables  :: [Enable]
  } deriving Show

data SPIRVCapability
  = SPIRVCapability
  { capEnables :: [Enable]
  } deriving Show

data Enable
  = EnableMinVersion
    { enableMinVersion :: Text
    }
  | EnableExtension
    { enableExtension :: Text }
  | EnableFeature
    { enableFeatureStruct :: Text
    , enableFeature       :: Text
    , enableRequires      :: Maybe Text
    , enableAlias         :: Maybe Text
    }
  | EnableProperty
    { enablePropertyStruct :: Text
    , enableMember         :: Text
    , enableValue          :: Text
    , enableRequires       :: Maybe Text
    }
  deriving Show

-- | Try to parse current tag as being "spirvextensions",
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "spirvextension", throw an exception
parseSPIRVExtensions :: VkXmlParser m => Sink Event m (Maybe SPIRVExtensions)
parseSPIRVExtensions = parseTagForceAttrs "spirvextensions" (forceAttr "comment")
  $ \_ -> Map.fromList <$> many parseSPIRVExtension


parseSPIRVExtension :: VkXmlParser m => Sink Event m (Maybe (SPIRVExtensionName,SPIRVExtension))
parseSPIRVExtension = parseTagForceAttrs "spirvextension" (forceAttr "name") $ \name -> do
  extEnables <- many parseEnable
  pure (SPIRVExtensionName name, SPIRVExtension { .. })

-- | Try to parse current tag as being "spirvcapabilities",
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "spirvextension", throw an exception
parseSPIRVCapabilities :: VkXmlParser m => Sink Event m (Maybe SPIRVCapabilities)
parseSPIRVCapabilities = parseTagForceAttrs "spirvcapabilities" (forceAttr "comment")
  $ \_ -> Map.fromList <$> many parseSPIRVCapability

-- | Try to parse a SPIR-V capability.
parseSPIRVCapability :: VkXmlParser m => Sink Event m (Maybe (SPIRVCapabilityName,SPIRVCapability))
parseSPIRVCapability = parseTagForceAttrs "spirvcapability" (forceAttr "name") $ \name -> do
  capEnables <- many parseEnable
  pure (SPIRVCapabilityName name, SPIRVCapability { .. })

-- | Try to parse information about enabling conditions for a SPIR-V extension or capability.
parseEnable :: VkXmlParser m => Sink Event m (Maybe Enable)
parseEnable =
  parseTagForceAttrs "enable" 
    (   parseEnableMinVersion
    <|> parseEnableExtension
    <|> parseEnableFeature
    <|> parseEnableProperty
    )
    pure
  where
    parseEnableMinVersion, parseEnableExtension, parseEnableFeature, parseEnableProperty
      :: ReaderT ParseLoc AttrParser Enable
    parseEnableMinVersion = do
      enableMinVersion <- forceAttr "version"
      pure ( EnableMinVersion { .. } )
    parseEnableExtension = do
      enableExtension <- forceAttr "extension"
      pure ( EnableExtension { .. } )
    parseEnableFeature = do
      enableFeatureStruct <- forceAttr "struct"
      enableFeature       <- forceAttr "feature"
      enableRequires      <- lift $ attr "requires"
      enableAlias         <- lift $ attr "alias"
      pure ( EnableFeature { .. } )
    parseEnableProperty = do
      enablePropertyStruct <- forceAttr "property"
      enableMember         <- forceAttr "member"
      enableValue          <- forceAttr "value"
      enableRequires       <- lift $ attr "requires"
      pure ( EnableProperty { .. } )
