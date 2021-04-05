{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}

module VkXml.Sections.Extensions
  ( parseExtensions
  , VkExtensions, VkExtension (..), VkExtAttrs (..), VkSpecialUse (..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Conduit
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser
import           VkXml.Sections.Feature

-- extension naming conventions
-- http://vulkan-spec-chunked.ahcox.com/apcs03.html



type VkExtensions = Map VkExtensionName VkExtension

data VkSpecialUse
  = Cadsupport
  | D3demulation
  | Debugging
  | Devtools
  | Glemulation
  deriving (Eq, Show)

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
  , extReqCore   :: Maybe Text -- TODO: add a proper version handling
  , extProtect   :: Maybe ProtectDef
  , extPlatform  :: Maybe VkPlatformName
    -- ^ seems to be used in a similar way as extProtect
  , extComment   :: Maybe Text
  , extDeprecatedby :: Maybe VkExtensionName
  , extSpecialuse   :: Maybe VkSpecialUse
  , extPromotedto   :: Maybe VkExtensionName
  , extObsoletedby  :: Maybe VkExtensionName
  , extSortorder    :: Maybe Int
  , extProvisional  :: Bool
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

parseAttrVkExtensionName :: Name -> ReaderT ParseLoc AttrParser (Maybe VkExtensionName)
parseAttrVkExtensionName name = do
  mdb <- lift $ attr name
  case mdb of
    Nothing -> pure Nothing
    Just "" -> pure Nothing
    Just db -> Just <$> toHaskellExt db

parseAttrVkExtensionSpeciause :: ReaderT ParseLoc AttrParser (Maybe VkSpecialUse)
parseAttrVkExtensionSpeciause  = do
  su <- lift $ attr "specialuse"
  pure $ case su of
    Just "cadsupport"   -> Just Cadsupport
    Just "d3demulation" -> Just D3demulation
    Just "debugging"    -> Just Debugging
    Just "devtools"     -> Just Devtools
    Just "glemulation"  -> Just Glemulation
    _                   -> Nothing

parseAttrVkExtensionSortorder :: ReaderT ParseLoc AttrParser (Maybe Int)
parseAttrVkExtensionSortorder = do
  x <- lift $ attr "sortorder"
  pure $ case decOrHex <$> x of
    Just (Right (i, _)) -> Just (fromInteger i)
    _                          -> Nothing

parseAttrVkExtensionProvisional :: ReaderT ParseLoc AttrParser Bool
parseAttrVkExtensionProvisional = do
  mr <- lift $ attr "provisional"
  case T.toLower <$> mr of
    Just "true" -> pure True
    _           -> pure False

parseVkExtAttrs :: ReaderT ParseLoc AttrParser VkExtAttrs
parseVkExtAttrs = do
  extName      <- forceAttr "name" >>= toHaskellExt
  extSupported <- forceAttr "supported"
  extContact   <- lift $ attr "contact"
  extAuthor    <- lift $ fmap VkTagName <$> attr "author"
  extType      <- lift $ attr "type"
  extProtect   <- lift (attr "protect") >>= mapM toProtectDef
  extPlatform  <- lift $ fmap VkPlatformName <$> attr "platform"
  extReqExts   <- commaSeparated <$> lift (attr "requires")
                  >>= mapM toHaskellExt
  extReqCore   <- lift (attr "requiresCore")
  eextNumber   <- decOrHex <$> forceAttr "number"
  extComment   <- lift $ attr "comment"
  extDeprecatedby <- parseAttrVkExtensionName "deprecatedby"
  extSpecialuse   <- parseAttrVkExtensionSpeciause
  extPromotedto   <- parseAttrVkExtensionName "promotedto"
  extObsoletedby  <- parseAttrVkExtensionName "obsoletedby"
  extSortorder    <- parseAttrVkExtensionSortorder
  extProvisional  <- parseAttrVkExtensionProvisional
  case eextNumber of
    Left err -> parseFailed $ "Could not parse extension.number: " ++ err
    Right (extNumber,_) ->  pure VkExtAttrs {..}
