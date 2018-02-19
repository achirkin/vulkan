{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
module VkXml.Sections
  ( parseVkXml
  , VkXml (..)
  ) where

import           Control.Monad.State.Class
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Foldable             (toList)
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq
import           Data.XML.Types
import           Text.XML.Stream.Parse     as Xml

import           VkXml.Parser
import           VkXml.CommonTypes
import           VkXml.Sections.Commands
import           VkXml.Sections.Enums
import           VkXml.Sections.Extensions
import           VkXml.Sections.Feature
import           VkXml.Sections.Tags
import           VkXml.Sections.Types
import           VkXml.Sections.VendorIds



parseVkXml :: VkXmlParser m => Sink Event m VkXml
parseVkXml = fmap fixVkXml . execStateC
    (VkXmlPartial mempty mempty mempty mempty
                  mempty mempty mempty)
      $ tagIgnoreAttrs "registry" parseAll
  where
    parseAll = do
      mr <- choose
        [ ignoreTreeContent "comment"
        , parseVendorIds >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpVendorIds = gpVendorIds v |> x
                }
        , parseTags >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpTags = gpTags v |> x
                }
        , parseTypes >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpTypes = gpTypes v |> x
                }
        , parseVkEnums >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpEnums = gpEnums v |> x
                }
        , parseCommands >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpCommands = gpCommands v |> x
                }
        , parseFeature >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpFeature = gpFeature v |> x
                }
        , parseExtensions >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpExtensions = gpExtensions v |> x
                }
        ]
      case mr of
        Nothing -> return ()
        Just () -> parseAll



-- | Contains all parsed content of vk.xml,
--    hopefully, preserves ordering of original vk.xml.
--
--   The data type is foldable and traversable functor
data VkXml
  = VkXml
  { globVendorIds  :: VendorIds
  , globTags       :: VkTags
  , globTypes      :: Map VkTypeName VkType
  , globEnums      :: Map (Maybe VkTypeName) VkEnums
  , globCommands   :: Map VkCommandName VkCommand
  , globFeature    :: [VkFeature]
  , globExtensions :: Map VkExtensionName VkExtension
  } deriving Show

data VkXmlPartial
  = VkXmlPartial
  { gpVendorIds  :: Seq VendorIds
  , gpTags       :: Seq VkTags
  , gpTypes      :: Seq VkTypes
  , gpEnums      :: Seq VkEnums
  , gpCommands   :: Seq VkCommands
  , gpFeature    :: Seq VkFeature
  , gpExtensions :: Seq VkExtensions
  } deriving Show


fixVkXml :: VkXmlPartial
         -> VkXml
fixVkXml VkXmlPartial
  { gpVendorIds  = Seq.Empty Seq.:|> pVendorIds
  , gpTags       = Seq.Empty Seq.:|> pTags
  , gpTypes      = Seq.Empty Seq.:|> pTypes
  , gpEnums      = pEnums
  , gpCommands   = Seq.Empty Seq.:|> pCommands
  , gpFeature    = pFeatures
  , gpExtensions = Seq.Empty Seq.:|> pExtensions
  } = VkXml
  { globVendorIds  = pVendorIds
  , globTags       = pTags
  , globTypes      = pTypes
  , globEnums      = Map.fromList
                   . map (\e -> ( _vkEnumsTypeName e, e)
                         )
                   $ toList pEnums
  , globCommands   = pCommands
  , globFeature    = toList pFeatures
  , globExtensions = pExtensions
  }
fixVkXml _ = error "Unexpected number of sections in vk.xml"
