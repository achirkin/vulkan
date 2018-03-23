{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module VkXml.Sections
  ( parseVkXml
  , VkXml (..)
  , reexportedTypesRequire
  , reexportedTypesFeature
  , reexportedTypesExtension
  , evalProtectedTypes
  , removeDisabledTypes
  ) where

import           Control.Monad.State.Class
import           Control.Arrow ((***), second)
import           Control.Applicative ((<|>))
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.Maybe
import           Data.Foldable             (toList)
import           Data.List                 (nub)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq
import           Data.XML.Types
import           Text.XML.Stream.Parse     as Xml

import           VkXml.CommonTypes
import           VkXml.Parser
import           VkXml.Sections.Commands
import           VkXml.Sections.Enums
import           VkXml.Sections.Extensions
import           VkXml.Sections.Feature
import           VkXml.Sections.Tags
import           VkXml.Sections.Types
import           VkXml.Sections.VendorIds
import           VkXml.Sections.Platforms



parseVkXml :: VkXmlParser m => Sink Event m VkXml
parseVkXml = fmap fixVkXml . execStateC
    (VkXmlPartial mempty mempty mempty mempty
                  mempty mempty mempty mempty)
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
        , parsePlatforms >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpPlatforms = gpPlatforms v |> x
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
  , globPlatforms  :: VkPlatforms
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
  , gpPlatforms  :: Seq VkPlatforms
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
  , gpPlatforms  = Seq.Empty Seq.:|> pPlatforms
  , gpTags       = Seq.Empty Seq.:|> pTags
  , gpTypes      = Seq.Empty Seq.:|> pTypes
  , gpEnums      = pEnums
  , gpCommands   = Seq.Empty Seq.:|> pCommands
  , gpFeature    = pFeatures
  , gpExtensions = Seq.Empty Seq.:|> pExtensions
  } = VkXml
  { globVendorIds  = pVendorIds
  , globPlatforms  = pPlatforms
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


reexportedTypesRequire :: VkXml -> VkRequire -> [VkTypeName]
reexportedTypesRequire VkXml {..} VkRequire {..} = nub $
  requireTypes ++
    ( requireComms
      >>= maybeToList . (`Map.lookup` globCommands)
      >>= requiresTypes
    )

reexportedTypesFeature :: VkXml -> VkFeature -> [VkTypeName]
reexportedTypesFeature vkXml VkFeature {..}
 = nub $ reqList >>= reexportedTypesRequire vkXml


reexportedTypesExtension :: VkXml -> VkExtension -> [VkTypeName]
reexportedTypesExtension vkXml VkExtension {..}
 = nub $ extRequires >>= reexportedTypesRequire vkXml


evalProtectedTypes :: VkXml
                   -> Map VkTypeName VkType
                   -> Map VkTypeName (VkType, Maybe ProtectDef)
evalProtectedTypes vkXml ts
    = flip (Set.foldr' $ Map.adjust (second $ const Nothing)) unprotectedL
    . flip (Map.foldrWithKey' $ \d ->
              flip (Set.foldr' $ Map.adjust (second . const $ Just d))
           ) protectedL
    $ fmap (flip (,) Nothing) ts
  where
    ui = Map.unionWith Set.union
    (unprotectedL, protectedL)
      = (fromMaybe mempty *** Map.mapKeys fromJust)
      $ Map.updateLookupWithKey (\_ _ -> Nothing) Nothing protectLists'
    protectLists' :: Map (Maybe ProtectDef) (Set VkTypeName)
    protectLists' = Map.singleton Nothing
      ( foldMap
        (Set.fromList . reexportedTypesFeature vkXml)
        $ globFeature vkXml
      ) `ui`
      foldl ui mempty (map f $ Map.elems $ globExtensions vkXml)
    pfs = platforms $ globPlatforms vkXml
    getPlatform mpn = mpn >>= \pn -> Map.lookup pn pfs
    f :: VkExtension -> Map (Maybe ProtectDef) (Set VkTypeName)
    f e = Map.singleton
      (extProtect as <|> protect <$> getPlatform (extPlatform as) )
      (Set.fromList $ reexportedTypesExtension vkXml e)
      where
        as = extAttributes e


removeDisabledTypes :: VkXml
                    -> Map VkTypeName VkType
                    -> Map VkTypeName VkType
removeDisabledTypes vkXml ts
    = Set.foldr Map.delete ts toDisable
  where
    toDisable = disabledL Set.\\ enabledL
    ui = Map.unionWith Set.union
    disabledL = fromMaybe mempty $ Map.lookup False enablingLists'
    enabledL = fromMaybe mempty $ Map.lookup True enablingLists'
    enablingLists' :: Map Bool (Set VkTypeName)
    enablingLists' = Map.singleton True
      ( foldMap
        (Set.fromList . reexportedTypesFeature vkXml)
        $ globFeature vkXml
      ) `ui`
      foldl ui mempty (map f $ Map.elems $ globExtensions vkXml)
    f :: VkExtension -> Map Bool (Set VkTypeName)
    f e = Map.singleton
      (extSupported (extAttributes e) /= "disabled")
      (Set.fromList $ reexportedTypesExtension vkXml e)
