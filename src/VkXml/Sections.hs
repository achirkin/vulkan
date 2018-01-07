{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE RecordWildCards       #-}
module VkXml.Sections
  ( parseVkXml
  , VkXml (..), InOrder (..)
  ) where

import           Control.Monad.State.Class
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.Foldable             (toList)
import           Data.Sequence             (Seq, (|>))
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text)
import           Data.List                 (sort)
import           Data.Semigroup
import           Data.XML.Types
import           Text.XML.Stream.Parse     as Xml

import           VkXml.Parser
import           VkXml.Sections.Commands
import           VkXml.Sections.Enums
import           VkXml.Sections.Extensions
import           VkXml.Sections.Feature
import           VkXml.Sections.Tags
import           VkXml.Sections.Types
import           VkXml.Sections.VendorIds



parseVkXml :: VkXmlParser m => Sink Event m (VkXml ())
parseVkXml = fmap fixVkXml . execStateC
    (VkXmlPartial mempty mempty mempty mempty
                  mempty mempty mempty mempty 0)
      $ tagIgnoreAttrs "registry" parseAll
  where
    parseAll = do
      mr <- choose
        [ tagIgnoreAttrs "comment" $ do
            com <- content
            modify' $ \v -> v
              { gpComments = gpComments v |> inOrd (gpCurLength v) com
              , gpCurLength = gpCurLength v + 1
              }
        , parseVendorIds >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpVendorIds = gpVendorIds v |> inOrd (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseTags >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpTags = gpTags v |> inOrd (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseTypes >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpTypes = gpTypes v |> inOrd (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseEnums >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpEnums = gpEnums v |> inOrd (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseCommands >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpCommands = gpCommands v |> inOrd (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseFeature >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpFeature = gpFeature v |> inOrd (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseExtensions >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpExtensions = gpExtensions v |> inOrd (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        ]
      case mr of
        Nothing -> return ()
        Just () -> parseAll



data InOrder a l = InOrder
  { getOrder  :: Int
  , getMeta   :: l
  , unInorder :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

inOrd :: Int -> a -> InOrder a ()
inOrd i = InOrder i ()

ordAndMeta :: InOrder a l -> Arg Int l
ordAndMeta InOrder {..} = Arg getOrder getMeta

fromArg :: Arg a b -> b
fromArg (Arg _ b) = b

-- | Contains all parsed content of vk.xml,
--    hopefully, preserves ordering of original vk.xml.
--
--   The data type is foldable and traversable functor
data VkXml l
  = VkXml
  { globComments   :: [InOrder Text l]
  , globVendorIds  :: InOrder VendorIds l
  , globTags       :: InOrder VkTags l
  , globTypes      :: InOrder VkTypes l
  , globEnums      :: [InOrder VkEnums l]
  , globCommands   :: InOrder VkCommands l
  , globFeature    :: InOrder VkFeature l
  , globExtensions :: InOrder VkExtensions l
  , globLength     :: Int
  } deriving (Show, Functor, Traversable)

instance Foldable VkXml where
  length = globLength
  null   = (0==) . globLength
  toList VkXml {..}
    = map fromArg
    $ sort [ ordAndMeta globVendorIds
           , ordAndMeta globTags
           , ordAndMeta globTypes
           , ordAndMeta globCommands
           , ordAndMeta globFeature
           , ordAndMeta globExtensions
           ]
    `mergeAsc` map ordAndMeta globComments
    `mergeAsc` map ordAndMeta globEnums
  foldr f i = foldr f i . toList
  foldMap f = foldMap f . toList


mergeAsc :: [Arg Int a] -> [Arg Int a] -> [Arg Int a]
mergeAsc [] xs = xs
mergeAsc xs [] = xs
mergeAsc (x@(Arg i _):xs) (y@(Arg j _):ys)
  | i <= j    = x : mergeAsc xs (y:ys)
  | otherwise = y : mergeAsc (x:xs) ys

data VkXmlPartial
  = VkXmlPartial
  { gpComments   :: Seq (InOrder Text ())
  , gpVendorIds  :: Seq (InOrder VendorIds ())
  , gpTags       :: Seq (InOrder VkTags ())
  , gpTypes      :: Seq (InOrder VkTypes ())
  , gpEnums      :: Seq (InOrder VkEnums ())
  , gpCommands   :: Seq (InOrder VkCommands ())
  , gpFeature    :: Seq (InOrder VkFeature ())
  , gpExtensions :: Seq (InOrder VkExtensions ())
  , gpCurLength  :: Int
  } deriving Show


fixVkXml :: VkXmlPartial
         -> VkXml ()
fixVkXml VkXmlPartial
  { gpComments   = pComments
  , gpVendorIds  = Seq.Empty Seq.:|> pVendorIds
  , gpTags       = Seq.Empty Seq.:|> pTags
  , gpTypes      = Seq.Empty Seq.:|> pTypes
  , gpEnums      = pEnums
  , gpCommands   = Seq.Empty Seq.:|> pCommands
  , gpFeature    = Seq.Empty Seq.:|> pFeature
  , gpExtensions = Seq.Empty Seq.:|> pExtensions
  , gpCurLength  = curLength
  } = VkXml
  { globComments   = toList pComments
  , globVendorIds  = pVendorIds
  , globTags       = pTags
  , globTypes      = pTypes
  , globEnums      = toList pEnums
  , globCommands   = pCommands
  , globFeature    = pFeature
  , globExtensions = pExtensions
  , globLength     = curLength
  }
fixVkXml _ = error "Unexpected number of sections in vk.xml"
