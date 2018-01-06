{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE LambdaCase            #-}
module ProcessVkXml
  ( processVkXmlFile
  , generateVkSource
  , VkXml (..), InOrder (..)
  ) where

import           Control.Monad                (unless)
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.State.Class
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.Conduit.Binary          (sourceFile)
import           Data.Semigroup
import           Data.XML.Types
import           Data.Text (Text)
import           Data.Foldable (toList)
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
-- import           Data.IntMap.Strict (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
import           Debug.Trace
import           Path
import           Path.IO
import           Text.XML                     as Xml
import           Text.XML.Stream.Parse        as Xml

import           VkXml.Parser
import           VkXml.Sections.Commands
import           VkXml.Sections.Enums
import           VkXml.Sections.Types
import           VkXml.Sections.Feature
import           VkXml.Sections.VendorIds
import           VkXml.Sections.Tags
import           VkXml.Sections.Extensions

processVkXmlFile ::
       Path a File -- ^ path to vk.xml
    -> Path b Dir  -- ^ output directory for saving generated sources
    -> (forall m . (MonadReader ParseLoc m, MonadThrow m)
                 => Path b Dir -> Sink Event m r
       ) -- ^ Process parsed xml events somehow
    -> IO r
processVkXmlFile vkXmlFile outputDir pipe = do
    doesFileExist vkXmlFile >>= flip unless
      (error $ "vk.xml file located at " <> show vkXmlFile <> " is not found!")
    createDirIfMissing True outputDir

    putStrLn $ "Parsing file " <> show vkXmlFile <> ";"
    putStrLn $ "Output folder is " <> show outputDir <> ";"
    r <- runResourceT
       $  sourceFile (toFilePath vkXmlFile)
      =$= Xml.parseBytesPos Xml.def
       $$ parseWithLoc initLoc (pipe outputDir)
    putStrLn "Done!"
    return r
  where
    initLoc = defParseLoc vkXmlFile

generateVkSource ::
       VkXmlParser m
    => Path b Dir
    -> Sink Event m ()
generateVkSource _outputDir = do
    xmlP <- execStateC (VkXmlPartial mempty mempty mempty mempty
                                     mempty mempty mempty mempty 0)
      $ tagIgnoreAttrs "registry" parseAll
    traceShowM $ fixVkXml xmlP
  where
    parseAll = do
      mr <- choose
        [ tagIgnoreAttrs "comment" $ do
            com <- content
            modify' $ \v -> v
              { gpComments = gpComments v |> InOrder (gpCurLength v) com
              , gpCurLength = gpCurLength v + 1
              }
        , parseVendorIds >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpVendorIds = gpVendorIds v |> InOrder (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseTags >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpTags = gpTags v |> InOrder (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseTypes >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpTypes = gpTypes v |> InOrder (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseEnums >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpEnums = gpEnums v |> InOrder (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseCommands >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpCommands = gpCommands v |> InOrder (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseFeature >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpFeature = gpFeature v |> InOrder (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        , parseExtensions >>= \case
            Nothing -> pure Nothing
            Just x  -> fmap (const $ Just ()) . modify' $ \v -> v
                { gpExtensions = gpExtensions v |> InOrder (gpCurLength v) x
                , gpCurLength = gpCurLength v + 1
                }
        ]
      case mr of
        Nothing -> return ()
        Just () -> parseAll



data InOrder a = InOrder
  { getOrder :: Int
  , unInorder :: a
  } deriving (Eq, Show)


data VkXml
  = VkXml
  { globComments  :: [InOrder Text]
  , globVendorIds  :: InOrder VendorIds
  , globTags       :: InOrder VkTags
  , globTypes      :: InOrder VkTypes
  , globEnums      :: [InOrder VkEnums]
  , globCommands   :: InOrder VkCommands
  , globFeature    :: InOrder VkFeature
  , globExtensions :: InOrder VkExtensions
  } deriving Show


data VkXmlPartial
  = VkXmlPartial
  { gpComments   :: Seq (InOrder Text)
  , gpVendorIds  :: Seq (InOrder VendorIds)
  , gpTags       :: Seq (InOrder VkTags)
  , gpTypes      :: Seq (InOrder VkTypes)
  , gpEnums      :: Seq (InOrder VkEnums)
  , gpCommands   :: Seq (InOrder VkCommands)
  , gpFeature    :: Seq (InOrder VkFeature)
  , gpExtensions :: Seq (InOrder VkExtensions)
  , gpCurLength  :: Int
  } deriving Show


fixVkXml :: VkXmlPartial
         -> VkXml
fixVkXml VkXmlPartial
  { gpComments   = pComments
  , gpVendorIds  = Seq.Empty Seq.:|> pVendorIds
  , gpTags       = Seq.Empty Seq.:|> pTags
  , gpTypes      = Seq.Empty Seq.:|> pTypes
  , gpEnums      = pEnums
  , gpCommands   = Seq.Empty Seq.:|> pCommands
  , gpFeature    = Seq.Empty Seq.:|> pFeature
  , gpExtensions = Seq.Empty Seq.:|> pExtensions
  } = VkXml
  { globComments   = toList pComments
  , globVendorIds  = pVendorIds
  , globTags       = pTags
  , globTypes      = pTypes
  , globEnums      = toList pEnums
  , globCommands   = pCommands
  , globFeature    = pFeature
  , globExtensions = pExtensions
  }
fixVkXml _ = error "Unexpected number of sections in vk.xml"
