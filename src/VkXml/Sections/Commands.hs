{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
-- | Vulkan types, as they defined in vk.xml
module VkXml.Sections.Commands
  ( parseCommands
  , VkCommands (..), VkCommand (..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Conduit
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser

-- import           Debug.Trace

-- * Types


data VkCommands
  = VkCommands
  { comment  :: Text
  , commands :: [VkCommand]
  } deriving Show

data VkCommand
  = VkCommand
  { attributes :: VkCommandAttrs
  } deriving Show

data VkCommandAttrs
  = VkCommandAttrs
  { successcodes   :: [VkEnumValueName]
  , errorcodes     :: [VkEnumValueName]
  , queues         :: Maybe Text
  , renderpass     :: Maybe Text
  , cmdbufferlevel :: [Text]
  , pipeline       :: Maybe Text
  , comment        :: Maybe Text
  } deriving Show


-- * Parsing


-- | Try to parse current tag as being types,
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "types", throw an exception
parseCommands :: VkXmlParser m
              => Sink Event m (Maybe VkCommands)
parseCommands = parseTagForceAttrs "commands" (lift $ attr "comment")
  $ \secComment -> do
    coms <- many parseVkCommand
    return $ VkCommands (fromMaybe mempty secComment) coms

parseVkCommand :: VkXmlParser m
               => Sink Event m (Maybe VkCommand)
parseVkCommand =
  parseTagForceAttrs "command" parseVkCommandAttrs $ \attrs -> do
    awaitForever $ const (return ())
    return $ VkCommand attrs


parseVkCommandAttrs :: ReaderT ParseLoc AttrParser VkCommandAttrs
parseVkCommandAttrs = lift $ do
    successcodes     <- map VkEnumValueName . f <$> attr "successcodes"
    errorcodes       <- map VkEnumValueName . f <$> attr "errorcodes"
    queues           <- attr "queues"
    renderpass       <- attr "renderpass"
    cmdbufferlevel   <- f <$> attr "cmdbufferlevel"
    pipeline         <- attr "pipeline"
    comment          <- attr "comment"
    return VkCommandAttrs {..}
  where
    f = maybe [] (T.split (',' ==))
