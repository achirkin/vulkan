{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
-- | Vulkan commands, as they defined in vk.xml
module VkXml.Sections.Commands
  ( parseCommands
  , VkCommands (..)
  , VkCommand (..), VkCommandAttrs (..)
  , VkCommandParam (..), VkCommandParamAttrs (..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Conduit
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser

import           Debug.Trace

-- * Types


data VkCommands
  = VkCommands
  { comment  :: Text
  , commands :: [VkCommand]
  } deriving Show

data VkCommand
  = VkCommand
  { returnType :: VkTypeName
  , name       :: VkCommandName
  , attributes :: VkCommandAttrs
  , parameters :: [VkCommandParam]
  } deriving Show

data VkCommandAttrs
  = VkCommandAttrs
  { successcodes   :: [VkEnumName]
  , errorcodes     :: [VkEnumName]
  , queues         :: Maybe Text
  , renderpass     :: Maybe Text
  , cmdbufferlevel :: [Text]
  , pipeline       :: Maybe Text
  , comment        :: Maybe Text
  } deriving Show

data VkCommandParam
  = VkCommandParam
  { attributes      :: VkCommandParamAttrs
  , paramType       :: VkTypeName
  , paramTypeRefLvl :: Word
    -- ^ how many stars are there
  , paramIsConst    :: Bool
  , paramArraySize  :: Maybe Word
    -- ^ size is given in brackets sometimes
  , paramName       :: Text
  , code            :: Text
  } deriving Show

data VkCommandParamAttrs
  = VkCommandParamAttrs
  { optional       :: Bool
  , externsync     :: Bool
  , noautovalidity :: Bool
  , len            :: Maybe Text
  } deriving Show


-- * Parsing


-- | Try to parse current tag as being commands,
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "commands", throw an exception
parseCommands :: VkXmlParser m => Sink Event m (Maybe VkCommands)
parseCommands = parseTagForceAttrs "commands" (lift $ attr "comment")
  $ \secComment -> do
    coms <- many parseVkCommand
    return $ VkCommands (fromMaybe mempty secComment) coms


parseVkCommand :: VkXmlParser m => Sink Event m (Maybe VkCommand)
parseVkCommand =
  parseTagForceAttrs "command" parseVkCommandAttrs $ \attributes -> do
    -- first element of command is always a "proto" tag
    --  the rest are "param" tags
    mtn <- parseTagForceAttrs "proto" (pure ()) $ \() -> do
      mt <- tagIgnoreAttrs "type" $ VkTypeName <$> content
      mn <- tagIgnoreAttrs "name" $ VkCommandName <$> content
      pure $ (,) <$> mt <*> mn
    case join mtn of
      Nothing -> parseFailed "Could not parse type/name from command.proto"
      Just (returnType, name) -> do
        parameters <- many $ do
           mi <- ignoreTreeContent "implicitexternsyncparams"
           case mi of
             Nothing -> return ()
             Just () ->
               traceM "Warning: ignoring <implicitexternsyncparams> tag."
           parseVkCommandParam
        return VkCommand {..}



parseVkCommandAttrs :: ReaderT ParseLoc AttrParser VkCommandAttrs
parseVkCommandAttrs = lift $ do
    successcodes     <- map VkEnumName . f <$> attr "successcodes"
    errorcodes       <- map VkEnumName . f <$> attr "errorcodes"
    queues           <- attr "queues"
    renderpass       <- attr "renderpass"
    cmdbufferlevel   <- f <$> attr "cmdbufferlevel"
    pipeline         <- attr "pipeline"
    comment          <- attr "comment"
    return VkCommandAttrs {..}
  where
    f = maybe [] (T.split (',' ==))

parseVkCommandParam :: VkXmlParser m => Sink Event m (Maybe VkCommandParam)
parseVkCommandParam =
  parseTagForceAttrs "param" parseVkCommandParamAttrs $ \attributes -> do
    constTxt <- content
    paramTypeTxt <- tagIgnoreAttrs "type" content >>= \case
      Nothing -> parseFailed "missing 'type' tag in command param"
      Just t  -> pure t
    paramTypeRefLvlTxt <- content
    paramName <- tagIgnoreAttrs "name" content >>= \case
      Nothing -> parseFailed "missing 'name' tag in command param"
      Just t  -> pure t
    paramArraySizeTxt <- content
    let paramIsConst    = T.strip constTxt == "const"
        paramType       = VkTypeName paramTypeTxt
        paramTypeRefLvl = fromIntegral $ T.count "*" paramTypeRefLvlTxt
        code = constTxt <> paramTypeTxt <> paramTypeRefLvlTxt
            <> paramName <> paramArraySizeTxt
        paramArraySize = parseArraySize paramArraySizeTxt
    return VkCommandParam {..}
  where
    parseArraySize t = case decOrHex . T.drop 1 $ T.strip t of
      Left _      -> Nothing
      Right (i,_) -> Just i


parseVkCommandParamAttrs :: ReaderT ParseLoc AttrParser VkCommandParamAttrs
parseVkCommandParamAttrs = do
    optional       <- parseBoolAttr "optional"
    externsync     <- parseBoolAttr "externsync"
    noautovalidity <- parseBoolAttr "noautovalidity"
    len            <- lift $ attr "len"
    return VkCommandParamAttrs {..}


parseBoolAttr :: Name -> ReaderT ParseLoc AttrParser Bool
parseBoolAttr n = do
  mr <- lift $ attr n
  case T.toLower <$> mr of
    Just "true" -> pure True
    _           -> pure False
