{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
-- | Vulkan commands, as they defined in vk.xml
module VkXml.Sections.Commands
  ( parseCommands
  , VkCommands
  , VkCommand (..), VkCommandAttrs (..)
  , VkCommandParam (..), VkCommandParamAttrs (..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Conduit
import           Data.List                  (nub)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser

import           Debug.Trace

-- * Types


type VkCommands = Map VkCommandName VkCommand


data VkCommand
  = VkCommand
    { cReturnType :: VkTypeName
      -- ^ Always, either () or VkResult; so no pointers expected.
    , cReturnTypeOrig :: Text
      -- ^ Keep original return type here for the documentation
    , cName       :: VkCommandName
      -- ^ Maybe a little bit adjusted to fit haskell var logic
    , cNameOrig   :: Text
      -- ^ Original name appeared as-is, used for foreign calls
    , cAttributes :: VkCommandAttrs
    , cParameters :: [VkCommandParam]
    }
  | VkCommandAlias
    { cName     :: VkCommandName
    , cAlias    :: VkCommandName
    , cNameOrig :: Text
    } deriving Show


-- |
-- <https://www.khronos.org/registry/vulkan/specs/1.0/registry.html#_attributes_of_code_command_code_tags vulkan registry 15.1>
data VkCommandAttrs
  = VkCommandAttrs
  { queues         :: [Text]
  , successcodes   :: [VkEnumName]
  , errorcodes     :: [VkEnumName]
  , renderpass     :: Maybe Text
  , cmdbufferlevel :: [Text]
  , pipeline       :: Maybe Text
  , cComment       :: Maybe Text
  } deriving Show

-- |
--   TODO: this is not fully compliant with the
-- <https://www.khronos.org/registry/vulkan/specs/1.0/registry.html#tag-command:param spec 15.4>
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


instance TypeScope VkCommand where
  providesTypes _ = []
  requiresTypes VkCommand {..} =
    nub $ cReturnType : (cParameters >>= requiresTypes)
  -- this can be a problem -- we need to pull the types from alieased command
  requiresTypes VkCommandAlias {} = []

instance TypeScope VkCommandParam where
  providesTypes _ = []
  requiresTypes VkCommandParam {..} = [paramType]


-- * Parsing


-- | Try to parse current tag as being commands,
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "commands", throw an exception
parseCommands :: VkXmlParser m => Sink Event m (Maybe VkCommands)
parseCommands = parseTagForceAttrs "commands" (lift $ attr "comment")
  $ \_ ->
      Map.fromList . fmap (\c -> (cName c, c)) <$> many parseVkCommand


parseVkCommand :: VkXmlParser m => Sink Event m (Maybe VkCommand)
parseVkCommand =
  parseTagForceAttrs "command" parseVkCommandAttrs $ \case
    Right cAttributes -> do
      -- first element of command is always a "proto" tag
      --  the rest are "param" tags
      mtn <- parseTagForceAttrs "proto" (pure ()) $ \() -> do
        mcReturnTypeOrig <- tagIgnoreAttrs "type" content
        mt <-  mapM toHaskellType mcReturnTypeOrig
        mon <- tagIgnoreAttrs "name" content
        mn <- mapM toHaskellComm mon
        pure $ (,,,) <$> mcReturnTypeOrig <*> mt <*> mn <*> mon
      case join mtn of
        Nothing -> parseFailed "Could not parse type/name from command.proto"
        Just (cReturnTypeOrig, cReturnType, cName, cNameOrig) -> do
          cParameters <- many $ do
             mi <- ignoreTreeContent "implicitexternsyncparams"
             case mi of
               Nothing -> return ()
               Just () ->
                 traceM "Warning: ignoring <implicitexternsyncparams> tag."
             parseVkCommandParam
          return VkCommand {..}
    Left c -> pure c

parseVkCommandAttrs :: ReaderT ParseLoc AttrParser (Either VkCommand VkCommandAttrs)
parseVkCommandAttrs = do
  malias <- lift (attr "alias") >>= mapM toHaskellComm
  case malias of
    Nothing -> do
      successcodes     <- commaSeparated <$> lift (attr "successcodes")
                          >>= mapM toHaskellPat
      errorcodes       <- commaSeparated <$> lift (attr "errorcodes")
                          >>= mapM toHaskellPat
      queues           <- commaSeparated <$> lift (attr "queues")
      renderpass       <- lift $ attr "renderpass"
      cmdbufferlevel   <- commaSeparated <$> lift (attr "cmdbufferlevel")
      pipeline         <- lift $ attr "pipeline"
      cComment         <- lift $ attr "comment"
      return $ Right VkCommandAttrs {..}
    Just cAlias -> do
      cNameOrig <- forceAttr "name"
      cName <- toHaskellComm cNameOrig
      return $ Left VkCommandAlias {..}

-- TODO: use https://hackage.haskell.org/package/language-c
--  to parse the param tag more robustly.
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
    paramType <- toHaskellType paramTypeTxt
    let paramIsConst    = T.strip constTxt == "const"
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
