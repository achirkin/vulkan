{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
module VkXml.Sections.Enums
  ( parseEnums
  , VkEnums (..)
  , VkEnumValue (..)
  , VkBitmaskValue (..)
  , VkConstant (..)
  ) where


import           Control.Monad.Except
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Conduit
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text.Read             as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser


-- * Types


data VkEnums
  = VkEnums
    { name        :: VkTypeName
    , comment     :: Maybe Text
    , memberEnums :: [VkEnumValue]
    }
  | VkBitmasks
    { name        :: VkTypeName
    , comment     :: Maybe Text
    , memberMasks :: [VkBitmaskValue]
    }
  | VkConstants
    { name         :: VkTypeName
    , comment      :: Maybe Text
    , memberConsts :: [VkConstant]
    }
  deriving Show

data VkEnumValue
  = VkEnumValue
    { name    :: VkEnumValueName
    , comment :: Maybe Text
    , value   :: Word
    }
  deriving Show

data VkBitmaskValue
  = VkBitmaskValue
    { name    :: VkEnumValueName
    , comment :: Maybe Text
    , bitpos  :: Word
    }
  deriving Show

data VkConstant
  = VkConstant
    { name    :: VkEnumValueName
    , comment :: Maybe Text
    , value   :: Text
    }
  deriving Show




-- * Parsing

-- | Try to parse current tag as being enums,
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "enums", throw an exception
parseEnums :: VkXmlParser m
           => Sink Event m (Maybe VkEnums)
parseEnums = parseTagForceAttrs "enums" parseVkEnumsAttrs $
  \case
    VkEnums n c _
      -> fmap (VkEnums n c)
      . many $ parseTagForceAttrs "enum" parseVkEnumValueAttrs pure

    VkBitmasks n c _
      -> fmap (VkBitmasks n c)
      . many $ parseTagForceAttrs "enum" parseVkBitmaskValueAttrs pure

    VkConstants n c _
      -> fmap (VkConstants n c)
      . many $ parseTagForceAttrs "enum" parseVkConstantAttrs pure





parseVkEnumsAttrs :: ReaderT ParseLoc AttrParser VkEnums
parseVkEnumsAttrs = do
  n <- VkTypeName <$> forceAttr "name"
  mc <- lift $ attr "comment"
  met <- lift $ attr "type"
  case met of
    Nothing        -> pure $ VkConstants n mc []
    Just "enum"    -> pure $ VkEnums n mc []
    Just "bitmask" -> pure $ VkEnums n mc []
    Just tt        -> parseFailed
                    $ "uknown enums type " <> show tt

parseVkEnumValueAttrs :: ReaderT ParseLoc AttrParser VkEnumValue
parseVkEnumValueAttrs = do
  n <- VkEnumValueName <$> forceAttr "name"
  mc <- lift $ attr "comment"
  ev <- T.decimal <$> forceAttr "value"
  case ev of
    Left err -> parseFailed
              $ "could not parse enum value: " <> err
    Right (i, _) -> pure $ VkEnumValue n mc i

parseVkBitmaskValueAttrs :: ReaderT ParseLoc AttrParser VkBitmaskValue
parseVkBitmaskValueAttrs = do
  n <- VkEnumValueName <$> forceAttr "name"
  mc <- lift $ attr "comment"
  ev <- T.decimal <$> forceAttr "bitpos"
  case ev of
    Left err -> parseFailed
              $ "could not parse bitmask bitpos: " <> err
    Right (i, _) -> pure $ VkBitmaskValue n mc i


parseVkConstantAttrs :: ReaderT ParseLoc AttrParser VkConstant
parseVkConstantAttrs = do
  n <- VkEnumValueName <$> forceAttr "name"
  mc <- lift $ attr "comment"
  v <- forceAttr "value"
  pure $ VkConstant n mc v
