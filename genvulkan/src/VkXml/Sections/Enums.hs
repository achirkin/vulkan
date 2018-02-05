{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Vulkan enums, as they defined in vk.xml
module VkXml.Sections.Enums
  ( parseEnums
  , VkEnums (..)
  , VkEnumValue (..)
  , VkBitmaskValue (..)
  , VkConstant (..)
  ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Reader   (ReaderT (..))
import           Data.Conduit
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                    (Text)
import qualified Data.Text.Read               as T
import           Data.XML.Types
import           Language.Haskell.Exts.Syntax
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser


-- * Enums in vk.xml

{- |
In the registry, enum is defined in several places:

<https://www.khronos.org/registry/vulkan/specs/1.0/registry.html#_enum_tags 19.2.3. Enum tags>

<https://www.khronos.org/registry/vulkan/specs/1.0/registry.html#tag-enum 12. Enumerants (enum tag)>

In addition, extensions have to define enums in a special way;
their values are calculated as follows in the link:
<https://www.khronos.org/registry/vulkan/specs/1.0/styleguide.html#_assigning_extension_token_values 3.10. Assigning Extension Token Values>

With all of the above in mind, I try to unify content of enum tags in a single haskell type.
In the generated library, all enum values are represented as pattern synonyms.
Thus, I need to keep the following information to be able to generate them:

  * Value type: a particular vulkan type, or general thing like Num or Fractional.
  * Value (depends on the type)
  * Comment: all other related info.

Depending on a context, I need to infer type and value.
-}

data VkEnum
  = VkEnum
  { _vkEnumName    :: VkEnumName
  , _vkEnumTName   :: Maybe VkTypeName
  , _vkEnumComment :: Text
  , _vkEnumType    :: Type ()
  , _vkEnumValue   :: Exp ()
  }


makeLenses ''VkEnum


-- | Try to read @enum@ tag.
--   This tag requires some context to generate a good enum:
--    whether this is a part of extension or a part of enum type definition.
parseVkEnum :: VkXmlParser m
            => Int
               -- ^ extension number, 0 otherwise
            -> Maybe VkTypeName
               -- ^ Name of the type this value belongs to.
            -> Sink Event m (Maybe VkEnum)
parseVkEnum extNumber mTypeName
    = ignoreEmptyTag "unused"
    >> parseTagForceAttrs "enum" parseAttrs pure
  where
    parseAttrs :: ReaderT ParseLoc AttrParser VkEnum
    parseAttrs = do
      _vkEnumName  <- VkEnumName <$> forceAttr "name"
      mvalue       <- lift $ attr "value"
      mbitpos      <- lift $ attr "bitpos"
      mapi         <- lift $ attr "api"
      mtype        <- lift $ attr "type"
      malias       <- lift $ attr "alias"
      comment      <- lift $ fromMaybe mempty <$> attr "comment"
      moffset      <- lift $ attr "offset"
      negDir       <- lift $ (Just "-" ==) <$> attr "dir"
      _vkEnumTName <- fmap (<|> mTypeName)
                    . lift $ fmap VkTypeName <$> attr "extends"
      let _vkEnumComment = comment
                       <:> maybe mempty (\s -> "bitpos = @" <> s <> "@") mbitpos
                       <:> maybe mempty (\s -> "api = @" <> s <> "@") mapi

      (_vkEnumType, _vkEnumValue) <-
        case _vkEnumTName of
          -- if vk type name is given, we are confident the type is Int32
          Just tname -> pure (undefined, undefined) -- TODO WIP
      return VkEnum{..}

-- * Types


data VkEnums
  = VkEnums
    { name        :: VkTypeName
    , comment     :: Maybe Text
    , memberEnums :: Sections VkEnumValue
    }
  | VkBitmasks
    { name        :: VkTypeName
    , comment     :: Maybe Text
    , memberMasks :: Sections VkBitmaskValue
    }
  | VkConstants
    { name         :: VkTypeName
    , comment      :: Maybe Text
    , memberConsts :: Sections VkConstant
    }
  deriving Show

data VkEnumValue
  = VkEnumValue
    { name    :: VkEnumValueName
    , comment :: Maybe Text
    , value   :: Int
    }
  deriving Show

data VkBitmaskValue
  = VkBitmaskBitpos
    { name    :: VkEnumValueName
    , comment :: Maybe Text
    , bitpos  :: Word
    }
  | VkBitmaskValue
    { name    :: VkEnumValueName
    , comment :: Maybe Text
    , value   :: Int
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
--
--   Note: enum "VkResult" also has a member "unused",
--    I ignore this member, because it does not seem to be useful for codegen.
parseEnums :: VkXmlParser m
           => Sink Event m (Maybe VkEnums)
parseEnums = parseTagForceAttrs "enums" parseVkEnumsAttrs $
  \case
    VkEnums n c _
      -> fmap (VkEnums n c)
      . parseSections $ ignoreEmptyTag "unused"
                      >> parseTagForceAttrs "enum" parseVkEnumValueAttrs pure

    VkBitmasks n c _
      -> fmap (VkBitmasks n c)
      . parseSections $ ignoreEmptyTag "unused"
                      >> parseTagForceAttrs "enum" parseVkBitmaskValueAttrs pure

    VkConstants n c _
      -> fmap (VkConstants n c)
      . parseSections $ ignoreEmptyTag "unused"
                      >> parseTagForceAttrs "enum" parseVkConstantAttrs pure




parseVkEnumsAttrs :: ReaderT ParseLoc AttrParser VkEnums
parseVkEnumsAttrs = do
  n <- VkTypeName <$> forceAttr "name"
  mc <- lift $ attr "comment"
  met <- lift $ attr "type"
  case met of
    Nothing        -> pure . VkConstants n mc $ Sections [] []
    Just "enum"    -> pure . VkEnums n mc $ Sections [] []
    Just "bitmask" -> pure . VkBitmasks n mc $ Sections [] []
    Just tt        -> parseFailed
                    $ "uknown enums type " <> show tt

parseVkEnumValueAttrs :: ReaderT ParseLoc AttrParser VkEnumValue
parseVkEnumValueAttrs = do
  n <- VkEnumValueName <$> forceAttr "name"
  mc <- lift $ attr "comment"
  ev <- T.signed decOrHex <$> forceAttr "value"
  case ev of
    Left err -> parseFailed
              $ "could not parse enum value: " <> err
    Right (i, _) -> pure $ VkEnumValue n mc i

parseVkBitmaskValueAttrs :: ReaderT ParseLoc AttrParser VkBitmaskValue
parseVkBitmaskValueAttrs = do
  n <- VkEnumValueName <$> forceAttr "name"
  mc <- lift $ attr "comment"
  mbpv <- fmap decOrHex <$> lift (attr "bitpos")
  mvav <- fmap (T.signed decOrHex) <$> lift (attr "value")
  case (mbpv, mvav) of
    (Nothing, Nothing) -> parseFailed
                          "missing bitpos or value attribute for a bitmask."
    (Just (Right (i, _)), _) -> pure $ VkBitmaskBitpos n mc i
    (_, Just (Right (i, _))) -> pure $ VkBitmaskValue n mc i
    (Just (Left err), _) -> parseFailed
                          $ "could not parse bitmask bitpos: " <> err
    (_, Just (Left err)) -> parseFailed
                          $ "could not parse bitmask value: " <> err


parseVkConstantAttrs :: ReaderT ParseLoc AttrParser VkConstant
parseVkConstantAttrs = do
  n <- VkEnumValueName <$> forceAttr "name"
  mc <- lift $ attr "comment"
  v <- forceAttr "value"
  pure $ VkConstant n mc v
