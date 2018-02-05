{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Vulkan enums, as they defined in vk.xml
module VkXml.Sections.Enums
  ( parseVkEnum, parseVkEnums
  , VkEnum (..)
  , vkEnumName, vkEnumTName, vkEnumComment
  , vkEnumValue
  , VkEnums (..)
  , vkEnumsTypeName, vkEnumsComment, vkEnumsMembers, vkEnumsIsBits
  ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.Reader   (ReaderT (..))
import           Data.Bits
import           Data.Conduit
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Read               as T
import           Data.Word
import           Data.XML.Types
import           Language.Haskell.Exts.Pretty
import           Text.RE.TDFA.Text
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
  , _vkEnumValue   :: VkEnumValue
  } deriving (Eq,Ord,Show)

data VkEnumValue
  = VkEnumString Text
    -- ^ String literal
  | VkEnumIntegral Integer Text
    -- ^ Value and type, i.e. "Num a => a" or "Word32" or "VkResult"
  | VkEnumFractional Rational
    -- ^ Value; type is assumed to be "Fractional a => a"
  | VkEnumReference
    -- ^ Enum with no value, assuming there is a value somewhere else.
  deriving (Eq,Ord,Show)

data VkEnums
  = VkEnums
    { _vkEnumsTypeName :: Maybe VkTypeName
    , _vkEnumsComment  :: Text
    , _vkEnumsIsBits   :: Bool
    , _vkEnumsMembers  :: Sections VkEnum
    } deriving Show

makeLenses ''VkEnum
makeLenses ''VkEnums

-- * Parsing

-- | Try to parse current tag as being enums,
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "enums", throw an exception
--
--   Note: enum "VkResult" also has a member "unused",
--    I ignore this member, because it does not seem to be useful for codegen.
parseVkEnums :: VkXmlParser m
           => Sink Event m (Maybe VkEnums)
parseVkEnums = parseTagForceAttrs "enums" parseVkEnumsAttrs $
  \case
    VkEnums n c ib _
      -> fmap (VkEnums n c ib)
      . parseSectionsL $ many_ (ignoreEmptyTag "unused") >> parseVkEnum 0 n


parseVkEnumsAttrs :: ReaderT ParseLoc AttrParser VkEnums
parseVkEnumsAttrs = do
  n <- VkTypeName <$> forceAttr "name"
  c <- lift $ fromMaybe mempty <$> attr "comment"
  met <- lift $ attr "type"
  let _vkEnumsComment = c
          <:> maybe mempty (\s -> "type = @" <> s <> "@") met
      _vkEnumsMembers = Sections [] []
      _vkEnumsTypeName =
         if n == VkTypeName "API Constants"
         then Nothing else Just n
      _vkEnumsIsBits = met == Just "bitmask"

  pure VkEnums {..}





-- | Try to read @enum@ tag.
--   This tag requires some context to generate a good enum:
--    whether this is a part of extension or a part of enum type definition.
parseVkEnum :: VkXmlParser m
            => Int
               -- ^ extension number, 0 otherwise
            -> Maybe VkTypeName
               -- ^ Name of the type this value belongs to.
            -> Sink Event m [VkEnum]
parseVkEnum extNumber mTypeName
    = (^.._Just.traverse) <$> parseTagForceAttrs "enum" parseAttrs pure
  where
    parseAttrs :: ReaderT ParseLoc AttrParser [VkEnum]
    parseAttrs = do
      _vkEnumName  <- VkEnumName <$> forceAttr "name"
      unless (isValid _vkEnumName)
        $ parseFailed
        $ "parseVkEnum: invalid enum name: "
          <> T.unpack (unVkEnumName _vkEnumName)
          <> ". The enum name should be a valid haskell pattern synonym."

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
                       <:> maybe mempty (\s -> "type = @" <> s <> "@") mtype

      _vkEnumValue <-
        case _vkEnumTName of

          -- if vk type name is given, we are confident the type is Int32
          Just tname -> makeNumeric
              (T.signed decOrHex <$> mvalue)
              (decOrHex <$> mbitpos)
              (decOrHex <$> moffset)
              negDir
              (T.pack . prettyPrint $ toType 0 (toHaskellName tname))

          -- if there is no type name, we are dealing with a constant,
          -- that can be WordXX, IntXX, Num a, Fractional a, or literal string
          Nothing -> case mvalue of
            -- this is a refernce to another existing constant
            Nothing -> pure VkEnumReference

            Just val

                -- this is a string literal
              | Just s <- matchedText $ val ?=~ [reMultilineSensitive|"([0-9A-Za-z_]+)"|]
                -> pure $ VkEnumString s

                -- this is a fractional number
              | Right (v, "f") <- T.rational val
                -> pure $ VkEnumFractional v

                -- arbitrary integral
              | Right (v, "") <- T.signed T.decimal val
                -> pure $ VkEnumIntegral v "(Num a, Eq a) => a"

                -- special unparsed cases
              | "(~0U)" <- val
                -> pure $ VkEnumIntegral (toInteger (complement 0 :: Word32)) "Word32"
              | "(~0ULL)" <- val
                -> pure $ VkEnumIntegral (toInteger (complement 0 :: Word64)) "Word64"
              | "(~0U-1)" <- val
                -> pure $ VkEnumIntegral (toInteger (complement 0 - 1 :: Word32)) "Word32"
              | "(~0U-2)" <- val
                -> pure $ VkEnumIntegral (toInteger (complement 0 - 2 :: Word32)) "Word32"

              | otherwise
                -> parseFailed
                $ "parseVkEnum: value " <> T.unpack val
                <> " does not match any known enum kind."

      let rEnum = VkEnum{..}

      aliasEnumL <- case malias of
        Nothing -> pure []
        Just alias -> do
          unless (isValid $ VkEnumName alias)
            $ parseFailed
            $ "parseVkEnum: invalid enum name: "
              <> T.unpack alias
              <> ". The enum name should be a valid haskell pattern synonym."
          return [rEnum & vkEnumName .~ VkEnumName alias]

      return $ rEnum : aliasEnumL

    fromBitpos = shiftL 1
    fromValue = id
    -- Extending enums in extensions:
    --  describes use of "offset" attribute
    -- https://www.khronos.org/registry/vulkan/specs/1.0/styleguide.html#_assigning_extension_token_values
    --
    -- enum_offset(extension_number, offset) = base_value + (extension_number - 1) × range_size + offset
    -- where
    --   base_value = 1000000000
    --   range_size = 1000
    --
    -- positve enum  = enum_offset(extension_number, offset)
    -- negative enum = - enum_offset(extension_number, offset)
    fromOffset isNegDir offset
      | base_value <- 1000000000
      , range_size <- 1000
      , vAbs <- base_value + range_size * (fromIntegral extNumber - 1) + offset
      , v <- if isNegDir then negate vAbs else vAbs
      = fromValue v

    makeNumeric (Just (Right (value,_))) _mbitpos _moffset _ndir ttype
      = pure $ VkEnumIntegral (fromValue value) ttype
    makeNumeric _mvalue (Just (Right (bitpos,_))) _moffset _ndir ttype
      = pure $ VkEnumIntegral (fromBitpos bitpos) ttype
    makeNumeric _mvalue _mbitpos (Just (Right (offset,_))) ndir ttype
      = pure $ VkEnumIntegral (fromOffset ndir offset) ttype
    makeNumeric (Just (Left err)) _ _ _ _ = parseFailed
              $ "Could not parse enum value: " <> err
    makeNumeric _ (Just (Left err)) _ _ _ = parseFailed
              $ "Could not parse enum bitpos: " <> err
    makeNumeric _ _ (Just (Left err)) _ _ = parseFailed
              $ "Could not parse enum offset: " <> err
    makeNumeric Nothing Nothing Nothing _ _ = pure VkEnumReference



--
-- data VkEnums
--   = VkEnums
--     { name        :: VkTypeName
--     , comment     :: Maybe Text
--     , memberEnums :: Sections VkEnumValue
--     }
--   | VkBitmasks
--     { name        :: VkTypeName
--     , comment     :: Maybe Text
--     , memberMasks :: Sections VkBitmaskValue
--     }
--   | VkConstants
--     { name         :: VkTypeName
--     , comment      :: Maybe Text
--     , memberConsts :: Sections VkConstant
--     }
--   deriving Show
--
-- data VkEnumValue
--   = VkEnumValue
--     { name    :: VkEnumValueName
--     , comment :: Maybe Text
--     , value   :: Int
--     }
--   deriving Show
--
-- data VkBitmaskValue
--   = VkBitmaskBitpos
--     { name    :: VkEnumValueName
--     , comment :: Maybe Text
--     , bitpos  :: Word
--     }
--   | VkBitmaskValue
--     { name    :: VkEnumValueName
--     , comment :: Maybe Text
--     , value   :: Int
--     }
--   deriving Show
--
-- data VkConstant
--   = VkConstant
--     { name    :: VkEnumValueName
--     , comment :: Maybe Text
--     , value   :: Text
--     }
--   deriving Show
--



-- -- * Parsing
--
-- -- | Try to parse current tag as being enums,
-- --
-- --   * If tag name does not match, return events upstream as leftovers
-- --   * If failed to parse tag "enums", throw an exception
-- --
-- --   Note: enum "VkResult" also has a member "unused",
-- --    I ignore this member, because it does not seem to be useful for codegen.
-- parseEnums :: VkXmlParser m
--            => Sink Event m (Maybe VkEnums)
-- parseEnums = parseTagForceAttrs "enums" parseVkEnumsAttrs $
--   \case
--     VkEnums n c _
--       -> fmap (VkEnums n c)
--       . parseSections $ ignoreEmptyTag "unused"
--                       >> parseTagForceAttrs "enum" parseVkEnumValueAttrs pure
--
--     VkBitmasks n c _
--       -> fmap (VkBitmasks n c)
--       . parseSections $ ignoreEmptyTag "unused"
--                       >> parseTagForceAttrs "enum" parseVkBitmaskValueAttrs pure
--
--     VkConstants n c _
--       -> fmap (VkConstants n c)
--       . parseSections $ ignoreEmptyTag "unused"
--                       >> parseTagForceAttrs "enum" parseVkConstantAttrs pure
--



-- parseVkEnumsAttrs :: ReaderT ParseLoc AttrParser VkEnums
-- parseVkEnumsAttrs = do
--   n <- VkTypeName <$> forceAttr "name"
--   mc <- lift $ attr "comment"
--   met <- lift $ attr "type"
--   case met of
--     Nothing        -> pure . VkConstants n mc $ Sections [] []
--     Just "enum"    -> pure . VkEnums n mc $ Sections [] []
--     Just "bitmask" -> pure . VkBitmasks n mc $ Sections [] []
--     Just tt        -> parseFailed
--                     $ "uknown enums type " <> show tt
--
-- parseVkEnumValueAttrs :: ReaderT ParseLoc AttrParser VkEnumValue
-- parseVkEnumValueAttrs = do
--   n <- VkEnumValueName <$> forceAttr "name"
--   mc <- lift $ attr "comment"
--   ev <- T.signed decOrHex <$> forceAttr "value"
--   case ev of
--     Left err -> parseFailed
--               $ "could not parse enum value: " <> err
--     Right (i, _) -> pure $ VkEnumValue n mc i
--
-- parseVkBitmaskValueAttrs :: ReaderT ParseLoc AttrParser VkBitmaskValue
-- parseVkBitmaskValueAttrs = do
--   n <- VkEnumValueName <$> forceAttr "name"
--   mc <- lift $ attr "comment"
--   mbpv <- fmap decOrHex <$> lift (attr "bitpos")
--   mvav <- fmap (T.signed decOrHex) <$> lift (attr "value")
--   case (mbpv, mvav) of
--     (Nothing, Nothing) -> parseFailed
--                           "missing bitpos or value attribute for a bitmask."
--     (Just (Right (i, _)), _) -> pure $ VkBitmaskBitpos n mc i
--     (_, Just (Right (i, _))) -> pure $ VkBitmaskValue n mc i
--     (Just (Left err), _) -> parseFailed
--                           $ "could not parse bitmask bitpos: " <> err
--     (_, Just (Left err)) -> parseFailed
--                           $ "could not parse bitmask value: " <> err
--
--
-- parseVkConstantAttrs :: ReaderT ParseLoc AttrParser VkConstant
-- parseVkConstantAttrs = do
--   n <- VkEnumValueName <$> forceAttr "name"
--   mc <- lift $ attr "comment"
--   v <- forceAttr "value"
--   pure $ VkConstant n mc v
