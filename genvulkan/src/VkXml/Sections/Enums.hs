{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
-- | Vulkan enums, as they defined in vk.xml
module VkXml.Sections.Enums
  ( parseVkEnum, parseVkEnums
  , VkEnum (..), VkEnumValue (..)
  , vkEnumName, vkEnumTName, vkEnumComment
  , vkEnumValue
  , VkEnums (..)
  , vkEnumsTypeName, vkEnumsComment, vkEnumsMembers, vkEnumsIsBits
  ) where


import           Control.Applicative
import           Control.Lens                 hiding (re)
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
import           Text.RE.TDFA.Text
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser


-- * Enums in vk.xml

{- |
In the registry, enum is defined in several places:

<https://www.khronos.org/registry/vulkan/specs/1.1/registry.html#_enum_tags 19.2.3. Enum tags>

<https://www.khronos.org/registry/vulkan/specs/1.1/registry.html#tag-enum 12. Enumerants (enum tag)>

In addition, extensions have to define enums in a special way;
their values are calculated as follows in the link:
<https://www.khronos.org/registry/vulkan/specs/1.1/styleguide.html#_assigning_extension_token_values 3.10. Assigning Extension Token Values>

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
  | VkEnumAlias VkEnumName
    -- ^ Enum with value referring to another enum
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


instance TypeScope VkEnum where
  providesTypes _ = []
  requiresTypes e = e ^.. vkEnumTName . _Just


instance TypeScope VkEnums where
  providesTypes _ = []
  requiresTypes e = e ^.. vkEnumsTypeName . _Just

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
  \(VkEnums n c ib _)
      -> fmap (VkEnums n c ib)
      . parseSectionsL $ many_ (ignoreEmptyTag "unused") >> parseVkEnum 0 n


parseVkEnumsAttrs :: ReaderT ParseLoc AttrParser VkEnums
parseVkEnumsAttrs = do
  _vkEnumsTypeName
    <- ( \s -> if s == "API Constants"
               then Nothing else Just s
       ) <$> forceAttr "name"
       >>= mapM toHaskellType
  c <- lift $ fromMaybe mempty <$> attr "comment"
  met <- lift $ attr "type"
  let _vkEnumsComment = c
          <:> maybe mempty (\s -> "type = @" <> s <> "@") met
      _vkEnumsMembers = Sections [] []
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
parseVkEnum baseExtNumber mTypeName
    = (^.._Just.traverse) <$> parseTagForceAttrs "enum" parseAttrs pure
  where
    parseAttrs :: ReaderT ParseLoc AttrParser [VkEnum]
    parseAttrs = do
      _vkEnumName  <- forceAttr "name" >>= toHaskellPat

      mvalue       <- lift $ attr "value"
      mbitpos      <- lift $ attr "bitpos"
      mapi         <- lift $ attr "api"
      mtype        <- lift $ attr "type"
      malias       <- lift (attr "alias") >>= mapM toHaskellPat
      extNumber    <- let f Nothing = baseExtNumber
                          f (Just t) = case decOrHex t of
                            Left _ -> baseExtNumber
                            Right (i, _) -> fromInteger i
                      in lift $ f <$> attr "extnumber"
      comment      <- lift $ fromMaybe mempty <$> attr "comment"
      moffset      <- lift $ attr "offset"
      negDir       <- lift $ (Just "-" ==) <$> attr "dir"
      _vkEnumTName <- fmap (<|> mTypeName)
                    $ lift (attr "extends") >>= mapM toHaskellType
      let _vkEnumComment = comment
                       <:> maybe mempty (\s -> "bitpos = @" <> s <> "@") mbitpos
                       <:> maybe mempty (\s -> "api = @" <> s <> "@") mapi
                       <:> maybe mempty (\s -> "type = @" <> s <> "@") mtype

      _vkEnumValue <-
        case _vkEnumTName of

          -- if vk type name is given, we are confident the type is Int32
          Just tname -> makeNumeric extNumber
              (T.signed decOrHex <$> mvalue)
              (decOrHex <$> mbitpos)
              (decOrHex <$> moffset)
              negDir
              (unVkTypeName tname)

          -- if there is no type name, we are dealing with a constant,
          -- that can be WordXX, IntXX, Num a, Fractional a, or literal string
          Nothing -> case mvalue of
            -- this is a refernce to another existing constant
            Nothing -> pure VkEnumReference

            Just val

                -- this is a string literal
              | matched $ val ?=~ [re|^"[0-9A-Za-z_]+"$|]
                -> pure . VkEnumString . read $ T.unpack val

                -- this is a fractional number
              | Right (v, "f") <- T.rational val
                -> pure $ VkEnumFractional v

                -- arbitrary integral
              | Right (v, "") <- T.signed T.decimal val
                -> pure $ VkEnumIntegral v "(Num a, Eq a) => a"

                -- this is a string literal
              | matched $ val ?=~ [re|^[A-Z][0-9A-Za-z_]*$|]
                -> VkEnumAlias <$> toHaskellPat val

                -- special unparsed cases
              | "(~0U)" <- val
                -> pure $ VkEnumIntegral (toInteger (complement 0 :: Word32)) "Word32"
              | "(~0ULL)" <- val
                -> pure $ VkEnumIntegral (toInteger (complement 0 :: Word64)) "Word64"
              | "(~0U-1)" <- val
                -> pure $ VkEnumIntegral (toInteger (complement 0 - 1 :: Word32)) "Word32"
              | "(~0U-2)" <- val
                -> pure $ VkEnumIntegral (toInteger (complement 0 - 2 :: Word32)) "Word32"
              -- language-c
              -- Prelude Language.C Language.C.Analysis.ConstEval Language.C.Analysis.TravMonad>
              --   let md = MachineDesc (const 8) (const 8) (const 8) 8 8 (const 8) (const 8) (const 8) 8 8
              -- Prelude Language.C Language.C.Analysis.ConstEval Language.C.Analysis.TravMonad>
              --   runTrav_ . constEval md mempty <$> execParser_ expressionP (inputStreamFromString "(~0U-2)") nopos
              -- Right (Right (CConst (CIntConst -3 (NodeInfo <no file> (<no file>,-1) (Name {nameId = 1}))),[]))


              | otherwise
                -> parseFailed
                $ "parseVkEnum: value " <> T.unpack val
                <> " does not match any known enum kind."

      let rEnum = VkEnum{..}
          aliasEnumL = case malias of
            Nothing -> []
            Just alias -> [rEnum & vkEnumName .~ alias]

      return $ case (_vkEnumValue == VkEnumReference, malias) of
        (True, Just alias) -> [rEnum & vkEnumValue .~ VkEnumAlias alias]
        (_, _) -> rEnum : aliasEnumL

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
    fromOffset extNumber isNegDir offset
      | base_value <- 1000000000
      , range_size <- 1000
      , vAbs <- base_value + range_size * (fromIntegral extNumber - 1) + offset
      , v <- if isNegDir then negate vAbs else vAbs
      = fromValue v

    makeNumeric _ (Just (Right (value,_))) _mbitpos _moffset _ndir ttype
      = pure $ VkEnumIntegral (fromValue value) ttype
    makeNumeric _ _mvalue (Just (Right (bitpos,_))) _moffset _ndir ttype
      = pure $ VkEnumIntegral (fromBitpos bitpos) ttype
    makeNumeric en _mvalue _mbitpos (Just (Right (offset,_))) ndir ttype
      = pure $ VkEnumIntegral (fromOffset en ndir offset) ttype
    makeNumeric _ (Just (Left err)) _ _ _ _ = parseFailed
              $ "Could not parse enum value: " <> err
    makeNumeric _ _ (Just (Left err)) _ _ _ = parseFailed
              $ "Could not parse enum bitpos: " <> err
    makeNumeric _ _ _ (Just (Left err)) _ _ = parseFailed
              $ "Could not parse enum offset: " <> err
    makeNumeric _ Nothing Nothing Nothing _ _ = pure VkEnumReference
