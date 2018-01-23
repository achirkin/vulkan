{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}

module VkXml.Sections.Extensions
  ( parseExtensions
  , VkExtensions (..), VkExtension (..)
  , VkExtRequire (..), VkExtAttrs (..)
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


data VkExtensions
  = VkExtensions
  { comment    :: Text
  , extensions :: [VkExtension]
  } deriving Show

data VkExtension
  = VkExtension
  { attributes  :: VkExtAttrs
  , extRequires :: [Sections (VkExtRequire, Maybe Text)]
  } deriving Show

data VkExtAttrs
  = VkExtAttrs
  { extName      :: VkExtensionName
  , extSupported :: Text
  , extContact   :: Maybe Text
  , extAuthor    :: Maybe VkTagName
  , extType      :: Maybe Text
  , extNumber    :: Int
  , extReqExts   :: [VkExtensionName]
  , extProtect   :: Maybe Text
  } deriving Show

data VkExtRequire
  = VkExtReqEnumEmpty
    { enumName :: VkEnumValueName
    }
  | VkExtReqEnumValueNum
    { enumName    :: VkEnumValueName
    , enumValNum  :: Int
    , enumExtends :: Maybe VkTypeName
    }
  | VkExtReqEnumBitpos
    { enumName    :: VkEnumValueName
    , enumBitpos  :: Word
    , enumExtends :: Maybe VkTypeName
    }
  |  VkExtReqEnumValueString
    { enumName      :: VkEnumValueName
    , enumValString :: Text
    }
  | VkExtReqEnumValueRef
    { enumName :: VkEnumValueName
    , enumRef  :: VkEnumValueName
    }
  | VkExtReqEnumOffset
    { enumName          :: VkEnumValueName
    , offset            :: Int
    , offsetDirNegative :: Bool
    , offsetExtends     :: VkTypeName
    }
  | VkExtReqCommand { commandName :: VkCommandName }
  | VkExtReqType { typeName :: VkTypeName }
  deriving Show

-- | Try to parse current tag as being "vendorids",
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "vendorids", throw an exception
parseExtensions :: VkXmlParser m => Sink Event m (Maybe VkExtensions)
parseExtensions =
  parseTagForceAttrs "extensions" (forceAttr "comment") $ \comment -> do
    extensions <- many parseVkExtension
    pure VkExtensions {..}

parseVkExtension :: VkXmlParser m => Sink Event m (Maybe VkExtension)
parseVkExtension =
    parseTagForceAttrs "extension" parseVkExtAttrs $ \attributes -> do
      extRequires <- many $ tagIgnoreAttrs "require"
                          $ parseSections parseIt
      pure VkExtension {..}
  where
    parseIt = choose
        [ parseTagForceAttrs "enum"
            parseEnumReqAttrs
            pure
        , parseTagForceAttrs "command"
            ((,) <$> (VkExtReqCommand . VkCommandName <$> forceAttr "name")
                 <*> lift (attr "comment")
            ) pure
        , parseTagForceAttrs "type"
            ((,) <$> (VkExtReqType . VkTypeName <$> forceAttr "name")
                 <*> lift (attr "comment")
            ) pure
        ]

parseEnumReqAttrs :: ReaderT ParseLoc AttrParser (VkExtRequire, Maybe Text)
parseEnumReqAttrs = do
  ename <- VkEnumValueName <$> forceAttr "name"
  moff <- lift $ attr "offset"
  mcomment <- lift $ attr "comment"
  case decOrHex <$> moff of
    Just (Right (off, _)) -> do
      extends <- VkTypeName <$> forceAttr "extends"
      negDir <- lift $ isJust <$> attr "dir"
      return (VkExtReqEnumOffset ename off negDir extends, mcomment)
    Just (Left err) -> parseFailed
          $ "Could not parse extension.require.enum.offset " ++ err
    Nothing -> do
      mbpos <- lift $ attr "bitpos"
      mextends <- lift $ fmap VkTypeName <$> attr "extends"
      case decOrHex <$> mbpos of
        Just (Right (bpos, _)) ->
          return (VkExtReqEnumBitpos ename bpos mextends, mcomment)
        Just (Left err) -> parseFailed
          $ "Could not parse extension.require.enum.bitpos " ++ err
        Nothing -> do
          mtval <- lift $ attr "value"
          case mtval of
            Nothing -> pure (VkExtReqEnumEmpty ename, mcomment)
            Just tval -> do
              when (T.null tval) $
                parseFailed "Empty extension.require.enum.value!"
              case decOrHex tval of
                Right (val, _) -> pure ( VkExtReqEnumValueNum ename val mextends
                                       , mcomment )
                Left _ ->
                  if T.head tval == '"' && T.last tval == '"'
                  then pure ( VkExtReqEnumValueString ename
                                . T.dropEnd 1 $ T.tail tval
                            , mcomment )
                  else pure ( VkExtReqEnumValueRef ename $ VkEnumValueName tval
                            , mcomment )


parseVkExtAttrs :: ReaderT ParseLoc AttrParser VkExtAttrs
parseVkExtAttrs = do
  extName      <- VkExtensionName <$> forceAttr "name"
  extSupported <- forceAttr "supported"
  extContact   <- lift $ attr "contact"
  extAuthor    <- lift $ fmap VkTagName <$> attr "author"
  extType      <- lift $ attr "type"
  extProtect   <- lift $ attr "protect"
  extReqExts   <- map VkExtensionName . maybe [] (T.split (',' ==))
              <$> lift (attr "requires")
  eextNumber   <- decOrHex <$> forceAttr "number"
  case eextNumber of
    Left err -> parseFailed $ "Could not parse extension.number: " ++ err
    Right (extNumber,_) ->  pure VkExtAttrs {..}
