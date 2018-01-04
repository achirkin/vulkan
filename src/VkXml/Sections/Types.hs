{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE Strict                     #-}
-- | Vulkan types, as they defined in vk.xml
module VkXml.Sections.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.Map                   (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.String                (IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           Debug.Trace
import           VkXml.Parser

newtype VkTypeName = VkTypeName { _unVkTypeName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)


data VkTypeDef
  = VkTypeDef
  { name          :: VkTypeName
  , category      :: VkTypeCategory
  , requires      :: Maybe VkTypeName
  , parent        :: Maybe VkTypeName
  , returnedonly  :: Bool
  , comment       :: Text
  , structextends :: Maybe VkTypeName
  } deriving Show


data VkTypeAttrs
  = VkTypeAttrs
  { name          :: Maybe VkTypeName
  , category      :: VkTypeCategory
  , requires      :: Maybe VkTypeName
  , parent        :: Maybe VkTypeName
  , returnedonly  :: Bool
  , comment       :: Text
  , structextends :: Maybe VkTypeName
  } deriving Show

data VkTypeCategory
  = VkTypeNoCat
  | VkTypeCatInclude
  | VkTypeCatDefine
  | VkTypeCatBasetype
  | VkTypeCatBitmask
  | VkTypeCatHandle
  | VkTypeCatEnum
  | VkTypeCatFuncpointer
  | VkTypeCatStruct
  | VkTypeCatUnion
  deriving (Eq,Ord,Show)

parseAttrVkTypeCategory :: ReaderT ParseLoc AttrParser VkTypeCategory
parseAttrVkTypeCategory = do
  mcat <- lift $ attr "category"
  case mcat of
    Nothing            -> return VkTypeNoCat
    Just "include"     -> return VkTypeCatInclude
    Just "define"      -> return VkTypeCatDefine
    Just "basetype"    -> return VkTypeCatBasetype
    Just "bitmask"     -> return VkTypeCatBitmask
    Just "handle"      -> return VkTypeCatHandle
    Just "enum"        -> return VkTypeCatEnum
    Just "funcpointer" -> return VkTypeCatFuncpointer
    Just "struct"      -> return VkTypeCatStruct
    Just "union"       -> return VkTypeCatUnion
    Just unknown       -> parseFailed $ "unknown type category " <> show unknown

parseAttrVkTypeName :: ReaderT ParseLoc AttrParser (Maybe VkTypeName)
parseAttrVkTypeName = lift $ fmap VkTypeName <$> attr "name"


parseAttrVkTypeRequires :: ReaderT ParseLoc AttrParser (Maybe VkTypeName)
parseAttrVkTypeRequires = lift $ fmap VkTypeName <$> attr "requires"

parseAttrVkTypeParent :: ReaderT ParseLoc AttrParser (Maybe VkTypeName)
parseAttrVkTypeParent = lift $ fmap VkTypeName <$> attr "parent"

parseAttrVkTypeReturnedonly :: ReaderT ParseLoc AttrParser Bool
parseAttrVkTypeReturnedonly = do
  mr <- lift $ attr "returnedonly"
  case T.toLower <$> mr of
    Just "true" -> pure True
    _           -> pure False

parseAttrVkTypeComment :: ReaderT ParseLoc AttrParser Text
parseAttrVkTypeComment = lift (fromMaybe mempty <$> attr "comment")

parseAttrVkTypeStructextends :: ReaderT ParseLoc AttrParser (Maybe VkTypeName)
parseAttrVkTypeStructextends = lift $ fmap VkTypeName <$> attr "structextends"


parseVkTypeAttrs :: ReaderT ParseLoc AttrParser VkTypeAttrs
parseVkTypeAttrs = VkTypeAttrs <$> parseAttrVkTypeName
                               <*> parseAttrVkTypeCategory
                               <*> parseAttrVkTypeRequires
                               <*> parseAttrVkTypeParent
                               <*> parseAttrVkTypeReturnedonly
                               <*> parseAttrVkTypeComment
                               <*> parseAttrVkTypeStructextends

parseType :: VkXmlParser m
          => Sink Event m VkTypeDef
parseType = do
  mr <- parseTagForceAttrs "type" parseVkTypeAttrs $ \attrs -> do
    traceShowM attrs
    awaitForever $ \ev -> traceShowM ev
    -- ev <- await >>= maybe (parseFailed "unexpected end of input") pure

    return $ VkTypeDef (fromMaybe "yo" (name (attrs :: VkTypeAttrs)))
                       (category      (attrs :: VkTypeAttrs))
                       (requires      (attrs :: VkTypeAttrs))
                       (parent        (attrs :: VkTypeAttrs))
                       (returnedonly  (attrs :: VkTypeAttrs))
                       (comment       (attrs :: VkTypeAttrs))
                       (structextends (attrs :: VkTypeAttrs))
  case mr of
    Nothing -> parseFailed
              "failed to parse tag type, unexpected evet tag 'type'"
    Just r  -> return r

-- | Parse type definitions based on xml tag `type`
parseTypes :: VkXmlParser m
           => Sink Event m (Map VkTypeName VkTypeDef)
parseTypes = fmap (fromMaybe mempty)
           $ parseTag "types" (lift ignoreAttrs)
           $ \_ -> execStateC mempty (awaitForever go)
  where
    -- encounter new type definition
    go ev@(EventBeginElement "type" _) = do
      leftover ev
      traceM "Starting pasing a type!"
      typeDef <- transPipe lift parseType
      traceM "parsed a type!"
      ask >>= traceShowM
      modify' (Map.insert (name (typeDef :: VkTypeDef)) typeDef)
    -- do not expect types section to have a meaningful content
    go (EventContent _)                    = return ()
    -- ignore comments for now
    go (EventBeginElement "comment" _)     = return ()
    go (EventEndElement "comment")         = return ()
    -- ignore xml comments too
    go EventComment {}                     = return ()
    -- unhandled token
    go e = parseFailed $ "unexpected token " <> show e
