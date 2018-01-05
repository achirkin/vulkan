{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
-- | Vulkan types, as they defined in vk.xml
module VkXml.Sections.Types
  ( parseTypes
  , VkTypeName (..), VkTypeQualifier (..)
  , VkTypeAttrs (..), VkMemberAttrs (..)
  , VkTypeCategory (..)
  , VkType (..), VkTypeData (..), VkTypeMember (..)

  , VkEnumName (..), VkEnumValueName (..)
  , VkMemberName (..), VkName (..)
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Class
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Coerce
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.Map                   (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.String                (IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Read             as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.Parser


-- * Types


newtype VkEnumName = VkEnumName { unVkEnumName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

newtype VkEnumValueName = VkEnumValueName { unVkEnumValueName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | Type name
newtype VkTypeName = VkTypeName { unVkTypeName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | E.g. member of a struct
newtype VkMemberName = VkMemberName { unVkMemberName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | Some identifier
newtype VkName = VkName { unVkName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

data VkTypeQualifier
  = VkTypeQStar
  | VkTypeQArrLen Int
  | VkTypeQArrLenEnum VkEnumValueName
  deriving Show

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

data VkTypeData name
  = VkTypeData
  { name      :: Maybe (name, [VkTypeQualifier])
    -- ^ name parsed from type or member content
  , reference :: [(name, [VkTypeQualifier])]
    -- ^ content of "type" child; typically, a part of type definition
  , comment   :: Maybe Text
    -- ^ optional comment
  , code      :: Text
    -- ^ raw code with xml tags removed
  } deriving Show



data VkTypeMember
  = VkTypeMember
  { name       :: VkMemberName
  , attributes :: VkMemberAttrs
  , memberData :: VkTypeData VkMemberName
  } deriving Show



data VkMemberAttrs
  = VkMemberAttrs
  { values         :: Maybe VkEnumValueName
    -- ^ Enum value indicating a certain struct type
  , optional       :: Bool
  , len            :: Maybe Text
    -- ^ normally, this is c-like expression depending on other struct members
  , noautovalidity :: Bool
  , externsync     :: Bool
  }
  deriving Show


data VkType
  = VkTypeSimple
    { name       :: VkTypeName
    , attributes :: VkTypeAttrs
    , typeData   :: VkTypeData VkTypeName
    }
  | VkTypeComposite
    { name       :: VkTypeName
    , attributes :: VkTypeAttrs
    , members    :: [(Maybe Text, [VkTypeMember])]
      -- ^ Members sometimes separated by comments
    }
  deriving Show






-- * Parsing


-- | Parse type definitions based on xml tag `type`
parseTypes :: VkXmlParser m
            => Sink Event m (Map VkTypeName VkType)
parseTypes = fmap (fromMaybe mempty)
           $ parseTag "types" (lift ignoreAttrs)
           $ \_ -> execStateC mempty (awaitForever go)
  where
    -- encounter new type definition
    go ev@(EventBeginElement "type" _) = do
      leftover ev -- need to put event back to do a propser parsing
      typeI <- transPipe lift parseVkType
      modify' (Map.insert (name (typeI :: VkType)) typeI)
    -- do not expect types section to have a meaningful content
    go (EventContent _)                    = return ()
    -- ignore comments for now
    go (EventBeginElement "comment" _)     = return ()
    go (EventEndElement "comment")         = return ()
    -- ignore xml comments too
    go EventComment {}                     = return ()
    -- unhandled token
    go e = parseFailed $ "unexpected token " <> show e





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




parseAttrVkMemberValues :: ReaderT ParseLoc AttrParser (Maybe VkEnumValueName)
parseAttrVkMemberValues = lift $ fmap VkEnumValueName <$> attr "values"


parseAttrVkMemberOptional :: ReaderT ParseLoc AttrParser Bool
parseAttrVkMemberOptional = do
  mr <- lift $ attr "optional"
  case T.toLower <$> mr of
    Just "true" -> pure True
    _           -> pure False

parseAttrVkMemberLen ::  ReaderT ParseLoc AttrParser (Maybe Text)
parseAttrVkMemberLen = lift $ do
  maltlen <- attr "altlen"
  mlen    <- attr "len"
  pure $ maltlen <|> mlen


parseAttrVkMemberNoautovalidity :: ReaderT ParseLoc AttrParser Bool
parseAttrVkMemberNoautovalidity = do
  mr <- lift $ attr "noautovalidity"
  case T.toLower <$> mr of
    Just "true" -> pure True
    _           -> pure False

parseAttrVkMemberExternsync:: ReaderT ParseLoc AttrParser Bool
parseAttrVkMemberExternsync = do
  mr <- lift $ attr "externsync"
  case T.toLower <$> mr of
    Just "true" -> pure True
    _           -> pure False


parseVkMemberAttrs :: ReaderT ParseLoc AttrParser VkMemberAttrs
parseVkMemberAttrs = VkMemberAttrs <$> parseAttrVkMemberValues
                                   <*> parseAttrVkMemberOptional
                                   <*> parseAttrVkMemberLen
                                   <*> parseAttrVkMemberNoautovalidity
                                   <*> parseAttrVkMemberExternsync



parseVkTypeData :: ( Coercible Text name
                   , VkXmlParser m
                   )
                => Sink Event m (VkTypeData name)
parseVkTypeData =
    parseIt (VkTypeData Nothing [] Nothing mempty)
  where
    parseQualifiers n = do
      mc <- contentMaybe
      case mc of
        Nothing -> return (n, [])
        Just c -> do
          -- filter out enums
          menum <- join <$> tagIgnoreAttrs "enum" contentMaybe
          case menum of
            Nothing -> (n, getSimpleQualifiers c)
                    <$ leftover (EventContent (ContentText c))
            Just en -> do
              leftover (EventContent (ContentText $ c <> en))
              parseQualifiers n
    getSimpleQualifiers t = case T.uncons (T.stripStart t) of
      Just ('*', s) -> VkTypeQStar : getSimpleQualifiers s
      Just ('[', s) -> case T.decimal (T.stripStart s) of
        Left _       -> case T.breakOn "]" s of
           (enumname, rest) -> VkTypeQArrLenEnum (VkEnumValueName $ T.strip enumname)
                               : getSimpleQualifiers (T.drop 1 rest)
        Right (i, u) -> case T.uncons (T.stripStart u) of
          Just (']', v) -> VkTypeQArrLen i : getSimpleQualifiers v
          _             -> []
      _ -> []
    parseIt d@VkTypeData{..} = do
      mev <- await
      case mev of
        Nothing -> return d
        Just ev -> leftover ev >> do
         mnewname <- join
                 <$> tagIgnoreAttrs "name" (coerce <$> contentMaybe)
         mnewnameq <- mapM parseQualifiers mnewname
         mnewref <- join
                 <$> tagIgnoreAttrs "type" (coerce <$> contentMaybe)
         mnewrefq <- mapM parseQualifiers mnewref
         mnewcomment <- join <$> tagIgnoreAttrs "comment" contentMaybe
         newcontent <- content
         -- fallback to error if could not manage to parse anything
         when (  isNothing mnewname
              && isNothing mnewref
              && isNothing mnewcomment
              && newcontent  == mempty
              ) $ parseFailed $ "Unexpected event in type content tree: "
                             <> show ev
         -- add newly parsed stuff to VkTypeData
         parseIt (d
             { name      = mnewnameq <|> name
             , reference = reference <> maybeToList mnewrefq
             , comment   = mnewcomment <|> comment
             , code      = code
                         <> maybe mempty coerce mnewname
                         <> maybe mempty coerce mnewref
                         <> newcontent
             }
           )



parseVkType :: VkXmlParser m
            => Sink Event m VkType
parseVkType = do
    mr <- parseTagForceAttrs "type" parseVkTypeAttrs $ \attrs -> do
      mems <- parseMembers []
      case (mems, name (attrs :: VkTypeAttrs)) of
        ([], Just n) -> VkTypeSimple n attrs <$> parseVkTypeData
        (_ , Just n) -> pure $ VkTypeComposite n attrs mems
        ([], Nothing) -> do
          d <- parseVkTypeData
          case fst <$> name (d :: VkTypeData VkTypeName) of
            Just n -> pure $ VkTypeSimple n attrs d
            Nothing -> parseFailed
                     $ "Could not get type name from tag content or attributes "
                     <> show attrs <> " "
                     <> show d
        (_, Nothing) -> parseFailed
                     $ "Could not get type name from tag attributes "
                     <> show attrs
    case mr of
      Nothing -> parseFailed
                "failed to parse tag type, unexpected evet tag 'type'"
      Just r  -> return r
  where
    parseMembers mems = do
      mev <- await
      case mev of
        Nothing -> return mems
        Just ev -> leftover ev >> do
          mcomment <- tagIgnoreAttrs "comment" contentMaybe
          mmem <- parseTagForceAttrs "member"
            parseVkMemberAttrs
            (\ma -> do
              d <- parseVkTypeData
              case fst <$> name (d :: VkTypeData VkMemberName) of
                Just n -> VkTypeMember n ma <$> parseVkTypeData
                Nothing -> parseFailed
                         $ "Could not get name from member tag content "
                         <> show d
            )
          case (mcomment, mmem, mems) of
            (Just com, Just mem, _) -> parseMembers ((com, [mem]):mems)
            (Nothing, Nothing, _) -> pure $ reverse mems
            (Nothing, Just mem, []) -> parseMembers [(Nothing, [mem])]
            (Nothing, Just mem, (c,ms):xs) -> parseMembers ((c, ms ++ [mem]):xs)
            -- looks like we have read a wrong thing!
            (Just (Just com), Nothing, []) -> do
              leftover (EventBeginElement "comment" [])
              leftover (EventContent (ContentText com))
              leftover (EventEndElement "comment")
              return []
            _ -> pure $ reverse mems
