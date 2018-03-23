{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
-- | Vulkan types, as they defined in vk.xml
module VkXml.Sections.Types
  ( parseTypes
  , VkTypes
  , VkTypeQualifier (..)
  , VkTypeAttrs (..), VkMemberAttrs (..)
  , VkTypeCategory (..)
  , VkType (..), VkTypeData (..), VkTypeMember (..)
  , vkTypeCat
  ) where

import           Control.Applicative        ((<|>))
import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Char                  (isSpace)
import           Data.Coerce
import           Data.Conduit
import           Data.List                  (nub, union)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.XML.Types
import           Text.RE.TDFA.Text
import           Text.XML.Stream.Parse

import           VkXml.CommonTypes
import           VkXml.Parser

-- * Types

type VkTypes = Map VkTypeName VkType

data VkType
  = VkTypeSimple
    { name       :: VkTypeName
    , attributes :: VkTypeAttrs
    , typeData   :: VkTypeData VkTypeName
    }
  | VkTypeComposite
    { name       :: VkTypeName
    , attributes :: VkTypeAttrs
    , members    :: Sections VkTypeMember
      -- ^ Members sometimes separated by comments
    }
  deriving Show

instance TypeScope VkType where
  providesTypes = (:[]) . (name :: VkType -> VkTypeName)
  requiresTypes VkTypeSimple {..} =
    requiresTypes attributes `union` requiresTypes typeData
  requiresTypes VkTypeComposite {..} =
    requiresTypes attributes `union`
      nub (items members >>= requiresTypes)

instance TypeScope VkTypeAttrs where
  providesTypes = maybeToList . (name :: VkTypeAttrs -> Maybe VkTypeName)
  requiresTypes VkTypeAttrs {..}
    = nub $ structextends ++ parent ++ maybeToList requires

instance TypeScope (VkTypeData a) where
  providesTypes _ = []
  requiresTypes VkTypeData {..} = nub $
    maybeToList (fmap fst retType) ++ map fst reference

instance TypeScope VkTypeMember where
  providesTypes _ = []
  requiresTypes VkTypeMember {..} = requiresTypes memberData



data VkTypeQualifier
  = VkTypeQStar
  | VkTypeQArrLen Int
  | VkTypeQArrLenEnum VkEnumName
  deriving Show


-- <https://www.khronos.org/registry/vulkan/specs/1.1/registry.html#_attributes_of_code_type_code_tags >
data VkTypeAttrs
  = VkTypeAttrs
  { name          :: Maybe VkTypeName
  , alias         :: Maybe VkTypeName
    -- ^ Additional name for this type
  , category      :: VkTypeCategory
  , requires      :: Maybe VkTypeName
  , parent        :: [VkTypeName]
  , returnedonly  :: Bool
  , comment       :: Text
  , structextends :: [VkTypeName]
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
  , retType   :: Maybe (VkTypeName, Word)
    -- ^ Makes only sense for funcpointer category
  , reference :: [(VkTypeName, [VkTypeQualifier])]
    -- ^ content of "type" child; typically, a part of type definition
  , comment   :: Maybe Text
    -- ^ optional comment
  , code      :: Text
    -- ^ raw code with xml tags removed
  } deriving Show


vkTypeCat :: VkType -> VkTypeCategory
vkTypeCat = category
          . (attributes :: VkType -> VkTypeAttrs)

data VkTypeMember
  = VkTypeMember
  { name       :: VkMemberName
  , attributes :: VkMemberAttrs
  , memberData :: VkTypeData VkMemberName
  } deriving Show



data VkMemberAttrs
  = VkMemberAttrs
  { values         :: Maybe VkEnumName
    -- ^ Enum value indicating a certain struct type
  , optional       :: Bool
  , len            :: Maybe Text
    -- ^ normally, this is c-like expression depending on other struct members
  , noautovalidity :: Bool
  , externsync     :: Bool
  }
  deriving Show




-- * Parsing


-- | Try to parse current tag as being types,
--
--   * If tag name does not match, return events upstream as leftovers
--   * If failed to parse tag "types", throw an exception
parseTypes :: VkXmlParser m => Sink Event m (Maybe VkTypes)
parseTypes = parseTagForceAttrs "types" (lift $ attr "comment")
  $ \_ ->
    Map.fromList . fmap (\t -> ( (name :: VkType -> VkTypeName) t,t))
    <$> manyIgnore parseVkType (ignoreTreeContent "comment")



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
parseAttrVkTypeName = lift (attr "name") >>= mapM toHaskellType

parseAttrVkTypeAlias :: ReaderT ParseLoc AttrParser (Maybe VkTypeName)
parseAttrVkTypeAlias = lift (attr "alias") >>= mapM toHaskellType

parseAttrVkTypeRequires :: ReaderT ParseLoc AttrParser (Maybe VkTypeName)
parseAttrVkTypeRequires = lift (attr "requires") >>= mapM toHaskellType

parseAttrVkTypeParent :: ReaderT ParseLoc AttrParser [VkTypeName]
parseAttrVkTypeParent = commaSeparated <$> lift (attr "parent")
                    >>= mapM toHaskellType

parseAttrVkTypeReturnedonly :: ReaderT ParseLoc AttrParser Bool
parseAttrVkTypeReturnedonly = do
  mr <- lift $ attr "returnedonly"
  case T.toLower <$> mr of
    Just "true" -> pure True
    _           -> pure False

parseAttrVkTypeComment :: ReaderT ParseLoc AttrParser Text
parseAttrVkTypeComment = lift (fromMaybe mempty <$> attr "comment")

parseAttrVkTypeStructextends :: ReaderT ParseLoc AttrParser [VkTypeName]
parseAttrVkTypeStructextends
  = commaSeparated <$> lift (attr "structextends")
  >>= mapM toHaskellType


parseVkTypeAttrs :: ReaderT ParseLoc AttrParser VkTypeAttrs
parseVkTypeAttrs = VkTypeAttrs <$> parseAttrVkTypeName
                               <*> parseAttrVkTypeAlias
                               <*> parseAttrVkTypeCategory
                               <*> parseAttrVkTypeRequires
                               <*> parseAttrVkTypeParent
                               <*> parseAttrVkTypeReturnedonly
                               <*> parseAttrVkTypeComment
                               <*> parseAttrVkTypeStructextends




parseAttrVkMemberValues :: ReaderT ParseLoc AttrParser (Maybe VkEnumName)
parseAttrVkMemberValues = lift (attr "values") >>= mapM toHaskellPat


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


-- TODO: rewrite this either using regex or language-c
parseVkTypeData :: ( Coercible Text name
                   , VkXmlParser m
                   )
                => (Text -> m name) -> Sink Event m (VkTypeData name)
parseVkTypeData k = do
    -- handle funcpointer
    mc <- contentMaybe
    mrt <- parseFuncpointerRetType mc
    forM_ mc $ leftover . EventContent . ContentText
    parseIt (VkTypeData Nothing mrt [] Nothing mempty)
  where
    parseFuncpointerRetType Nothing = pure Nothing
    parseFuncpointerRetType (Just t)
      | Just tstars <- fmap (T.drop 7 . T.filter (not . isSpace))
          . matchedText $ t ?=~
          [re|typedef[[:space:]]+[A-Za-z][0-9A-Za-z_]*[\*[[:space:]]]*|]
      , stars <- fromIntegral . T.length $ T.filter ('*' ==) tstars
      , tp <- T.filter ('*' /=) tstars
        = Just . flip (,) stars <$> toHaskellType tp
      | otherwise = pure Nothing
    parseQualifiers n = do
      mc <- contentMaybe
      case mc of
        Nothing -> return (n, [])
        Just c -> do
          -- filter out enums
          menum <- join <$> tagIgnoreAttrs "enum" contentMaybe
          case menum of
            Nothing -> leftover (EventContent (ContentText c))
                     >> (,) n <$> getSimpleQualifiers c
            Just en -> do
              leftover (EventContent (ContentText $ c <> en))
              parseQualifiers n
    getSimpleQualifiers t = case T.uncons (T.stripStart $ T.replace "const" "" t) of
      Just ('*', s) -> (VkTypeQStar :) <$> getSimpleQualifiers s
      Just ('[', s) -> case decOrHex (T.stripStart s) of
        Left _       -> case T.breakOn "]" s of
           (enumname, rest) -> do
              nnn <- toHaskellPat $ T.strip enumname
              (VkTypeQArrLenEnum nnn :) <$> getSimpleQualifiers (T.drop 1 rest)
        Right (i, u) -> case T.uncons (T.stripStart u) of
          Just (']', v) -> (VkTypeQArrLen i :) <$> getSimpleQualifiers v
          _             -> pure []
      _ -> pure []
    parseIt d@VkTypeData{..} = do
      mr <- choose
        [ fmap (\c -> d { code = code <> c}) <$> contentMaybe
        , (join <$> tagIgnoreAttrs "type" contentMaybe) >>= mapM
          (\newrefTxt -> do
            newref <- toHaskellType newrefTxt
            newrefq <- parseQualifiers newref
            return $ d { code = code <> newrefTxt
                       , reference = reference <> [newrefq] }
          )
        , (join <$> tagIgnoreAttrs "name" contentMaybe) >>= mapM
          (\newnameTxt -> do
            newname <- lift $ k newnameTxt
            newnameq <- parseQualifiers newname
            return d { code = code <> newnameTxt, name = Just newnameq }
          )
        , fmap (dataComment d) . join <$> tagIgnoreAttrs "comment" contentMaybe
        ]
      case mr of
        Nothing -> pure d
        Just d' -> parseIt d'

dataComment :: VkTypeData a -> Text -> VkTypeData a
dataComment d@VkTypeData{ comment = Just s} t
  = d { comment = Just (T.unlines [s,t])}
dataComment d@VkTypeData{ comment = Nothing} t
  = d { comment = Just t}



parseVkType :: VkXmlParser m => Sink Event m (Maybe VkType)
parseVkType = parseTagForceAttrs "type" parseVkTypeAttrs $ \attrs ->
  hasMembers >>= \has ->
    if has
    then do
      mems <- parseSections parseVkTypeMember
      case name (attrs :: VkTypeAttrs) of
        Just n  -> pure $ VkTypeComposite n attrs mems
        Nothing -> parseFailed
                 $ "Could not get type name from tag attributes "
                 <> show attrs
    else do
      d <- parseVkTypeData toHaskellType
      case name (attrs :: VkTypeAttrs)
       <|> fst <$> name (d :: VkTypeData VkTypeName) of
        Just n -> pure $ VkTypeSimple n attrs d
        Nothing -> parseFailed
                 $ "Could not get type name from tag content or attributes "
                 <> show attrs <> " "
                 <> show d

parseVkTypeMember :: VkXmlParser m
                  => Sink Event m (Maybe VkTypeMember)
parseVkTypeMember = parseTagForceAttrs "member" parseVkMemberAttrs $ \ma -> do
    d <- parseVkTypeData toHaskellMemb
    case fst <$> name (d :: VkTypeData VkMemberName) of
      Just n  -> pure $ VkTypeMember n ma d
      Nothing -> parseFailed
               $ "Could not get name from member tag content "
               <> show d

-- | Look ahead if there are any "member" tags inside,
--   and no other tags except "member" or "comment"
hasMembers :: VkXmlParser m => Sink Event m Bool
hasMembers = do
    cmns <- grabComments
    (ctnPiece, mev) <- grabContent
    returnAll cmns ctnPiece mev
    return $ case mev of
      Just (EventBeginElement "member" _) -> True
      _                                   -> False
  where
    grabComments = many $ tagIgnoreAttrs "comment" content
    grabContent = do
      mev <- await
      case mev of
        Just (EventContent c) -> first (unContent c <>) <$> grabContent
        _                     -> return (mempty, mev)
    returnAll cmns ctnPiece mev = do
      mapM_ leftover mev
      unless (T.null ctnPiece) $
        leftover . EventContent $ ContentText ctnPiece
      mapM_ returnComment $ reverse cmns
    returnComment c = do
      leftover (EventEndElement "comment")
      leftover (EventContent (ContentText c))
      leftover (EventBeginElement "comment" [])
