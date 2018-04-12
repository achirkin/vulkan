{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module VkXml.CommonTypes
  ( VkTypeName (..)
  , VkEnumName (..)
  , VkMemberName (..)
  , VkCommandName (..)
  , VkPlatformName (..)
  , Sections (..), VkTagName (..)
  , VkExtensionName (..)
  , ProtectCPP (..), ProtectFlag (..), ProtectDef (..)
  , parseSections, parseSectionsL
  , (<:>)
  , toType, toQName, qNameTxt
  , moduleName, unqualify
  , TypeScope (..)
    -- * Initial ident parsing
  , toHaskellType, toHaskellPat, toHaskellVar, toHaskellExt
  , toHaskellComm, toHaskellMemb
  , commaSeparated
  , toProtectDef
    -- * Useful helpers
  , firstUp
  ) where

import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Data.Char
import           Data.Coerce
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.Maybe                   (fromMaybe)
import           Data.String                  (IsString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.XML.Types               hiding (Name)
import           GHC.Stack
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.Syntax
import           Text.XML.Stream.Parse

import           VkXml.Parser


newtype VkEnumName = VkEnumName { unVkEnumName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | Type name
newtype VkTypeName = VkTypeName { unVkTypeName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | E.g. member of a struct
newtype VkMemberName = VkMemberName { unVkMemberName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | Command name
newtype VkCommandName = VkCommandName { unVkCommandName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)


newtype VkTagName = VkTagName { unVkTagName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

newtype VkPlatformName = VkPlatformName { unVkPlatformName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)


newtype VkExtensionName = VkExtensionName { unVkExtensionName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)


newtype ProtectCPP = ProtectCPP { unProtectCPP :: Text }
  deriving (Eq, Ord, Show, Read, IsString)


newtype ProtectFlag = ProtectFlag  { unProtectFlag :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

data ProtectDef
  = ProtectDef
  { protectCPP  :: ProtectCPP
  , protectFlag :: ProtectFlag
  } deriving (Eq, Ord, Show, Read)


-- | Parse a list of elements interspersed with comments,
--   that overwise would be a homogenous list of xml tag.
data Sections a
  = Sections
  { items    :: [a]
    -- ^ List of items in original order from xml file
  , comments :: [(Int, Text)]
    -- ^ Comments and their indices in between items.
    --   e.g. (0,"abc") means comment inserted before anything else.
    --   e.g. (length (items xs), "abc") means comment inserted after a.e. .
  } deriving Show

-- | Parse elements and comments in between them.
parseSectionsL :: VkXmlParser m
              => Sink Event m [a] -- ^ how to parse elements
              -> Sink Event m (Sections a)
parseSectionsL parseElem = evalStateC (0::Int) parseIt
  where
    prepComment c ~secs@(Sections _ cs) = secs{ comments = c:cs}
    prepItem    e ~secs@(Sections es _) = secs{ items    = e:es}
    parseIt = do
      mnewcomment <- tagIgnoreAttrs "comment" content
      me <- transPipe lift parseElem
      case (mnewcomment, me) of
        (Nothing, []) -> pure $ Sections [] []
        (Just comment, e:es) -> do
          i <- get
          modify' (+1)
          prepComment (i,comment)
            . prepItem e
            . flip (foldr prepItem) (reverse es) <$> parseIt
        (Just comment, []) -> do
          i <- get
          prepComment (i,comment) <$> parseIt
        (Nothing, e:es) -> do
          modify' (+1)
          prepItem e
             . flip (foldr prepItem) (reverse es) <$> parseIt

parseSections :: VkXmlParser m
              => Sink Event m (Maybe a) -- ^ how to parse elements
              -> Sink Event m (Sections a)
parseSections parseElem = parseSectionsL (g <$> parseElem)
  where
    g Nothing  = []
    g (Just a) = [a]


-- | Combine two comments vertically
(<:>) :: Text -> Text -> Text
a <:> b
  | T.null (T.strip a) = b
  | T.null (T.strip b) = a
  | otherwise          = T.unlines [a, "", b]
infixr 6 <:>



toHaskellType :: (VkXmlParser m, HasCallStack) => Text -> m VkTypeName
toHaskellType "()"       = pure $ VkTypeName "()"
toHaskellType "void"     = pure $ VkTypeName "()"
toHaskellType "char"     = pure $ VkTypeName "CChar"
toHaskellType "float"    = pure $ VkTypeName "HSC2HS___ \"#{type float}\""
toHaskellType "double"   = pure $ VkTypeName "HSC2HS___ \"#{type double}\""
toHaskellType "uint8_t"  = pure $ VkTypeName "Word8"
toHaskellType "uint16_t" = pure $ VkTypeName "Word16"
toHaskellType "uint32_t" = pure $ VkTypeName "Word32"
toHaskellType "uint64_t" = pure $ VkTypeName "Word64"
toHaskellType "int8_t"   = pure $ VkTypeName "Int8"
toHaskellType "int16_t"  = pure $ VkTypeName "Int16"
toHaskellType "int32_t"  = pure $ VkTypeName "Int32"
toHaskellType "int64_t"  = pure $ VkTypeName "Int64"
toHaskellType "size_t"   = pure $ VkTypeName "CSize"
toHaskellType "int"      = pure $ VkTypeName "CInt"
-- exceptions **************************************************
-- Linux types
toHaskellType t
  | "wl_" `T.isPrefixOf` t || "xcb_" `T.isPrefixOf` t
    = toHaskellType . firstUp . T.pack . toCamelCase $ T.unpack t
-- Header files
toHaskellType t
  | ".h" `T.isSuffixOf` t
    = pure $ VkTypeName t
-- *************************************************************
toHaskellType t
  |  "HSC2HS___" `T.isPrefixOf` t
    = pure $ VkTypeName t
toHaskellType t
  | isHaskellIdent t = pure $ VkTypeName . firstUp $ T.dropWhile ('_' ==) t
  | otherwise = parseFailed $ "Invalid haskell type name " ++ show t


toHaskellVar :: (VkXmlParser m, HasCallStack) => Text -> m Text
toHaskellVar t
  | isHaskellIdent t = pure $ firstDown t
  | otherwise = parseFailed $ "Invalid haskell variable name " ++ show t


toHaskellPat :: (VkXmlParser m, HasCallStack) => Text -> m VkEnumName
toHaskellPat t
  | isHaskellIdent t = pure . VkEnumName . firstUp $ T.dropWhile ('_' ==) t
  | otherwise = parseFailed $ "Invalid haskell pattern/enum name " ++ show t


toHaskellExt :: (VkXmlParser m, HasCallStack) => Text -> m VkExtensionName
toHaskellExt t
  | isHaskellIdent t = pure . VkExtensionName . firstUp $ T.dropWhile ('_' ==) t
  | otherwise = parseFailed $ "Invalid haskell extension (module) name " ++ show t

toHaskellComm :: (VkXmlParser m, HasCallStack) => Text -> m VkCommandName
toHaskellComm = fmap VkCommandName . toHaskellVar

toHaskellMemb :: (VkXmlParser m, HasCallStack) => Text -> m VkMemberName
toHaskellMemb t
  | isHaskellIdent t = pure $ VkMemberName t
  | otherwise = parseFailed $ "Invalid haskell member name " ++ show t


toFlagName :: ProtectCPP -> ProtectFlag
toFlagName
    = ProtectFlag
    . firstDown
    . T.pack
    . toCamelCase
    . T.unpack
    . T.toLower
    . removeVk
  where
    removeVk (ProtectCPP g) = fromMaybe g $ T.stripPrefix "VK_" g

toProtectDef :: (VkXmlParser m, HasCallStack) => Text -> m ProtectDef
toProtectDef t
    | isHaskellIdent t = pure $ ProtectDef pCPP (toFlagName pCPP)
    | otherwise = parseFailed
       $ "Invalid protect key (which should be a CPP macros) " ++ show t
  where
    pCPP = ProtectCPP t

commaSeparated :: Maybe Text -> [Text]
commaSeparated = maybe [] (T.split (',' ==))


class TypeScope a where
  requiresTypes :: a -> [VkTypeName]
  providesTypes :: a -> [VkTypeName]

-- * Checking type and value names


firstUp :: Text -> Text
firstUp s = case T.uncons s of
  Just (a, ss) -> T.cons (toUpper a) ss
  Nothing      -> s

firstDown :: Text -> Text
firstDown s = case T.uncons s of
  Just (a, ss) -> T.cons (toLower a) ss
  Nothing      -> s


-- | check if this is a valid haskell-vulkan ident
--   (only ascii alhanumeric chars are accepted, first must be alpha or underscore).
isHaskellIdent :: Text -> Bool
isHaskellIdent s
  | T.null s = False
  | isDigit (T.head s) = False
  | T.any invalidChar s = False
  | otherwise = True
  where
    validChar c = isAscii c && (isAlphaNum c || c == '_')
    invalidChar = not . validChar


-- | Construct a type from a qualified type name and pointer level.
--   If the type is c void and it is wrapped into a Ptr,
--     I treat it as Void.
toType :: Word -- ^ number of times pointer
       -> VkTypeName -- ^ name of the type
       -> Type ()
toType n t | t == "()"    = if n > 0 then appPtr n voidTy
                                     else unitTy
           | n == 0       = tty
           | n > 0
           , t == "CChar" = toType (n-1) "CString"
           | otherwise    = appPtr n tty
  where
    appPtr 0 ty = ty
    appPtr k ty = appPtr (k-1) $ TyApp () ptrTy ty
    voidTy = TyCon () (UnQual () (Ident () "Void"))
    ptrTy  = TyCon () (UnQual () (Ident () "Ptr"))
    tty    = TyCon () (UnQual () (Ident () . T.unpack $ unVkTypeName t))
    unitTy = TyCon () (Special () (UnitCon ()))


toQName :: Coercible a Text => a -> QName ()
toQName = toQName' . coerce

toQName' :: Text -> QName ()
toQName' "()" = Special () (UnitCon ())
toQName' t    = UnQual () . Ident () $ T.unpack t


unqualify :: QName a -> Name a
unqualify (Qual _ _ n)  = n
unqualify (UnQual _ n)  = n
unqualify (Special _ _) = error "unqualify: cannot unqualify special name."


moduleName :: QName a -> Maybe String
moduleName (Qual _ (ModuleName _ m) _) = Just m
moduleName UnQual{}                    = Nothing
moduleName Special{}                   = Nothing


qNameTxt :: QName a -> Text
qNameTxt (Qual _ _ (Ident _ t))  = T.pack t
qNameTxt (Qual _ _ (Symbol _ t)) = T.pack t
qNameTxt (UnQual _ (Ident _ t))  = T.pack t
qNameTxt (UnQual _ (Symbol _ t)) = T.pack t
qNameTxt (Special _ t)           = T.pack $ prettyPrint t


toCamelCase :: String -> String
toCamelCase ('_':c:cs)
  | isLower c = toUpper c : toCamelCase cs
toCamelCase (c:cs) = c : toCamelCase cs
toCamelCase [] = []
