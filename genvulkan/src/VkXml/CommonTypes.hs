{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module VkXml.CommonTypes
  ( VkTypeName (..), VkMemberName (..), VkCommandName (..)
  , Sections (..), VkTagName (..), VkExtensionName (..)
  , parseSections, parseSectionsL
  , VkEnumName (..)
  , (<:>)
  , ValidIdent (..)
  , isHaskellIdent, isHaskellLowerFirst, isHaskellUpperFirst
  , firstUp, firstDown, toCamelCase
  , toHaskellName, toHaskellName', toType
  , moduleName, unqualifyQ, unqualify, qNameTxt
  ) where

import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Data.Char
import           Data.Coerce
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.String                  (IsString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.XML.Types               hiding (Name)
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


newtype VkExtensionName = VkExtensionName { unVkExtensionName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)



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



-- * Checking type and value names

class ValidIdent a where
  isValid :: a -> Bool

instance ValidIdent VkEnumName where
  isValid = isHaskellUpperFirst . unVkEnumName


instance ValidIdent VkTypeName where
  isValid "void"         = True
  isValid "char"         = True
  isValid "float"        = True
  isValid "double"       = True
  isValid "uint8_t"      = True
  isValid "uint16_t"     = True
  isValid "uint32_t"     = True
  isValid "uint64_t"     = True
  isValid "int8_t"       = True
  isValid "int16_t"      = True
  isValid "int32_t"      = True
  isValid "int64_t"      = True
  isValid "size_t"       = True
  isValid "int"          = True
  isValid (VkTypeName n) = isHaskellUpperFirst n


firstUp :: Text -> Text
firstUp s = case T.uncons s of
  Just (a, ss) -> T.cons (toUpper a) ss
  Nothing      -> s

firstDown :: Text -> Text
firstDown s = case T.uncons s of
  Just (a, ss) -> T.cons (toLower a) ss
  Nothing      -> s

isHaskellUpperFirst :: Text -> Bool
isHaskellUpperFirst s = isHaskellIdent s && isUpper (T.head s)

isHaskellLowerFirst :: Text -> Bool
isHaskellLowerFirst s = isHaskellIdent s && not (isUpper (T.head s))

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

toHaskellName :: Coercible a Text => a -> QName ()
toHaskellName = toHaskellName' . coerce

toHaskellName' :: Text -> QName ()
toHaskellName' "void"
  = Special () (UnitCon ())
toHaskellName' "char"
  = Qual () (ModuleName () "Foreign.C.Types") (Ident () "CChar")
toHaskellName' "float"
  = UnQual () (Ident () "HSC2HS___ \"#{type float}\"")
toHaskellName' "double"
  = UnQual () (Ident () "HSC2HS___ \"#{type double}\"")
toHaskellName' "uint8_t"
  = Qual () (ModuleName () "Data.Word") (Ident () "Word8")
toHaskellName' "uint16_t"
  = Qual () (ModuleName () "Data.Word") (Ident () "Word16")
toHaskellName' "uint32_t"
  = Qual () (ModuleName () "Data.Word") (Ident () "Word32")
toHaskellName' "uint64_t"
  = Qual () (ModuleName () "Data.Word") (Ident () "Word64")
toHaskellName' "int8_t"
  = Qual () (ModuleName () "Data.Int") (Ident () "Int8")
toHaskellName' "int16_t"
  = Qual () (ModuleName () "Data.Int") (Ident () "Int16")
toHaskellName' "int32_t"
  = Qual () (ModuleName () "Data.Int") (Ident () "Int32")
toHaskellName' "int64_t"
  = Qual () (ModuleName () "Data.Int") (Ident () "Int64")
toHaskellName' "size_t"
  = UnQual () (Ident () "HSC2HS___ \"#{type size_t}\"")
toHaskellName' "int"
  = UnQual () (Ident () "HSC2HS___ \"#{type int}\"")

-- exceptions **************************************************
toHaskellName' t
  |  "wl_" `T.isPrefixOf` t || "xcb_" `T.isPrefixOf` t
    = toHaskellName' . firstUp . T.pack . toCamelCase $ T.unpack t
-- *************************************************************

toHaskellName' t
  = UnQual () (Ident () (T.unpack t))



-- | Construct a type from a qualified type name and pointer level.
--   If the type is c void and it is wrapped into a Ptr,
--     I treat it as Void.
toType :: Word -- ^ number of times pointer
       -> QName () -- ^ name of the type
       -> Type ()
toType 0 t = TyCon () t
toType n t | t == toHaskellName' "void"
             = appPtr n voidTy
           | Ident () "CChar" <- unqualify t
           , n > 0
             = toType (n-1) cstringQN
           | otherwise
             = appPtr n $ TyCon () t
  where
    appPtr 0 ty = ty
    appPtr k ty = appPtr (k-1) $ TyApp () ptrTy ty
    voidTy = TyCon () (UnQual () (Ident () "Void"))
    ptrTy  = TyCon () (UnQual () (Ident () "Ptr"))
    cstringQN = UnQual () (Ident () "CString")

unqualify :: QName a -> Name a
unqualify (Qual _ _ n)  = n
unqualify (UnQual _ n)  = n
unqualify (Special _ _) = error "unqualify: cannot unqualify special name."

unqualifyQ :: QName a -> QName a
unqualifyQ (Qual l _ n) = UnQual l n
unqualifyQ x@UnQual{}   = x
unqualifyQ x@Special{}  = x

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
