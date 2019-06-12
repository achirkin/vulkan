{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Write.Util.DeclaredNames
  ( DeclaredNames, baseDeclaredNames
  , DeclaredIdent (..), DIThingMembers (..)
  , diToImportSpec, diToExportSpec
  , combineExportSpecs, importToExportSpec
  ) where


import           Control.DeepSeq
import           Data.List                    (unionBy)
import qualified Data.Map                     as Map
import           Data.Map.Strict              (Map)
import           Data.Semigroup
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           GHC.Generics                 (Generic)
import           Language.Haskell.Exts.Syntax



-- | I want to have unambiguous set of haskell identifiers globally for
--   the library. Thus, it should be possible to lookup module names
--   (for export or import) by identifiers.
type DeclaredNames = Map DeclaredIdent (ModuleName (), [ImportSpec ()])


-- | I limit number of ways to express imports and exports to
--    simplify further processing.
--   I think, this is a good compromise.
data DeclaredIdent
  = DIVar         { diName :: Text }
    -- ^ variable or function (starts with lower case)
  | DIPat         { diName :: Text }
    -- ^ pattern synonyms
  | DIThing       { diName :: Text, diWithMembers :: DIThingMembers}
    -- ^ Class or data or type (starts with upper case)
  deriving (Eq, Ord, Show, Generic, NFData)

data DIThingMembers = DITNo | DITEmpty | DITAll
  deriving (Eq, Ord, Show, Generic, NFData)


diToImportSpec :: DeclaredIdent -> ImportSpec ()
diToImportSpec (DIVar n)
  | T.length n >= 2 && T.head n == '(' && T.last n == ')'
    = IVar () (Symbol () . T.unpack . T.init $ T.tail n)
  | otherwise
    = IVar () (Ident () $ T.unpack n)
diToImportSpec (DIPat n)
  = IAbs () (PatternNamespace ()) (Ident () $ T.unpack n)
diToImportSpec (DIThing n DITNo)
  = IAbs () (NoNamespace ()) (Ident () $ T.unpack n)
diToImportSpec (DIThing n DITEmpty)
  = IThingWith () (Ident () $ T.unpack n) []
diToImportSpec (DIThing n DITAll)
  = IThingAll () (Ident () $ T.unpack n)


diToExportSpec :: DeclaredIdent -> ExportSpec ()
diToExportSpec (DIVar n)
  | T.length n >= 2 && T.head n == '(' && T.last n == ')'
    = EVar () (UnQual () . Symbol () . T.unpack . T.init $ T.tail n)
  | otherwise
    = EVar () (UnQual () $ Ident () $ T.unpack n)
diToExportSpec (DIPat n)
  = EAbs () (PatternNamespace ())
    (UnQual () $ Ident () $ T.unpack n)
diToExportSpec (DIThing n DITNo)
  = EAbs () (NoNamespace ())
    (UnQual () $ Ident () $ T.unpack n)
diToExportSpec (DIThing n DITEmpty)
  = EThingWith () (NoWildcard ())
    (UnQual () $ Ident () $ T.unpack n) []
diToExportSpec (DIThing n DITAll)
  = EThingWith () (EWildcard () 0)
    (UnQual () $ Ident () $ T.unpack n) []

-- | Make a union of two export specs if it is possible,
--   otherwise return nothing.
combineExportSpecs :: ExportSpec a -> ExportSpec a -> Maybe (ExportSpec a)
combineExportSpecs a@EVar {} b@EThingWith{} = combineExportSpecs b a
combineExportSpecs a@EAbs {} b@EThingWith{} = combineExportSpecs b a
combineExportSpecs (EVar l a) (EVar _ b)
    | a `eq'` b = Just $ EVar l a
combineExportSpecs (EAbs l n a) (EAbs _ m b)
    | n `eq'` m && a `eq'` b = Just $ EAbs l n a
combineExportSpecs (EThingWith l wcA nA csA) (EThingWith _ wcB nB csB)
    | nA `eq'` nB = Just $ EThingWith l (combineWildCards wcA wcB) nA (unionBy eq' csA csB)
combineExportSpecs a@(EThingWith _ _ _ csA) (EVar _ b)
    | any (eq' (eVarName b)) csA = Just a
  where
    eVarName (Qual l _ n)  = VarName l n
    eVarName (UnQual l n)  = VarName l n
    eVarName (Special l _) = VarName l (Symbol l "invalid!")
combineExportSpecs a@(EThingWith _ _ nA csA) (EAbs _ ns b)
    | PatternNamespace _ <- ns
    , any (eq' (eConName b)) csA = Just a
    | nA `eq'` b                 = Just a
  where
    eConName (Qual l _ n)  = ConName l n
    eConName (UnQual l n)  = ConName l n
    eConName (Special l _) = ConName l (Symbol l "invalid!")
combineExportSpecs a b
    | a `eq'` b = Just a
combineExportSpecs _ _ = Nothing

eq' :: (Eq (c ()), Functor c) => c a -> c b -> Bool
eq' a b = (() <$ a) == (() <$ b)

combineWildCards :: EWildcard l -> EWildcard l -> EWildcard l
combineWildCards (NoWildcard _) w                = w
combineWildCards w (NoWildcard _)                = w
combineWildCards (EWildcard l a) (EWildcard _ b) = EWildcard l (max a b)

importToExportSpec :: ImportSpec () -> ExportSpec ()
importToExportSpec (IVar l a) = EVar l (UnQual () a)
importToExportSpec (IAbs l s a) = EAbs l s (UnQual () a)
importToExportSpec (IThingAll l a) = EThingWith l (EWildcard () 0) (UnQual () a) []
importToExportSpec (IThingWith l a cs) = EThingWith l (NoWildcard ()) (UnQual () a) cs


baseDeclaredNames :: DeclaredNames
baseDeclaredNames
    = ida "Graphics.Vulkan.Marshal.Internal" "VulkanMarshalPrim"
   <> ida "Graphics.Vulkan.Marshal" "FlagType"
   <> id0 "Graphics.Vulkan.Marshal" "FlagMask"
   <> id0 "Graphics.Vulkan.Marshal" "FlagBit"
   <> ida "Graphics.Vulkan.Marshal" "VulkanMarshal"
   <> id0 "Graphics.Vulkan.Marshal" "VulkanMarshalPrim"
   <> ida "Graphics.Vulkan.Marshal" "VulkanPtr"
   <> ida "Graphics.Vulkan.Marshal" "VkPtr"
   <> ipa "Graphics.Vulkan.Marshal" "VK_NULL_HANDLE"
   <> ipa "Graphics.Vulkan.Marshal" "VK_NULL"
   <> ipa "Graphics.Vulkan.Marshal" "VK_ZERO_FLAGS"
   <> iva "Graphics.Vulkan.Marshal" "clearStorable"
   <> iva "Graphics.Vulkan.Marshal" "withPtr"
   <> ida "Graphics.Vulkan.Marshal" "HasField"
   <> ida "Graphics.Vulkan.Marshal" "CanReadField"
   <> ida "Graphics.Vulkan.Marshal" "CanWriteField"
   <> ida "Graphics.Vulkan.Marshal" "CanReadFieldArray"
   <> ida "Graphics.Vulkan.Marshal" "CanWriteFieldArray"
   <> id0 "Graphics.Vulkan.Marshal" "IndexInBounds"
   <> iva "Graphics.Vulkan.Marshal" "mallocForeignPtr"
   <> iva "Graphics.Vulkan.Marshal" "withForeignPtr"
   <> iva "Graphics.Vulkan.Marshal" "addForeignPtrFinalizer"
   <> id0 "Graphics.Vulkan.Marshal" "Int8"
   <> id0 "Graphics.Vulkan.Marshal" "Int16"
   <> id0 "Graphics.Vulkan.Marshal" "Int32"
   <> id0 "Graphics.Vulkan.Marshal" "Int64"
   <> id0 "Graphics.Vulkan.Marshal" "Word8"
   <> id0 "Graphics.Vulkan.Marshal" "Word16"
   <> id0 "Graphics.Vulkan.Marshal" "Word32"
   <> id0 "Graphics.Vulkan.Marshal" "Word64"
   <> id0 "Graphics.Vulkan.Marshal" "Ptr"
   <> id0 "Graphics.Vulkan.Marshal" "FunPtr"
   <> id0 "Graphics.Vulkan.Marshal" "Void"
   <> id0 "Graphics.Vulkan.Marshal" "CString"
   <> ida "Graphics.Vulkan.Marshal" "CInt"
   <> ida "Graphics.Vulkan.Marshal" "CSize"
   <> ida "Graphics.Vulkan.Marshal" "CChar"
   <> ida "Graphics.Vulkan.Marshal" "CWchar"
   <> ida "Graphics.Vulkan.Marshal" "CULong"
   <> iva "Graphics.Vulkan.Marshal" "withCStringField"
   <> iva "Graphics.Vulkan.Marshal" "unsafeCStringField"
   <> iva "Graphics.Vulkan.Marshal" "getStringField"
   <> iva "Graphics.Vulkan.Marshal" "readStringField"
   <> iva "Graphics.Vulkan.Marshal" "writeStringField"
   <> iva "Graphics.Vulkan.Marshal" "eqCStrings"
   <> ida "Graphics.Vulkan.Marshal.Proc" "VulkanProc"
   <> ida "Foreign.C.Types" "CFloat"
   <> iva "Foreign.Ptr" "nullPtr"
   <> id1 "GHC.Ptr" "Ptr"
   <> id0 "GHC.Generics" "Generic"
   <> id0 "Data.Data" "Data"
   <> ida "Data.Bits" "Bits"
   <> ida "Data.Bits" "FiniteBits"
   <> ida "Foreign.Storable" "Storable"
   <> iva "Text.ParserCombinators.ReadPrec" "(+++)"
   <> iva "Text.ParserCombinators.ReadPrec" "step"
   <> iva "Text.ParserCombinators.ReadPrec" "prec"
   <> iva "Text.Read" "parens"
   <> ida "Text.Read" "Read"
   <> iva "GHC.Read" "choose"
   <> iva "GHC.Read" "expectP"
   <> ida "Text.Read.Lex" "Lexeme"
   <> iva "Data.Coerce" "coerce"
   <> id1 "GHC.Base" "IO"
   <> id1 "GHC.Base" "Int"
   <> id1 "GHC.Base" "Word"
   <> id0 "GHC.Base" "Addr#"
   <> id0 "GHC.Base" "Proxy#"
   <> id0 "GHC.Base" "ByteArray#"
   <> iva "GHC.Base" "plusAddr#"
   <> iva "GHC.Base" "proxy#"
   <> iva "GHC.Base" "byteArrayContents#"
   <> ida "GHC.ForeignPtr" "ForeignPtr"
   <> ida "GHC.ForeignPtr" "ForeignPtrContents"
   <> iva "GHC.ForeignPtr" "newForeignPtr_"
   <> iva "System.IO.Unsafe" "unsafeDupablePerformIO"
   <> ida "GHC.TypeLits" "ErrorMessage"
   <> id0 "GHC.TypeLits" "TypeError"
   <> id0 "GHC.TypeLits" "KnownNat"
   <> id0 "GHC.TypeLits" "CmpNat"
   <> iva "GHC.TypeLits" "natVal'"
  where
    ida m t = Map.fromList $ withISpec
      [ (DIThing t DITAll ,  ModuleName () m)
      , (DIThing t DITEmpty, ModuleName () m)
      , (DIThing t DITNo,    ModuleName () m)
      ]
    id0 m t = Map.fromList $ withISpec
      [ (DIThing t DITEmpty, ModuleName () m)
      , (DIThing t DITNo,    ModuleName () m)
      ]
    id1 m t = Map.fromList $ withISpec
      [ (DIThing t DITAll,   ModuleName () m)
      ]
    -- ity m t x = Map.fromList [ (DIThing t x, ModuleName () m) ]
    iva m t = Map.fromList $ withISpec
      [ (DIVar t, ModuleName () m) ]
    ipa m t = Map.fromList $ withISpec
      [ (DIPat t, ModuleName () m) ]
    withISpec = map withISpec'
    withISpec' (di,m) = (di, (m, [diToImportSpec di]))
