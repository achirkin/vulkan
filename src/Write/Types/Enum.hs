{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
-- Generate enums and bitmasks
module Write.Types.Enum
  ( genEnum
  , genAlias
  , genApiConstants
  ) where

import           Control.Monad
import           Control.Monad.Reader.Class
import           Data.Bits
import           Data.Word
import           Data.Semigroup
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Map.Strict           as Map
import qualified Data.Text.Read as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation
import           Numeric

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Types
import           VkXml.Sections.Enums

import           Write.ModuleWriter

genApiConstants :: Monad m => ModuleWriter m ()
genApiConstants = ask >>= \vk -> mapM_ genEnums
  (unInorder <$> Map.lookup (VkTypeName "API Constants") (globEnums vk))


-- | Lookup an enum in vk.xml and generate code for it
genEnum :: Monad m => VkType -> ModuleWriter m ()
genEnum t = ask >>= \vk -> case unInorder <$> Map.lookup tname (globEnums vk) of
      Nothing -> genAlias t
      Just e  -> genEnums e
  where
    tname = (name :: VkType -> VkTypeName) t

genEnums :: Monad m => VkEnums -> ModuleWriter m ()
genEnums enum = do
    writeImport "Foreign.Storable"
      $ IAbs () (NoNamespace ()) (Ident () "Storable")
    when nb $ writeImport "Data.Bits"
      $ IAbs () (NoNamespace ()) (Ident () "Bits")

    unless (tname == VkTypeName "API Constants")
      $ newInt32TypeDec tname derives tcomment

    enumPats tname enum
  where
    nb = needsBits enum
    derives = if nb then ["Eq","Ord","Bits","Storable"]
                    else ["Eq","Ord","Storable"]
    tcomment = (comment :: VkEnums -> Maybe Text) enum
    tname = (name :: VkEnums -> VkTypeName) enum
    enumPats tn VkEnums {..}
      = writeSections (enumPattern tn) memberEnums
    enumPats tn VkBitmasks {..}
      = writeSections (bitmaskPattern tn) memberMasks
    enumPats _ VkConstants {..}
      = writeSections constantPattern memberConsts

genAlias :: Monad m => VkType -> ModuleWriter m ()
genAlias t@VkTypeComposite{..}
    = error $ "genAlias: did not expect "
           <> show (vkTypeCat t) <> " being a composite type (VkTypeComposite)!"
genAlias VkTypeSimple
    { name = vkTName
    , attributes = VkTypeAttrs
        { comment = txt
        }
    , typeData = VkTypeData
       { reference = [(vkTRef, _)]
       , comment = mtxt2
       -- , code = ccode
       }
    } = do
    writeDecl . setComment rezComment $ parseDecl'
      [text|type $tnametxt = $treftxt|]

    writeExport $ EAbs () (NoNamespace ()) tname
  where
    tname = toHaskellType vkTName
    tnametxt = qNameTxt tname
    treftxt = qNameTxt $ toHaskellType vkTRef
    rezComment = rezComment' >>= preComment . T.unpack
    rezComment' = if txt == mempty
                  then mtxt2
                  else case mtxt2 of
                    Nothing -> Just txt
                    Just txt2 -> appendComLine (Just txt) txt2
genAlias t@VkTypeSimple
    { typeData = td@VkTypeData
       { reference = []
       }
    } = genAlias t
     { typeData = td { reference = [("VkFlags", [])] } }
genAlias t = error $ "genAlias: expected a simple enum type, but got: "
                  <> show t


needsBits :: VkEnums -> Bool
needsBits VkEnums {..}     = False
needsBits VkBitmasks {..}  = True
needsBits VkConstants {..} = False




newInt32TypeDec :: Monad m
                => VkTypeName -> [Text] -> Maybe Text -> ModuleWriter m ()
newInt32TypeDec vkTName insts com = do
    regLink <- vkRegistryLink $ unVkTypeName vkTName
    let tinsts = T.intercalate ", " insts
        com' = appendComLine com regLink
        rezComment = com' >>= preComment . T.unpack

    writePragma "GeneralizedNewtypeDeriving"
    writeDecl . setComment rezComment $ parseDecl'
      [text|
        newtype $tnametxt = $tnametxt VkFlags
          deriving ($tinsts)
      |]

    writeExport $ EThingWith () (EWildcard () 0) tname []
  where
    tname = toHaskellType vkTName
    tnametxt = qNameTxt tname


constantPattern :: Monad m => VkConstant -> ModuleWriter m ()
constantPattern vkc@VkConstant {..}
  | T.null value' = error $ "constantPattern: empty value! " <> show vkc
  | Right (v, "") <- T.signed T.decimal value'
  , vtxt <- T.pack $ show (v :: Integer) = go vtxt "(Num a, Eq a) => a"
  | Right (v, "f") <- T.signed T.double value'
  , vtxt <- T.pack $ show v = go vtxt "(Fractional a, Eq a) => a"
  | "(~0U)" <- value'
  , vtxt <- T.pack $ show (complement 0 :: Word32) = go vtxt "Word32"
  | "(~0ULL)" <- value'
  , vtxt <- T.pack $ show (complement 0 :: Word64) = go vtxt "Word64"
  | "(~0U-1)" <- value'
  , vtxt <- T.pack $ show (complement 0 - 1 :: Word32) = go vtxt "Word32"
  | "(~0U-2)" <- value'
  , vtxt <- T.pack $ show (complement 0 - 2 :: Word32) = go vtxt "Word32"
  | otherwise = error $ "constantPattern: value " <> show value'
                    <> " is not parsed! "
                    <> " Please, add a new case in Write.Types.Enums.constantPattern. "
                    <> show vkc
  where
    value' = T.strip value
    patname = toHaskellVar name
    patnametxt = qNameTxt patname
    rezComment = comment >>= preComment . T.unpack
    go tval ttype = do
      writePragma "PatternSynonyms"
      writeDecl . setComment rezComment $ parseDecl'
        [text|pattern $patnametxt :: $ttype|]
      writeDecl $ parseDecl'
        [text|pattern $patnametxt = $tval|]
      writeExport $ EAbs () (PatternNamespace ()) patname




bitmaskPattern :: Monad m => VkTypeName -> VkBitmaskValue -> ModuleWriter m ()
bitmaskPattern tname VkBitmaskBitpos {..}
    = enumPattern tname $ VkEnumValue name c v
  where
    v = shiftL 1 (fromIntegral bitpos)
    c = appendComLine comment
      $ "Bit position @" <> T.pack (show bitpos) <> "@"
bitmaskPattern tname VkBitmaskValue {..}
    = enumPattern tname $ VkEnumValue name c value
  where
    c = appendComLine comment
      $ "Bitmask value @0x" <> T.pack (showHex value "@")


enumPattern :: Monad m => VkTypeName -> VkEnumValue -> ModuleWriter m ()
enumPattern vkTName VkEnumValue {..} = do
    writePragma "PatternSynonyms"
    writeDecl . setComment rezComment
              $ parseDecl' [text|pattern $patnametxt :: $tnametxt|]
    writeDecl $ parseDecl' [text|pattern $patnametxt = $tnametxt $patval|]
    writeExport $ EAbs () (PatternNamespace ()) patname
  where
    tname = toHaskellType vkTName
    tnametxt = qNameTxt tname
    patname = toHaskellVar name
    patnametxt = qNameTxt patname
    patval = if value < 0 then "(" <> T.pack (show value) <> ")"
                          else T.pack (show value)
    rezComment = comment >>= preComment . T.unpack
