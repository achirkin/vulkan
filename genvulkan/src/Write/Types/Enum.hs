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
  , constantPattern
  ) where

import           Control.Monad
import           Control.Monad.Reader.Class
import qualified Control.Monad.Trans.RWS.Strict       as RWS
import           Data.Bits
import qualified Data.Map.Strict                      as Map
import           Data.Semigroup
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Read                       as T
import           Data.Word
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation
import           Numeric

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Enums
import           VkXml.Sections.Types

import           Write.ModuleWriter


genApiConstants :: Monad m => ModuleWriter m ()
genApiConstants = do
  glvl <- ModuleWriter $ RWS.gets currentSecLvl
  writeSection glvl "API Constants"
  vk <- ask
  pushSecLvl . const $ mapM_ genEnums
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

    unless (tname == VkTypeName "API Constants") $ do
      newInt32TypeDec tname derives tcomment
      genEnumShow tname allPNs
      genEnumRead tname allPNs

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

    allPNs = allPatNames enum
    allPatNames VkEnums {..}    = map enumPatName . items $ memberEnums
    allPatNames VkBitmasks {..} = map bitmaskPatName . items $ memberMasks
    allPatNames VkConstants {}  = []


genEnumShow :: Monad m => VkTypeName -> [Text] -> ModuleWriter m ()
genEnumShow (VkTypeName tname) xs =
    writeDecl $ parseDecl' $ T.unlines
       $  [text|instance Show $tname where|]
       : map (\x -> "  " <>
                  [text|showsPrec _ $x = showString "$x"|]
             ) xs
      ++ ["  " <> [text|showsPrec p ($tname x) = showParen (p >= 11) (showString "$tname " . showsPrec 11 x)|]
         ]


genEnumRead :: Monad m => VkTypeName -> [Text] -> ModuleWriter m ()
genEnumRead (VkTypeName tname) xs = do

    writeImport "Text.Read.Lex" $ IThingWith () (Ident () "Lexeme") [ConName () (Ident () "Ident")]
    writeImport "GHC.Read"      $ IVar () (Ident () "expectP")
    writeImport "GHC.Read"      $ IVar () (Ident () "choose")
    writeImport "Text.Read"     $ IThingAll () (Ident () "Read")
    writeImport "Text.Read"     $ IVar () (Ident () "parens")
    writeImport "Text.ParserCombinators.ReadPrec" $ IVar () (Ident () "prec")
    writeImport "Text.ParserCombinators.ReadPrec" $ IVar () (Ident () "step")
    writeImport "Text.ParserCombinators.ReadPrec" $ IVar () (Symbol () "+++")

    writeDecl $ parseDecl'
       $  [text|
            instance Read $tname where
              readPrec = parens ( choose [
          |]
       <> ( T.unlines . map ("        " <>) $ T.lines $
           T.intercalate ", " ( map (\x -> [text|("$x", pure $x)|]) xs ) <>
           [text|
                                         ] +++
                                  prec 10 (expectP (Ident "$tname") >> ($tname <$> step readPrec))
                                )
           |]
          )

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
                    Nothing   -> Just txt
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


enumPatName :: VkEnumValue -> Text
enumPatName VkEnumValue {..} = qNameTxt (toHaskellVar name)

bitmaskPatName :: VkBitmaskValue -> Text
bitmaskPatName VkBitmaskBitpos {..} = qNameTxt (toHaskellVar name)
bitmaskPatName VkBitmaskValue {..}  = qNameTxt (toHaskellVar name)
