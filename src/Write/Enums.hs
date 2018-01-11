{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
-- Generate enums and bitmasks
module Write.Enums
  ( genEnum
  ) where

import           Control.Monad
import           Control.Monad.Reader.Class
import           Data.Bits
import           Data.Semigroup
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Map.Strict           as Map
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation
import           Numeric

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Types
import           VkXml.Sections.Enums

import           Write.ModuleWriter



-- genBasetypeAlias :: Monad m => VkType -> ModuleWriter m ()
-- genBasetypeAlias t = do
--   genAlias

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
    enumPats _tn VkConstants {..} = pure ()

genAlias :: Monad m => VkType -> ModuleWriter m ()
genAlias t@VkTypeComposite{..}
    = error $ "genAlias: did not expect "
           <> show (vkTypeCat t) <> " be a composite type (VkTypeComposite)!"
genAlias VkTypeSimple
    { name = VkTypeName tname
    , attributes = VkTypeAttrs
        { comment = txt
        }
    , typeData = VkTypeData
       { reference = [(VkTypeName tref, _)]
       , comment = mtxt2
       -- , code = ccode
       }
    } = do
    writeDecl . setComment rezComment $ parseDecl'
      [text|type $tname = $tref|]

    writeExport $ EThingWith ()
                  (NoWildcard ())
                  (UnQual () $ Ident () $ T.unpack tname) []
  where
    rezComment = rezComment' >>= preComment . T.unpack
    rezComment' = if txt == mempty
                  then mtxt2
                  else case mtxt2 of
                    Nothing -> Just txt
                    Just txt2 -> appendComLine (Just txt) txt2
genAlias VkTypeSimple
    {  typeData = VkTypeData
       { reference = []
       }
    } = pure ()
genAlias t = error $ "genAlias: expected exactly one type reference!"
                  <> show t


needsBits :: VkEnums -> Bool
needsBits VkEnums {..}     = False
needsBits VkBitmasks {..}  = True
needsBits VkConstants {..} = True




newInt32TypeDec :: Monad m
                => VkTypeName -> [Text] -> Maybe Text -> ModuleWriter m ()
newInt32TypeDec (VkTypeName tname) insts com = do
    regLink <- vkRegistryLink tname
    let tinsts = T.intercalate ", " insts
        com' = appendComLine com regLink
        rezComment = com' >>= preComment . T.unpack

    writePragma "GeneralizedNewtypeDeriving"
    writeImport "Data.Int"
      $ IAbs () (NoNamespace ()) (Ident () "Int32")

    writeDecl . setComment rezComment $ parseDecl'
      [text|
        newtype $tname = $tname Int32
          deriving ($tinsts)
      |]

    writeExport $ EThingWith ()
                  (EWildcard () 0)
                  (UnQual () $ Ident () $ T.unpack tname) []



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
enumPattern (VkTypeName tname) VkEnumValue {..} = do
    writePragma "PatternSynonyms"
    writeDecl . setComment rezComment
              $ parseDecl' [text|pattern $patname :: $tname|]
    writeDecl $ parseDecl' [text|pattern $patname = $tname $patval|]
    writeExport $ EAbs ()
                  (PatternNamespace ())
                  (UnQual () (Ident () $ T.unpack patname))
  where
    patname = unVkEnumValueName name
    patval = if value < 0 then "(" <> T.pack (show value) <> ")"
                          else T.pack (show value)
    rezComment = comment >>= preComment . T.unpack
