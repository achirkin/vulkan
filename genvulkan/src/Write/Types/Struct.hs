{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
-- | WIP
module Write.Types.Struct
  ( genStruct, genUnion, genStructOrUnion, genClassName
  ) where


import           Control.Monad                        (forM_)
import           Data.Char                            (isSpace, toUpper)
import           Data.Maybe                           (fromMaybe, isJust)
import           Data.Semigroup
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Traversable                     (mapAccumL)
import           Language.Haskell.Exts.Pretty         (prettyPrint)
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation

import VkXml.CommonTypes
import VkXml.Sections.Types

import Write.ModuleWriter


genStruct :: Monad m => VkType -> ModuleWriter m ()
genStruct = genStructOrUnion False

genUnion :: Monad m => VkType -> ModuleWriter m ()
genUnion = genStructOrUnion True

genStructOrUnion :: Monad m => Bool -> VkType -> ModuleWriter m ()
genStructOrUnion isUnion structOrUnion@VkTypeComposite
    { name = vkTName
    , attributes = attrs@VkTypeAttrs
        { comment = txt
        }
    , members = tmems
    }
    = do
  indeed <- isIdentDeclared tnameDeclared
  if indeed
  then do
    writeImport tnameDeclared
    writeExport tnameDeclared
  else do

    writeFullImport "Graphics.Vulkan.Marshal"
    writeFullImport "Graphics.Vulkan.Marshal.Internal"
    forM_ (requiresTypes structOrUnion) $ \(VkTypeName n) ->
      writeImport $ DIThing n DITNo

    -- generate field descriptions VkFieldMeta
    sfields <- mapM
          (\(m, (a,b)) -> genStructField attrs structNameTxt tname m a b)
          . zip (items tmems) $  sfimems

    let allFields = "'[ (" <> T.intercalate "), (" sfields <> ") ]"
        ds = parseDecls [text|
          type $tnametxt = VkStruct $tnametxtPrime

          data $tnametxtPrime

          instance VulkanMarshal $tnametxt where
            type StructRep $tnametxt =
              'StructMeta "$tnametxt" $tnametxt
                HSC2HS___ "#{size $structNameTxt}"
                HSC2HS___ "#{alignment $structNameTxt}"
                $allFields
                $isUnionTxt $returnedonlyTxt $structextendsTxt
          |]


    regLink <- vkRegistryLink tnametxt
    let rezComment = appendComLine rezComment'' regLink
                 >>= preComment . T.unpack

    mapM_ writeDecl
      . insertDeclComment (T.unpack tnametxt) rezComment $ ds

    writeExport tnameDeclared
  where
    returnedonlyTxt = T.pack . ('\'':) . show $ returnedonly attrs
    isUnionTxt = T.pack . ('\'':) . show $ category attrs == VkTypeCatUnion
    structextendsTxt
      = "'[" <> T.intercalate "," (unVkTypeName <$> structextends attrs) <> "]"
    tnameDeclared = DIThing tnametxt DITNo
    -- tnamePrimeDeclared = DIThing tnametxtPrime DITNo
    -- totalSizeTxt = T.pack $ prettyPrint totalSize
    (_totalSize, sfimems)
            = mapAccumL (\o m -> let fi = fieldInfo m
                                 in ( InfixApp () o
                                      (QVarOp () (UnQual () sizeOp))
                                      (sfiSize fi)
                                    , (offsetF o, fi))
                        ) (Lit () (Int () 0 "0"))
                        $ items tmems
    sizeOp = if isUnion
             then Ident () "max"
             else Symbol () "+"
    offsetF = if isUnion
              then const (Lit () (Int () 0 "0"))
              else id
    tname = toQName vkTName
    tnametxt = qNameTxt tname
    tnametxtPrime = tnametxt <> "'"
    structNameTxt = unVkTypeName vkTName
    rezComment'' = appendComLine rezComment'
                 $ T.unlines . map ("> " <>) $ T.lines ccode
    rezComment' = if txt == mempty
                  then Nothing
                  else Just txt
    ccode = cstype <> tnametxt <> " {"
        <> T.unlines (("":) . map (\x -> "    " <> code (memberData x) <> ";")
                     $ items tmems)
        <> "} " <> tnametxt <> ";"
      where
        cstype = if isUnion
                 then "typedef union "
                 else "typedef struct "


genStructOrUnion _ VkTypeSimple
  { name = vkTName
  , attributes = VkTypeAttrs
      { alias = Just vkTAlias
      }
  } = do
  indeed <- isIdentDeclared tnameDeclared
  if indeed
  then do
    writeImport tnameDeclared
    writeExport tnameDeclared
  else do
    writeImport anameDeclared
    writeDecl
      . setComment
        (preComment $ T.unpack [text|Alias for `$anametxt`|])
      $ parseDecl'
        [text|type $tnametxt = $anametxt|]
    writeExport tnameDeclared
  where
    anameDeclared = DIThing anametxt DITNo
    tnameDeclared = DIThing tnametxt DITNo
    tname = toQName vkTName
    aname = toQName vkTAlias
    tnametxt = qNameTxt tname
    anametxt = qNameTxt aname


genStructOrUnion _ t
  = error $ "genStructOrUnion: expected a type with members, "
          <> "but got: "
          <> show t



genStructField :: Monad m
               => VkTypeAttrs
               -> Text
               -> QName () -- ^ struct type name
               -> VkTypeMember
               -> Exp () -- ^ offset
               -> StructFieldInfo
               -> ModuleWriter m Text
genStructField _structAttrs structNameTxt structType VkTypeMember{..} _offsetE SFI{..}
    = genInstance
  where
    -- exportName = ExportType $ unqualify className
    VkTypeMember { name = VkMemberName origNameTxt} = sfdata
    origNameTxtQQ = "\"" <> origNameTxt <> "\""
    structTypeTxt = qNameTxt structType
    -- structTypeCTxt = structTypeTxt <> "#"
    -- valueOffsetTxt = T.pack $ prettyPrint offsetE
    valueTypeTxt = T.pack $ prettyPrint sfiType
    offsetExpr = "HSC2HS___ \"#{offset " <> structNameTxt
                                 <> ", " <> origNameTxt <> "}\""
    fieldOptionalT = "'" <> fieldOptional
    fieldOptional = T.pack . show $ optional attributes

    genInstance = do
      writePragma "TypeFamilies"
      writePragma "MultiParamTypeClasses"
      writePragma "FlexibleInstances"
      writePragma "DataKinds"
      -- writeImport $ DIVar "unsafeDupablePerformIO"
      writeImport $ DIThing valueTypeTxt DITNo
      writeImport $ DIThing sfiRTypeTxt DITNo
      writeImport $ DIThing structTypeTxt DITAll
      -- when (isJust sfiTyElemN) $ do
      --   writeImport $ DIThing "Proxy#" DITNo
      --   writeImport $ DIVar "proxy#"
      case memberData of
            VkTypeData { name = Just (_, [VkTypeQArrLenEnum n]) }
              -> writeImport $ DIThing (unVkEnumName n) DITNo
            _ -> pure ()

      let len1 = fromMaybe "1" sfiTyElemN
          valueTypeTxtParen = if T.any isSpace valueTypeTxt
                              then "(" <> valueTypeTxt <> ")" else valueTypeTxt
          spec = T.unwords
            ["'FieldMeta"
            , origNameTxtQQ  -- fieldName
            , valueTypeTxtParen   -- fieldType
            , fieldOptionalT -- optional
            , offsetExpr     -- byteOffset
            , len1           -- length (for arrays, 1 otherwise)
            , "'True"        -- canRead
            , "'True"        -- canWrite
            ]
      return spec



genClassName :: Monad m => (QName (), [Decl A]) -> ModuleWriter m ()
genClassName (className, decls) = do
  writePragma "TypeFamilies"
  writePragma "FlexibleContexts"
  writePragma "FlexibleInstances"
  writePragma "UndecidableInstances"
  writePragma "DataKinds"
  writePragma "TypeOperators"
  writeImport $ DIThing "ErrorMessage" DITAll
  writeImport $ DIThing "TypeError" DITNo
  mapM_ writeDecl decls
  writeExport $ DIThing (qNameTxt className) DITAll


-- TODO: the following needs a major rework, since I moved to use hsc2hs
--         to determine data sizes, alignments, and offsets.
data StructFieldInfo
  = SFI
  { sfiSize        :: Exp ()
  , sfiElemN       :: Maybe (Exp ())
  , sfiTyElemN     :: Maybe Text
  , sfiType        :: Type ()
  , sfiRTypeTxt    :: Text
  , sfiName        :: QName ()
  , sfiBaseNameTxt :: Text
  , sfdata         :: VkTypeMember
  }

fieldInfo :: VkTypeMember -> StructFieldInfo
fieldInfo tm@VkTypeMember
  { name = vkn
  , memberData = VkTypeData
    { name = mvkn
    , reference = [(vkt, quals)]
    }
  } = SFI
  { sfiSize = case en of
      Just n  -> InfixApp () n (QVarOp () (UnQual () (Symbol () "*"))) uSize
      Nothing -> uSize
  , sfiElemN = en
  , sfiTyElemN = case mvkn of
      Just (_, [VkTypeQArrLen n]) ->
        Just (T.pack $ show n)
      Just (_, [VkTypeQArrLenEnum n]) ->
        Just (unVkEnumName n)
      _ -> Nothing
  , sfiType = t
  , sfiRTypeTxt = unVkTypeName vkt
  , sfiBaseNameTxt = case unqualify sname of
      Ident _ x -> case T.uncons $ T.pack x of
         Just (s, ss) -> T.cons (toUpper s) ss
         Nothing      -> T.pack x
      _ -> error "Struct fieldInfo sfiBaseNameTxt: expected Ident name"
  , sfiName = sname
  , sfdata  = tm
  }
  where
    en = case mvkn of
        Just (_, [VkTypeQArrLen n]) ->
          Just (Lit () (Int () (fromIntegral n) $ show n))
        Just (_, [VkTypeQArrLenEnum n]) ->
          Just (Var () (toQName n))
        _ -> Nothing
    sname = if isJust en
            then toQName (unVkMemberName vkn <> "Array")
            else toQName vkn
    t = toType (fromIntegral $ length quals) vkt
    uSize = App ()
        (Var () (UnQual () (Ident () "sizeOf")))
        (Paren ()
          (ExpTypeSig ()
            (Var () (UnQual () (Ident () "undefined")))
            t
          )
        )
fieldInfo tm = error
             $ "Struct fieldInfo: expected VkTypeMember with single ref"
             <> " but got: " <> show tm
