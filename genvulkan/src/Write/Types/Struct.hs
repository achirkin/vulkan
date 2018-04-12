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


import           Control.Monad                        (when, forM_)
import           Data.Char                            (toUpper)
import           Data.Maybe                           (isJust)
import           Data.Semigroup
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Traversable                     (mapAccumL)
import           Language.Haskell.Exts.Pretty         (prettyPrint)
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation

import           VkXml.CommonTypes
import           VkXml.Sections.Types

import           Write.ModuleWriter
import           Write.Util.DeclaredNames


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
    return mempty
  else do
    writePragma "MagicHash"

    writeImport $ DIThing "Storable" DITAll
    writeImport $ DIThing "Addr#" DITNo
    writeImport $ DIThing "ByteArray#" DITNo
    writeImport $ DIVar "plusAddr#"
    writeImport $ DIVar "byteArrayContents#"
    -- writeImport $ DIThing "IO" DITAll
    -- writeImport $ DIThing "Int" DITAll
    -- writeImport $ DIThing "Ptr" DITAll
    -- writeImport $ DIThing "ForeignPtr" DITAll
    -- writeImport $ DIThing "ForeignPtrContents" DITAll
    -- writeImport $ DIVar   "newForeignPtr_"

    writeFullImport "Graphics.Vulkan.Marshal"
    writeFullImport "Graphics.Vulkan.Marshal.Internal"
    forM_ (requiresTypes structOrUnion) $ \(VkTypeName n) ->
      writeImport $ DIThing n DITNo

    let ds = parseDecls [text|
          data $tnametxt = $tnametxt# Addr# ByteArray#

          instance Eq $tnametxt where
            ($tnametxt# a _) == x@($tnametxt# b _)
              = EQ == cmpBytes# (sizeOf x) a b
            {-# INLINE (==) #-}

          instance Ord $tnametxt where
            ($tnametxt# a _) `compare` x@($tnametxt# b _)
              = cmpBytes# (sizeOf x) a b
            {-# INLINE compare #-}

          instance Storable $tnametxt where
            sizeOf ~_ = HSC2HS___ "#{size $structNameTxt}"
            {-# INLINE sizeOf #-}
            alignment ~_ = HSC2HS___ "#{alignment $structNameTxt}"
            {-# INLINE alignment #-}
            peek = peekVkData#
            {-# INLINE peek #-}
            poke = pokeVkData#
            {-# INLINE poke #-}

          instance VulkanMarshalPrim $tnametxt where
            unsafeAddr ($tnametxt# a _) = a
            {-# INLINE unsafeAddr #-}
            unsafeByteArray ($tnametxt# _ b) = b
            {-# INLINE unsafeByteArray #-}
            unsafeFromByteArrayOffset off b
              = $tnametxt# (plusAddr# (byteArrayContents# b) off) b
            {-# INLINE unsafeFromByteArrayOffset #-}

          instance VulkanMarshal $tnametxt where
            type StructFields  $tnametxt = $fieldNamesTxt
            type CUnionType    $tnametxt = $isUnionTxt
            type ReturnedOnly  $tnametxt = $returnedonlyTxt
            type StructExtends $tnametxt = $structextendsTxt
          |]


    regLink <- vkRegistryLink tnametxt
    let rezComment = appendComLine rezComment'' regLink
                 >>= preComment . T.unpack

    mapM_ writeDecl
      . insertDeclComment (T.unpack tnametxt) rezComment
      $ ds

    -- generate field setters and getters
    mapM_ (\(m, (a,b)) -> genStructField attrs structNameTxt tname m a b)
          . zip (items tmems) $  sfimems

    genStructShow (VkTypeName tnametxt) $ map snd sfimems

    writeExport tnameDeclared
  where
    returnedonlyTxt = T.pack . ('\'':) . show $ returnedonly attrs
    isUnionTxt = T.pack . ('\'':) . show $ category attrs == VkTypeCatUnion
    structextendsTxt
      = "'[" <> T.intercalate "," (unVkTypeName <$> structextends attrs) <> "]"
    fieldNamesTxt = T.pack . ('\'':) . show
                  $ map (\VkTypeMember{ name = VkMemberName n} -> n ) $ items tmems
    tnameDeclared = DIThing tnametxt DITAll
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
  }
  = do
    indeed <- isIdentDeclared anameDeclared
    writeImport anameDeclared
    writeDecl
      . setComment
        (preComment $ T.unpack [text|Alias for `$anametxt`|])
      $ parseDecl'
        [text|type $tnametxt = $anametxt|]
    writeExportExplicit (DIThing tnametxt DITAll)
      [ diToImportSpec anameDeclared
      , diToImportSpec $ DIThing tnametxt DITNo
      ]
      ( diToExportSpec (DIThing tnametxt DITNo) :
      [ diToExportSpec anameDeclared | indeed ] -- export aliased name only if from another module
      )
    writeExportExplicit (DIThing tnametxt DITNo)
      [ diToImportSpec $ DIThing tnametxt DITNo] []
    writeExportExplicit (DIThing tnametxt DITEmpty)
      [ diToImportSpec $ DIThing tnametxt DITEmpty] []
  where
    anameDeclared = DIThing anametxt DITAll
    tname = toQName vkTName
    aname = toQName vkTAlias
    tnametxt = qNameTxt tname
    anametxt = qNameTxt aname



genStructOrUnion _ t
  = error $ "genStructOrUnion: expected a type with members, "
          <> "but got: "
          <> show t





genStructShow :: Monad m => VkTypeName -> [StructFieldInfo] -> ModuleWriter m ()
genStructShow (VkTypeName tname) xs = do
    mapM_ importEnums xs
    writePragma "TypeApplications"
    writeDecl $ parseDecl'
       $  [text|
            instance Show $tname where
              showsPrec d x = showString "$tname {"
          |]
       <> "       . "
            <>  T.intercalate " . showString \", \"  . " ( map showMem xs )
            <> " . showChar '}'"
  where
    importEnums SFI { sfiTyElemN = Just en }
      = do
        writePragma "PatternSynonyms"
        writeImport $ DIPat en
    importEnums _ = pure ()
    showMem SFI{..}
      | Just en <- sfiElemN
      , ntxt <- T.pack $ prettyPrint en
        = T.strip . T.unwords $ T.lines
          [text|
           ( showString "$origNameTxt = ["
             . showsPrec d
               ( let {s = sizeOf (undefined :: FieldType $fNameQ $tname);
                      o = fieldOffset $fNameApp $tnameApp;
                      f i = peekByteOff (unsafePtr x) i :: IO (FieldType $fNameQ $tname)}
                 in unsafeDupablePerformIO
                      . mapM f
                      $ map (\i -> o + i * s )  [ 0 .. $ntxt - 1 ]
               )
             . showChar ']'
           ) |]
      | otherwise
        = T.strip [text|showString "$origNameTxt = " . showsPrec d (getField $fNameApp x)|]
      where
        VkTypeMember { name = VkMemberName origNameTxt} = sfdata
        fNameQ = "\"" <> origNameTxt <> "\""
        fNameApp = "@" <> fNameQ
        tnameApp = "@" <> tname


genStructField :: Monad m
               => VkTypeAttrs
               -> Text
               -> QName () -- ^ struct type name
               -> VkTypeMember
               -> Exp () -- ^ offset
               -> StructFieldInfo
               -> ModuleWriter m ()
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
    esizeExpr = "sizeOf (undefined :: " <> valueTypeTxt <> ")"
    fieldIsArrayT = T.pack . ('\'':) . show $ isJust sfiElemN
    fieldOptionalT = "'" <> fieldOptional
    fieldOptional = T.pack . show $ optional attributes

    specMax = case memberData of
          VkTypeData { name = Just (_, [VkTypeQArrLen n]) } -> n
          _ -> 4
    specStart t = "{-# SPECIALIZE instance " <> t <> " " <> origNameTxtQQ <> " "
    specEnd   = structTypeTxt <> " #-}"
    spec0r = if 0 >= specMax then ""
            else specStart "CanReadFieldArray" <> "0" <> specEnd
    spec1r = if 1 >= specMax then ""
            else specStart "CanReadFieldArray" <> "1" <> specEnd
    spec2r = if 2 >= specMax then ""
            else specStart "CanReadFieldArray" <> "2" <> specEnd
    spec3r = if 3 >= specMax then ""
            else specStart "CanReadFieldArray" <> "3" <> specEnd
    spec0w = if 0 >= specMax then ""
            else specStart "CanWriteFieldArray" <> "0" <> specEnd
    spec1w = if 1 >= specMax then ""
            else specStart "CanWriteFieldArray" <> "1" <> specEnd
    spec2w = if 2 >= specMax then ""
            else specStart "CanWriteFieldArray" <> "2" <> specEnd
    spec3w = if 3 >= specMax then ""
            else specStart "CanWriteFieldArray" <> "3" <> specEnd

    genInstance = do
      writePragma "TypeFamilies"
      writePragma "MultiParamTypeClasses"
      writePragma "FlexibleInstances"
      writePragma "DataKinds"
      writeImport $ DIVar "unsafeDupablePerformIO"
      writeImport $ DIThing valueTypeTxt DITNo
      writeImport $ DIThing sfiRTypeTxt DITNo
      writeImport $ DIThing structTypeTxt DITAll
      when (isJust sfiTyElemN) $ do
        writeImport $ DIThing "Proxy#" DITNo
        writeImport $ DIVar "proxy#"
      case memberData of
            VkTypeData { name = Just (_, [VkTypeQArrLenEnum n]) }
              -> writeImport $ DIThing (unVkEnumName n) DITNo
            _ -> pure ()

      let ds = parseDecls [text|
            instance {-# OVERLAPPING #-} HasField $origNameTxtQQ $structTypeTxt where
              type FieldType $origNameTxtQQ $structTypeTxt = $valueTypeTxt
              type FieldOptional $origNameTxtQQ $structTypeTxt = $fieldOptionalT
              type FieldOffset $origNameTxtQQ $structTypeTxt = $offsetExpr
              type FieldIsArray $origNameTxtQQ $structTypeTxt = $fieldIsArrayT
              {-# INLINE fieldOptional #-}
              fieldOptional = $fieldOptional
              {-# INLINE fieldOffset #-}
              fieldOffset = $offsetExpr
            |]
          dsRead = case sfiTyElemN of
            Nothing -> parseDecls [text|
              instance {-# OVERLAPPING #-} CanReadField $origNameTxtQQ $structTypeTxt where
                {-# NOINLINE getField #-}
                getField x
                  = unsafeDupablePerformIO (peekByteOff (unsafePtr x) $offsetExpr)
                {-# INLINE readField #-}
                readField p = peekByteOff p $offsetExpr
              |]
            Just lentxt -> parseDecls [text|
              instance {-# OVERLAPPING #-}
                       ( KnownNat idx
                       , IndexInBounds $origNameTxtQQ idx $structTypeTxt
                       )
                    => CanReadFieldArray $origNameTxtQQ idx $structTypeTxt where
                $spec0r
                $spec1r
                $spec2r
                $spec3r
                type FieldArrayLength $origNameTxtQQ $structTypeTxt = $lentxt
                {-# INLINE fieldArrayLength #-}
                fieldArrayLength = $lentxt
                {-# INLINE getFieldArray #-}
                getFieldArray = f
                  where
                    {-# NOINLINE f #-}
                    f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                    off = $offsetExpr + $esizeExpr * fromInteger ( natVal' (proxy# :: Proxy# idx) )
                {-# INLINE readFieldArray #-}
                readFieldArray p = peekByteOff p
                  ( $offsetExpr + $esizeExpr *
                    fromInteger ( natVal' (proxy# :: Proxy# idx) )
                  )
              |]
          dsWrite =
            if False -- returnedonly structAttrs
                     -- TODO: what to do? sometimes we need to create such structs
                     -- e.g. VkSurfaceFormatKHR
            then []
            else case sfiTyElemN of
              Nothing -> parseDecls [text|
                instance {-# OVERLAPPING #-}
                         CanWriteField $origNameTxtQQ $structTypeTxt where
                  {-# INLINE writeField #-}
                  writeField p = pokeByteOff p $offsetExpr
                |]
              Just _ -> parseDecls [text|
                instance {-# OVERLAPPING #-}
                         ( KnownNat idx
                         , IndexInBounds $origNameTxtQQ idx $structTypeTxt
                         )
                      => CanWriteFieldArray $origNameTxtQQ idx $structTypeTxt where
                  $spec0w
                  $spec1w
                  $spec2w
                  $spec3w
                  {-# INLINE writeFieldArray #-}
                  writeFieldArray p = pokeByteOff p
                    ( $offsetExpr + $esizeExpr *
                      fromInteger ( natVal' (proxy# :: Proxy# idx) )
                    )
                |]

      when (isJust sfiTyElemN) $ do
        writeImport $ DIThing "KnownNat" DITEmpty
        writeImport $ DIVar "natVal'"
        writePragma "ScopedTypeVariables"
        writePragma "FlexibleContexts"
        writePragma "UndecidableInstances"

      mapM_ writeDecl ds
      mapM_ writeDecl dsRead
      mapM_ writeDecl dsWrite



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
