{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
-- | WIP
module Write.Types.Struct
  ( genStruct, genUnion, genClassName
  , ClassDeclarations
  ) where


import           Data.Char                            (toUpper)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
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


type ClassDeclarations = Map (QName ()) [Decl A]

genStruct :: Monad m => VkType -> ModuleWriter m ClassDeclarations
genStruct = genStructOrUnion False

genUnion :: Monad m => VkType -> ModuleWriter m ClassDeclarations
genUnion = genStructOrUnion True

genStructOrUnion :: Monad m => Bool -> VkType -> ModuleWriter m ClassDeclarations
genStructOrUnion isUnion VkTypeComposite
    { name = vkTName
    , attributes = VkTypeAttrs
        { comment = txt
        }
    , members = tmems
    }
    = do
  indeed <- isIdentDeclared tnameDeclared
  if indeed
  then do
    writeImport tnameDeclared
    writeExportNoScope tnameDeclared
    return mempty
  else do
    writePragma "MagicHash"
    writePragma "UnboxedTuples"

    writeImport $ DIThing "Storable" DITAll
    writeImport $ DIThing "IO" DITAll
    writeImport $ DIThing "Int" DITAll
    writeImport $ DIThing "Ptr" DITAll
    writeImport $ DIThing "ForeignPtr" DITAll
    writeImport $ DIThing "ForeignPtrContents" DITAll
    writeImport $ DIVar   "newForeignPtr_"

    writeFullImport "Graphics.Vulkan.StructMembers"
    writeFullImport "GHC.Prim"
    writeFullImport "Graphics.Vulkan.Marshal"
    writeFullImport "Graphics.Vulkan.Marshal.Internal"

    let ds = parseDecls [text|
          data $tnametxt = $tnametxt# ByteArray#

          instance Eq $tnametxt where
            ($tnametxt# a) == ($tnametxt# b)
              = EQ == cmpImmutableContent a b
            {-# INLINE (==) #-}

          instance Ord $tnametxt where
            ($tnametxt# a) `compare` ($tnametxt# b)
              = cmpImmutableContent a b
            {-# INLINE compare #-}

          instance Storable $tnametxt where
            sizeOf ~_ = HSC2HS___ "#{size $structNameTxt}"
            {-# INLINE sizeOf #-}
            alignment ~_ = HSC2HS___ "#{alignment $structNameTxt}"
            {-# INLINE alignment #-}
            peek (Ptr addr)
              | I# n <- sizeOf (undefined :: $tnametxt)
              , I# a <- alignment (undefined :: $tnametxt)
              = IO
              (\s -> case newAlignedPinnedByteArray# n a s of
                (# s1, mba #) -> case copyAddrToByteArray# addr mba 0# n s1 of
                  s2 -> case unsafeFreezeByteArray# mba s2 of
                    (# s3, ba #) -> (# s3, $tnametxt# ba #)
              )
            {-# INLINE peek #-}
            poke (Ptr addr) ($tnametxt# ba)
              | I# n <- sizeOf (undefined :: $tnametxt)
              = IO (\s -> (# copyByteArrayToAddr# ba 0# addr n s, () #))
            {-# INLINE poke #-}

          instance VulkanMarshal $tnametxt where
            {-# INLINE newVkData #-}
            newVkData f
              | I# n <- sizeOf (undefined :: $tnametxt)
              , I# a <- alignment (undefined :: $tnametxt)
              = IO
              (\s0 -> case newAlignedPinnedByteArray# n a s0 of
                (# s1, mba #) -> case unsafeFreezeByteArray# mba s1 of
                  (# s2, ba #) -> case f (Ptr (byteArrayContents# ba)) of
                    IO k -> case k s2 of
                      (# s3, () #) -> (# s3, $tnametxt# ba #)
              )
            {-# INLINE unsafePtr #-}
            unsafePtr ($tnametxt# ba) = Ptr (byteArrayContents# ba)
            {-# INLINE fromForeignPtr #-}
            fromForeignPtr = fromForeignPtr# $tnametxt#
            {-# INLINE toForeignPtr #-}
            toForeignPtr ($tnametxt# ba) = do
              ForeignPtr addr (PlainForeignPtr r)
                <- newForeignPtr_ (Ptr (byteArrayContents# ba))
              IO (\s -> (# s, ForeignPtr addr (MallocPtr (unsafeCoerce# ba) r) #))
            {-# INLINE toPlainForeignPtr #-}
            toPlainForeignPtr ($tnametxt# ba) = IO
              (\s -> (# s, ForeignPtr (byteArrayContents# ba) (PlainPtr (unsafeCoerce# ba)) #))
            {-# INLINE touchVkData #-}
            touchVkData x@($tnametxt# ba)
              = IO (\s -> (# touch# x (touch# ba s), () #))
          |]


    regLink <- vkRegistryLink tnametxt
    let rezComment = appendComLine rezComment'' regLink
                 >>= preComment . T.unpack

    mapM_ writeDecl
      . insertDeclComment (T.unpack tnametxt) rezComment
      $ ds

    -- generate field setters and getters
    classDefs <- Map.fromList
      <$> mapM (uncurry $ genStructField structNameTxt tname) sfimems

    genStructShow (VkTypeName tnametxt) $ map snd sfimems

    writeExport tnameDeclared
    return classDefs
  where
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
    tname = toHaskellName vkTName
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

genStructOrUnion _ t
  = error $ "genStructOrUnion: expected a type with members, "
          <> "but got: "
          <> show t



genStructShow :: Monad m => VkTypeName -> [StructFieldInfo] -> ModuleWriter m ()
genStructShow (VkTypeName tname) xs = do
    mapM_ importEnums xs
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
        = T.strip [text|showString "$indexFunTxt = [" . showsPrec d (map ($indexFunTxt x) [ 1 .. $ntxt ]) . showChar ']'|]
      | otherwise
        = T.strip [text|showString "$indexFunTxt = " . showsPrec d ($indexFunTxt x)|]
      where
        indexFunTxt = "vk" <> sfiBaseNameTxt



genStructField :: Monad m
               => Text
               -> QName () -- ^ struct type name
               -> Exp () -- ^ offset
               -> StructFieldInfo
               -> ModuleWriter m (QName (), [Decl A])
genStructField structNameTxt structType _offsetE SFI{..}
    = genClass <$ genInstance
  where
    -- exportName = ExportType $ unqualify className
    VkTypeMember { name = VkMemberName origNameTxt} = sfdata
    origNameTxtQ = "'" <> origNameTxt <> "'"
    classNameTxt = "HasVk" <> sfiBaseNameTxt
    memberTypeTxt = "Vk" <> sfiBaseNameTxt <> "MType"
    className = toHaskellName classNameTxt
    indexFunTxt = "vk" <> sfiBaseNameTxt
    readFunTxt = "readVk" <> sfiBaseNameTxt
    writeFunTxt = "writeVk" <> sfiBaseNameTxt
    offsetFunTxt = "vk" <> sfiBaseNameTxt <> "ByteOffset"
    structTypeTxt = qNameTxt structType
    -- structTypeCTxt = structTypeTxt <> "#"
    -- valueOffsetTxt = T.pack $ prettyPrint offsetE
    valueTypeTxt = T.pack $ prettyPrint sfiType
    offsetExpr = "HSC2HS___ \"#{offset " <> structNameTxt
                                 <> ", " <> origNameTxt <> "}\""
    elemIdxArg = case sfiElemN of
      Nothing -> ""
      Just _  -> "Int ->"
    elemIdxConstr = case sfiElemN of
      Nothing -> ""
      Just _  -> "idx"
    elemOffsetF = case sfiElemN of
      Nothing -> offsetExpr
      Just _  -> "(idx * sizeOf (undefined :: " <> valueTypeTxt <> ")"
               <> " + " <> offsetExpr <> ")"

    genInstance = do
      writePragma "TypeFamilies"
      writeImport $ DIVar "unsafeDupablePerformIO"
      writeImport $ DIThing valueTypeTxt DITNo
      writeImport $ DIThing sfiRTypeTxt DITNo
      writeImport $ DIThing structTypeTxt DITAll

      let ds = parseDecls [text|
            instance {-# OVERLAPPING #-} $classNameTxt $structTypeTxt where
              type $memberTypeTxt $structTypeTxt = $valueTypeTxt
              {-# NOINLINE $indexFunTxt #-}
              $indexFunTxt x $elemIdxConstr
                = unsafeDupablePerformIO (peekByteOff (unsafePtr x) $elemOffsetF)
              {-# INLINE $offsetFunTxt #-}
              $offsetFunTxt ~_ = $offsetExpr
              {-# INLINE $readFunTxt #-}
              $readFunTxt p $elemIdxConstr = peekByteOff p $elemOffsetF
              {-# INLINE $writeFunTxt #-}
              $writeFunTxt p $elemIdxConstr = pokeByteOff p $elemOffsetF
            |]

      mapM_ writeDecl
        -- . insertDeclComment (T.unpack tnametxt) rezComment $
        ds

    genClass =
      ( className
      , parseDecls [text|
            class $classNameTxt a where
                type $memberTypeTxt a :: *
                $indexFunTxt :: a -> $elemIdxArg $memberTypeTxt a
                $offsetFunTxt :: a -> Int
                $readFunTxt :: Ptr a -> $elemIdxArg IO ($memberTypeTxt a)
                $writeFunTxt :: Ptr a -> $elemIdxArg $memberTypeTxt a -> IO ()


            instance {-# OVERLAPPABLE #-}
                     TypeError
                      ( 'ShowType a ':<>: 'Text " does not seem to have field $origNameTxtQ."
                        $dd 'Text "Check Vulkan documentation for available fields of this type."
                      ) => $classNameTxt a
            |]
      ) where dd = "':$$:"


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
  , sfiRTypeTxt = unVkMemberName vkt
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
          Just (Var () (toHaskellName n))
        _ -> Nothing
    sname = if isJust en
            then toHaskellName (unVkMemberName vkn <> "Array")
            else toHaskellName vkn
    t = toType (fromIntegral $ length quals) $ unqualifyQ $ toHaskellName
      $ VkTypeName $ unVkMemberName vkt
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
