{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
-- | WIP
module Write.Types.Struct
  ( genStruct, genUnion
  ) where


import           Control.Monad                        (unless)
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


genStruct :: Monad m => VkType -> ModuleWriter m ()
genStruct = genStructOrUnion False

genUnion :: Monad m => VkType -> ModuleWriter m ()
genUnion = genStructOrUnion True

genStructOrUnion :: Monad m => Bool -> VkType -> ModuleWriter m ()
genStructOrUnion isUnion VkTypeComposite
    { name = vkTName
    , attributes = VkTypeAttrs
        { comment = txt
        }
    , members = tmems
    }
    = do
    writePragma "MagicHash"
    writePragma "UnboxedTuples"
    writePragma "TypeFamilies"
    writePragma "UnliftedFFITypes"
    writePragma "TypeOperators"
    writePragma "DataKinds"
    writePragma "FlexibleInstances"
    writePragma "UndecidableInstances"
    writePragma "Strict"
    writePragma "FlexibleContexts"

    writeImport "Data.Void" (IAbs () (NoNamespace ()) (Ident () "Void"))
    writeImport "Foreign.Storable" (IThingAll () (Ident () "Storable"))
    writeImport "Foreign.Ptr" (IAbs () (NoNamespace ()) (Ident () "Ptr"))
    writeImport "GHC.Types" (IThingAll () (Ident () "IO"))
    writeImport "GHC.Types" (IThingAll () (Ident () "Int"))
    writeImport "GHC.Ptr" (IThingAll () (Ident () "Ptr"))
    writeImport "Foreign.C.Types" (IAbs () (NoNamespace ()) (Ident () "CChar"))
    writeImport "Foreign.C.String" (IAbs () (NoNamespace ()) (Ident () "CString"))
    writeFullImport "Data.Word"
    writeFullImport "Data.Int"
    writeFullImport "GHC.Prim"
    writeFullImport "Graphics.Vulkan.Marshal"
    writeImport "GHC.ForeignPtr" (IThingAll () (Ident () "ForeignPtr"))
    writeImport "GHC.ForeignPtr" (IThingAll () (Ident () "ForeignPtrContents"))
    writeImport "GHC.ForeignPtr" (IVar () (Ident () "newForeignPtr_"))

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

    mapM_ writeDecl
      . insertDeclComment (T.unpack tnametxt) rezComment
      $ ds

    -- generate field setters and getters
    mapM_ (uncurry $ genStructField structNameTxt tname) sfimems

    genStructShow (VkTypeName tnametxt) $ map snd sfimems

    writeExport $ EThingWith () (EWildcard () 0) tname []
  where
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
    rezComment = rezComment'' >>= preComment . T.unpack
    rezComment'' = appendComLine rezComment'
                 $ T.unlines . map ("> " <>) $ T.lines "" -- c
    rezComment' = if txt == mempty
                  then Nothing
                  else Just txt
genStructOrUnion _ t
  = error $ "genStructOrUnion: expected a type with members, "
          <> "but got: "
          <> show t

genStructShow :: Monad m => VkTypeName -> [StructFieldInfo] -> ModuleWriter m ()
genStructShow (VkTypeName tname) xs =
    writeDecl $ parseDecl'
       $  [text|
            instance Show $tname where
              showsPrec d x = showString "$tname {"
          |]
       <> "       . "
            <>  T.intercalate " . showString \", \"  . " ( map showMem xs )
            <> " . showChar '}'"
  where
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
               -> ModuleWriter m ()
genStructField structNameTxt structType _offsetE SFI{..} = do
    isMember <- isNameInScope exportName
    unless isMember genClass
    genInstance
  where
    exportName = ExportType $ unqualify className
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
      writeImport "System.IO.Unsafe" (IVar () (Ident () "unsafeDupablePerformIO"))

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

    genClass = do
      writePragma "TypeFamilies"
      writeImport "GHC.TypeLits" (IThingAll () (Ident () "ErrorMessage"))
      writeImport "GHC.TypeLits" (IAbs () (NoNamespace ()) (Ident () "TypeError"))

      let dd = ":$$:"
          ds = parseDecls [text|
            class $classNameTxt a where
                type $memberTypeTxt a :: *
                $indexFunTxt :: a -> $elemIdxArg $memberTypeTxt a
                $offsetFunTxt :: a -> Int
                $readFunTxt :: Ptr a -> $elemIdxArg IO ($memberTypeTxt a)
                $writeFunTxt :: Ptr a -> $elemIdxArg $memberTypeTxt a -> IO ()


            instance {-# OVERLAPPABLE #-}
                     TypeError
                      ( ShowType a :<>: Text " does not seem to have field $origNameTxtQ."
                        $dd Text "Check Vulkan documentation for available fields of this type."
                      ) => $classNameTxt a
            |]

      mapM_ writeDecl
        -- . insertDeclComment (T.unpack tnametxt) rezComment $
        ds

      writeExport $ EThingWith () (EWildcard () 0) className []


data StructFieldInfo
  = SFI
  { sfiSize        :: Exp ()
  , sfiElemN       :: Maybe (Exp ())
  , sfiType        :: Type ()
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
  , sfiType = t
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
    t = toType (fromIntegral $ length quals) $ toHaskellName
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
