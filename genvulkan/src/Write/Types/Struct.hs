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
    writeImport "Foreign.C.Types" (IAbs () (NoNamespace ()) (Ident () "CFloat"))
    writeImport "Foreign.C.Types" (IAbs () (NoNamespace ()) (Ident () "CChar"))
    writeImport "Foreign.C.Types" (IAbs () (NoNamespace ()) (Ident () "CSize"))
    writeImport "Foreign.C.Types" (IAbs () (NoNamespace ()) (Ident () "CInt"))
    writeImport "GHC.Types" (IThingAll () (Ident () "IO"))
    writeImport "GHC.Types" (IThingAll () (Ident () "Int"))
    writeImport "Data.Word" (IAbs () (NoNamespace ()) (Ident () "Word8"))
    writeImport "Data.Word" (IAbs () (NoNamespace ()) (Ident () "Word32"))
    writeImport "Data.Word" (IAbs () (NoNamespace ()) (Ident () "Word64"))
    writeImport "Data.Int"  (IAbs () (NoNamespace ()) (Ident () "Int32"))
    writeImport "GHC.Ptr" (IThingAll () (Ident () "Ptr"))
    writeFullImport "GHC.Prim"
    writeFullImport "Graphics.Vulkan.Marshal"

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
            sizeOf ~_ = $totalSizeTxt
            {-# INLINE sizeOf #-}
            alignment ~_ = sizeOf (undefined :: Ptr Void)
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
            freeze (Mutable# mba)
              | I# n <- sizeOf (undefined :: $tnametxt)
              , I# a <- alignment (undefined :: $tnametxt)
              = IO
              (\s -> case newAlignedPinnedByteArray# n a s of
                (# s1, mba' #) -> case copyMutableByteArray# mba 0# mba' 0# n s1 of
                  s2 -> case unsafeFreezeByteArray# mba' s2 of
                    (# s3, ba #) -> (# s3, $tnametxt# ba #)
              )
            {-# INLINE freeze #-}
            unsafeFreeze (Mutable# mba) = IO
              (\s -> case unsafeFreezeByteArray# mba s of
                (# s', ba #) -> (# s', $tnametxt# ba #)
              )
            {-# inline unsafeFreeze #-}
            thaw ($tnametxt# ba)
              | I# n <- sizeOf (undefined :: $tnametxt)
              , I# a <- alignment (undefined :: $tnametxt)
              = IO
              (\s -> case newAlignedPinnedByteArray# n a s of
                (# s1, mba #) -> (# copyByteArray# ba 0# mba 0# n s1
                                 ,  Mutable# mba #)
              )
            {-# inline thaw #-}
            unsafeThaw ($tnametxt# ba) = IO
              (\s -> (# s,  Mutable# (unsafeCoerce# ba) #))
            {-# inline unsafeThaw #-}

          |]

    mapM_ writeDecl
      . insertDeclComment (T.unpack tnametxt) rezComment
      $ ds

    -- generate field setters and getters
    mapM_ (uncurry $ genStructField tname) sfimems

    writeExport $ EThingWith () (EWildcard () 0) tname []
  where
    totalSizeTxt = T.pack $ prettyPrint totalSize
    (totalSize, sfimems)
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
    tname = toHaskellType vkTName
    tnametxt = qNameTxt tname
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


genStructField :: Monad m
               => QName () -- ^ struct type name
               -> Exp () -- ^ offset
               -> StructFieldInfo
               -> ModuleWriter m ()
genStructField structType offsetE SFI{..} = do
    isMember <- isNameInScope exportName
    unless isMember genClass
    genInstance
  where
    exportName = ExportType $ unqualify className
    VkTypeMember { name = VkMemberName origNameTxt} = sfdata
    origNameTxtQ = "'" <> origNameTxt <> "'"
    classNameTxt = "HasVk" <> sfiBaseNameTxt
    memberTypeTxt = "Vk" <> sfiBaseNameTxt <> "MType"
    className = toHaskellVar classNameTxt
    indexFunTxt = "vk" <> sfiBaseNameTxt
    readFunTxt = "readVk" <> sfiBaseNameTxt
    writeFunTxt = "writeVk" <> sfiBaseNameTxt
    offsetFunTxt = "vk" <> sfiBaseNameTxt <> "ByteOffset"
    structTypeTxt = qNameTxt structType
    structTypeCTxt = structTypeTxt <> "#"
    valueOffsetTxt = T.pack $ prettyPrint offsetE
    valueTypeTxt = T.pack $ prettyPrint sfiType
    elemIdxArg = case sfiElemN of
      Nothing -> ""
      Just _  -> "Int ->"
    elemIdxConstr = case sfiElemN of
      Nothing -> ""
      Just _  -> "(I# idx)"
    elemOffsetF = case sfiElemN of
      Nothing -> "o"
      Just _  -> "(idx *# _n +# o)"

    genInstance = do
      writeImport "System.IO.Unsafe" (IVar () (Ident () "unsafeDupablePerformIO"))

      let ds = parseDecls [text|
            instance {-# OVERLAPPING #-} $classNameTxt $structTypeTxt where
              type $memberTypeTxt $structTypeTxt = $valueTypeTxt
              $indexFunTxt ($structTypeCTxt ba) $elemIdxConstr
                | I# _n <- sizeOf (undefined :: $valueTypeTxt)
                , I# o <- $offsetFunTxt (undefined :: $structTypeTxt)
                = unsafeDupablePerformIO
                 (peek (Ptr (byteArrayContents# ba `plusAddr#` $elemOffsetF)))
              {-# NOINLINE $indexFunTxt #-}
              $offsetFunTxt ~_ = $valueOffsetTxt
              {-# INLINE $offsetFunTxt #-}
              $readFunTxt (Mutable# mba) $elemIdxConstr
                | I# _n <- sizeOf (undefined :: $valueTypeTxt)
                , I# o <- $offsetFunTxt (undefined :: $structTypeTxt)
                = peek (Ptr (byteArrayContents# (unsafeCoerce# mba) `plusAddr#` $elemOffsetF))
              {-# INLINE $readFunTxt #-}
              $writeFunTxt (Mutable# mba) $elemIdxConstr x
                | I# _n <- sizeOf x
                , I# o <- $offsetFunTxt (undefined :: $structTypeTxt)
                = poke (Ptr (byteArrayContents# (unsafeCoerce# mba) `plusAddr#` $elemOffsetF)) x
              {-# INLINE $writeFunTxt #-}
            |]

      mapM_ writeDecl
        -- . insertDeclComment (T.unpack tnametxt) rezComment $
        ds

    genClass = do
      writePragma "TypeFamilies"
      writeImport "GHC.TypeLits" (IThingAll () (Ident () "ErrorMessage"))
      writeImport "GHC.TypeLits" (IAbs () (NoNamespace ()) (Ident () "TypeError"))

      let dd = "':$$:"
          ds = parseDecls [text|
            class $classNameTxt a where
                type $memberTypeTxt a :: *
                $indexFunTxt :: a -> $elemIdxArg $memberTypeTxt a
                $offsetFunTxt :: a -> Int
                $readFunTxt :: Mutable a -> $elemIdxArg IO ($memberTypeTxt a)
                $writeFunTxt :: Mutable a -> $elemIdxArg $memberTypeTxt a -> IO ()


            instance {-# OVERLAPPABLE #-}
                     TypeError
                      ( 'ShowType a ':<>: 'Text " does not seem to have field $origNameTxtQ."
                        $dd 'Text "Check Vulkan documentation for available fields of this type."
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
          Just (Var () (toHaskellVar n))
        _ -> Nothing
    sname = if isJust en
            then toHaskellVar (unVkMemberName vkn <> "Array")
            else toHaskellVar vkn
    t = toType (fromIntegral $ length quals) $ toHaskellType
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
