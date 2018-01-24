{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
-- | WIP
module Write.Types.Struct
  ( genStruct, genUnion
  ) where


import           Data.Char                            (toUpper)
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


    writeImport "GHC.TypeLits" (IThingAll () (Ident () "ErrorMessage"))
    writeImport "GHC.TypeLits" (IAbs () (NoNamespace ()) (Ident () "TypeError"))
    writeImport "Data.Void" (IAbs () (NoNamespace ()) (Ident () "Void"))
    writeImport "Foreign.Storable" (IThingAll () (Ident () "Storable"))
    -- writeImport "Foreign.C.Types" (IThingAll ()  (Ident () "CInt"))
    -- writeImport "Foreign.C.Types" (IThingAll ()  (Ident () "CSize"))
    writeImport "GHC.Types" (IThingAll () (Ident () "IO"))
    writeImport "GHC.Types" (IThingAll () (Ident () "Int"))
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

    writeExport $ EThingWith () (EWildcard () 0) tname []
  where
    totalSizeTxt = T.pack $ prettyPrint totalSize
    (totalSize, _sfimems)
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


data StructFieldInfo
  = SFI
  { sfiSize        :: Exp ()
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
  { sfiSize = case mvkn of
      Just (_, [VkTypeQArrLen n]) ->
        InfixApp () (Lit () (Int () (fromIntegral n) $ show n))
          (QVarOp () (UnQual () (Symbol () "*")))
          uSize
      Just (_, [VkTypeQArrLenEnum en]) ->
        InfixApp () (Var () (toHaskellVar en))
          (QVarOp () (UnQual () (Symbol () "*")))
          uSize
      _ -> uSize
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
    sname = toHaskellVar vkn
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

-- _structCommon :: Monad m => ModuleWriter m ()
-- _structCommon = do
--   writePragma "MagicHash"
--   writePragma "UnboxedTuples"
--   writePragma "TypeFamilies"
--   writePragma "UnliftedFFITypes"
--   writePragma "TypeOperators"
--   writePragma "DataKinds"
--   writePragma "FlexibleInstances"
--   writePragma "UndecidableInstances"
--
--   writeImport "GHC.TypeLits" (IThingAll () (Ident () "ErrorMessage"))
--   writeImport "GHC.TypeLits" (IAbs () (NoNamespace ()) (Ident () "TypeError"))
--   writeImport "Data.Void" (IAbs () (NoNamespace ()) (Ident () "Void"))
--   writeImport "Foreign.Storable" (IThingAll () (Ident () "Storable"))
--   writeImport "Foreign.C.Types" (IThingAll ()  (Ident () "CInt"))
--   writeImport "Foreign.C.Types" (IThingAll ()  (Ident () "CSize"))
--   writeImport "GHC.Types" (IThingAll () (Ident () "IO"))
--   writeImport "GHC.Types" (IThingAll () (Ident () "Int"))
--   writeImport "GHC.Base" (IVar () (Ident () "runRW#"))
--   writeImport "GHC.Base" (IVar () (Ident () "isTrue#"))
--   writeImport "GHC.Ptr" (IThingAll () (Ident () "Ptr"))
--   writeFullImport "GHC.Prim"



--
-- data VkShaderModuleCreateInfo
--   = VkShaderModuleCreateInfo# ByteArray#
-- data VkDescriptorSetLayoutCreateInfo
--   = VkDescriptorSetLayoutCreateInfo# ByteArray#
--
--
--
-- instance Eq VkShaderModuleCreateInfo where
--   (VkShaderModuleCreateInfo# a) == (VkShaderModuleCreateInfo# b)
--     = EQ == cmpByteArrays a b
--   {-# INLINE (==) #-}
--
-- instance Ord VkShaderModuleCreateInfo where
--   (VkShaderModuleCreateInfo# a) `compare` (VkShaderModuleCreateInfo# b)
--     = cmpByteArrays a b
--   {-# INLINE compare #-}
--
-- class HasVkPNext a where
--   vkPNext :: a -> Ptr Void
--   readVkPNext :: Mutable a -> IO (Ptr Void)
--   writeVkPNext :: Mutable a -> Ptr Void -> IO ()
--
-- instance {-# OVERLAPPABLE #-}
--          TypeError
--       ( 'ShowType a ':<>: 'Text " does not seem to have field \"pNext\"."
--   ':$$: 'Text "Check Vulkan documentation for available fields of this type."
--       ) => HasVkPNext a where
--   vkPNext _ = error "This type does not have field \"pNext\""
--   readVkPNext _ = error "This type does not have field \"pNext\""
--   writeVkPNext _ _ = error "This type does not have field \"pNext\""
--
--
--
-- -- Have to convert bytearrays to Addr# to index non-multiples of
-- -- element size.
-- -- Waiting for <https://ghc.haskell.org/trac/ghc/ticket/11143 11143>
-- instance {-# OVERLAPPING #-}
--          HasVkPNext VkShaderModuleCreateInfo where
--   vkPNext (VkShaderModuleCreateInfo# ba)
--     = Ptr (indexAddrOffAddr# (plusAddr# (byteArrayContents# ba) 4#) 0#)
--   {-# INLINE vkPNext #-}
--   readVkPNext (Mutable# mba) = IO
--     (\s -> case readAddrOffAddr#  (plusAddr#
--                   (byteArrayContents# (unsafeCoerce# mba)) 4# ) 0# s of
--       (# s', a #) -> (# s', Ptr a #)
--     )
--   {-# INLINE readVkPNext #-}
--   writeVkPNext (Mutable# mba) (Ptr a) = IO
--     (\s -> case writeAddrOffAddr#  (plusAddr#
--                   (byteArrayContents# (unsafeCoerce# mba)) 4# ) 0# a s of
--       s' -> (# s', () #)
--     )
--   {-# INLINE writeVkPNext #-}
