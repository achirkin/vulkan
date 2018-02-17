{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
module Write.Types
  ( genBaseTypes, genType
  , genBaseStructs
  ) where

import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.Reader.Class
import qualified Control.Monad.Trans.RWS.Strict       as RWS
import           Control.Monad.Trans.State.Strict     (StateT)
import qualified Control.Monad.Trans.State.Strict     as State
import           Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Types
import           VkXml.Sections.Feature
import           VkXml.Sections.Extensions

import           Write.ModuleWriter
import           Write.Types.Define
import           Write.Types.Enum
import           Write.Types.Funcpointer
import           Write.Types.Handle
import           Write.Types.Struct

genBaseTypes :: Monad m => ModuleWriter m ()
genBaseTypes = hoist (`State.evalStateT` Nothing) genBaseTypes'


genBaseTypes' :: Monad m => ModuleWriter (StateT (Maybe VkTypeCategory) m) ()
genBaseTypes' = do
    vkXml <- ask
    glvl <- ModuleWriter $ RWS.gets currentSecLvl
    writeSection glvl "Types and enumerations"
    pushSecLvl $ \curlvl ->
      foldSectionsWithComments (fItem curlvl) fLast
                               (types $ globTypes vkXml)
        where
          fItem curlvl cs t = do
            oldcat <- lift State.get
            let curcat = vkTypeCat t
            when (oldcat /= Just curcat) $ do
              lift . State.put $ Just curcat
              case curcat of
                VkTypeNoCat          -> writeSection curlvl "External types"
                VkTypeCatInclude     -> return ()
                VkTypeCatDefine      -> writeSection curlvl "Define pragmas"
                VkTypeCatBasetype    -> writeSection curlvl "Base types"
                VkTypeCatBitmask     -> writeSection curlvl "Bitmasks"
                VkTypeCatHandle      -> writeSection curlvl "Handles"
                VkTypeCatEnum        -> writeSection curlvl "Enums"
                VkTypeCatFuncpointer -> writeSection curlvl "Function pointers"
                VkTypeCatStruct      -> return ()
                VkTypeCatUnion       -> return ()
            forM_ cs $ writeSection (curlvl+1)
            case vkTypeCat t of
              VkTypeNoCat          -> genNocatData t
              VkTypeCatInclude     -> return ()
              VkTypeCatDefine      -> genDefine t
              VkTypeCatBasetype    -> genBasetypeAlias t
              VkTypeCatBitmask     -> genEnum t
              VkTypeCatHandle      -> genHandle t
              VkTypeCatEnum        -> genEnum t
              VkTypeCatFuncpointer -> genFuncpointer t
              VkTypeCatStruct      -> return ()
              VkTypeCatUnion       -> return ()
          fLast [] = pure ()
          fLast cs = writeSection 0 $ T.unlines $ "|":cs


genBaseStructs :: Monad m => ModuleWriter m ClassDeclarations
genBaseStructs = do
    vkXml <- ask
    let featureTypes = Set.fromList
                     . join
                     . map requireTypes
                     . reqList $ globFeature vkXml
        extTypes = Set.fromList
                      $ extensions (globExtensions vkXml)
                          >>= extRequires >>= requireTypes
        excludedTypes = Set.union featureTypes extTypes

    fmap mconcat
      $ forM (items . types $ globTypes vkXml) $ \t ->
        if (name :: VkType -> VkTypeName) t `Set.member` excludedTypes
        then pure mempty
        else case vkTypeCat t of
          VkTypeNoCat          -> return mempty
          VkTypeCatInclude     -> mempty <$ genInclude t
          VkTypeCatDefine      -> return mempty
          VkTypeCatBasetype    -> return mempty
          VkTypeCatBitmask     -> return mempty
          VkTypeCatHandle      -> return mempty
          VkTypeCatEnum        -> return mempty
          VkTypeCatFuncpointer -> return mempty
          VkTypeCatStruct      -> genStruct t
          VkTypeCatUnion       -> genUnion t



genType :: Monad m => VkType -> ModuleWriter m ClassDeclarations
genType t = case vkTypeCat t of
  VkTypeNoCat          -> return mempty
  VkTypeCatInclude     -> mempty <$ genInclude t
  VkTypeCatDefine      -> return mempty
  VkTypeCatBasetype    -> return mempty
  VkTypeCatBitmask     -> return mempty
  VkTypeCatHandle      -> return mempty
  VkTypeCatEnum        -> return mempty
  VkTypeCatFuncpointer -> return mempty
  VkTypeCatStruct      -> genStruct t
  VkTypeCatUnion       -> genUnion t


-- | At this moment, just define the data type with no constructors.
--   Later we will think what to do with it.
genNocatData :: Monad m => VkType -> ModuleWriter m ()
genNocatData VkTypeSimple
  { name = vkTName
  , attributes = VkTypeAttrs
      { requires = mreq
      }
  } = case tname of
      UnQual () (Ident () n)
        | "HSC2HS___" `T.isPrefixOf` T.pack n -> pure ()
        | otherwise -> do
        -- Guess representation of some imported types
        -- https://github.com/haskell/win32
        -- https://github.com/xmonad/X11
        case n of
          "HINSTANCE" -> writeDecl . setComment rezComment $ parseDecl'
              [text|type HINSTANCE = Ptr ()|]
          "HWND" -> writeDecl . setComment rezComment $ parseDecl'
              [text|type HWND = Ptr ()|]
          "HANDLE" -> writeDecl . setComment rezComment $ parseDecl'
              [text|type HANDLE = Ptr ()|]
          "DWORD" -> writeDecl . setComment rezComment $ parseDecl'
              [text|type DWORD = Word32|]
          "DDWORD" -> writeDecl . setComment rezComment $ parseDecl'
              [text|type DWORD = Word64|]
          "LPCWSTR" -> do
            writeImport $ DIThing "CWchar" DITAll
            writeDecl . setComment rezComment $ parseDecl'
              [text|type LPCWSTR = Ptr CWchar|]
          "Xcb_window_t" -> do
            writeImport $ DIThing "CULong" DITAll
            writeDecl . setComment rezComment $ parseDecl'
              [text|type Xcb_window_t = CULong|]
          "Xcb_visualid_t" -> do
            writeImport $ DIThing "CULong" DITAll
            writeDecl . setComment rezComment $ parseDecl'
              [text|type Xcb_visualid_t = CULong|]
          "Window" -> do
            writeImport $ DIThing "CULong" DITAll
            writeDecl . setComment rezComment $ parseDecl'
              [text|type Window = CULong|]
          "VisualID" -> do
            writeImport $ DIThing "CULong" DITAll
            writeDecl . setComment rezComment $ parseDecl'
              [text|type VisualID = CULong|]
          "RROutput" -> do
            writeImport $ DIThing "CULong" DITAll
            writeDecl . setComment rezComment $ parseDecl'
              [text|type RROutput = CULong|]
          _ -> do
            writePragma "EmptyDataDecls"
            writeDecl . setComment rezComment $ parseDecl'
              [text|data $tnametxt|]
        writeExport $ DIThing tnametxt DITNo
      Qual{}   -> do
        writeImport $ DIThing tnametxt DITNo
        writeExport $ DIThing tnametxt DITNo
      _        -> pure ()
  where
    tname = toHaskellName vkTName
    tnametxt = qNameTxt tname
    rezComment = ((\s -> "Requires @" <> s <> "@") . unVkTypeName <$> mreq)
              >>= preComment . T.unpack
genNocatData t = error
  $ "genNocatData: expected data with no description, but got: "
  <> show t


-- | Stub for VkTypeCatInclude
genInclude :: Monad m => VkType -> ModuleWriter m ()
genInclude VkTypeSimple
    { typeData = VkTypeData
       { code = c
       }
    } = writeSection 0 . T.unlines . map ("> " <>) $ T.lines c
genInclude t = error
  $ "genInclude: expected C-style include code, but got: "
  <> show t

-- | VkTypeCatBasetype
genBasetypeAlias :: Monad m => VkType -> ModuleWriter m ()
genBasetypeAlias t@VkTypeSimple
    { typeData = VkTypeData
       { reference = [(vkTRef, [])]
       }
    } = do
  writeImport $ DIThing (qNameTxt $ toHaskellName vkTRef) DITNo
  genAlias t
genBasetypeAlias t
  = error $ "genBasetypeAlias: expected a simple basetype, but got: "
         <> show t
