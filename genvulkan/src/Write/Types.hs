{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Types
  ( genBaseTypes, genType
  , genBaseStructs
  , writeAllTypes
  ) where

import           Control.Arrow                        ((&&&))
import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.Reader.Class
import qualified Control.Monad.Trans.RWS.Strict       as RWS
import           Control.Monad.Trans.State.Strict     (StateT)
import qualified Control.Monad.Trans.State.Strict     as State
import           Data.Char                            (isLower, isUpper)
import qualified Data.List                            as L
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe, isNothing)
import           Data.Semigroup
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Extensions
import           VkXml.Sections.Feature
import           VkXml.Sections.Types

import           Write.ModuleWriter
import           Write.Types.Define
import           Write.Types.Enum
import           Write.Types.Funcpointer
import           Write.Types.Handle
import           Write.Types.Struct
-- import           Write.Util.DeclaredNames


-- | Write all types one-per-module in Graphics.Vulkan.Types
writeAllTypes :: VkXml
              -> DeclaredNames
              -> IO ([(ModuleWriting, Maybe ProtectDef)], DeclaredNames)
writeAllTypes vkXml@VkXml{..}
  | Map.null allt8 = State.runStateT $ do

    moduleInclude <-
      writeSimpleTypes "Graphics.Vulkan.Types.Include" $ do
        mapM_ genInclude typesIncludes
        mapM_ genNocatData typesNoCat

    moduleDefines <-
      writeSimpleTypes "Graphics.Vulkan.Types.Defines" $
        mapM_ genDefine typesDefine

    moduleBaseTypes <-
      writeSimpleTypes "Graphics.Vulkan.Types.BaseTypes" $
        mapM_ genBasetypeAlias typesBaseTypes

    moduleHandles <-
      writeSimpleTypes "Graphics.Vulkan.Types.Handles" $
        mapM_ genHandle typesHandles

    moduleFuncpointers <-
      writeSimpleTypes "Graphics.Vulkan.Types.Funcpointers" $
        mapM_ genFuncpointer typesFuncpointers

    moduleOrphanBitmasks <-
      writeSimpleTypes "Graphics.Vulkan.Types.Bitmasks" $
        mapM_ genEnum bitmasksNoRequireds


    let moduleEnumBitmaskRuns
           = groupEnumMods
           . L.sortOn fst
           $ map (vktName &&& genEnum) (Map.elems trueEnums)
          ++ map (\(tbm, te) -> (vktName tbm, genBitmaskPair tbm te)) (Map.elems bitmasksCouples)

    moduleEnumBitmasks <- forM moduleEnumBitmaskRuns $ \(tn, as) ->
      writeSimpleTypes ("Graphics.Vulkan.Types.Enum." <> T.unpack tn
                       ) $ sequence_ as


    -- moduleEnums <- forM (Map.elems trueEnums) $ \t ->
    --   writeSimpleTypes ("Graphics.Vulkan.Types.Enum."
    --                      <> T.unpack (unVkTypeName
    --                                    $ (name :: VkType -> VkTypeName) t)
    --                    ) $ genEnum t
    --
    -- moduleBitmasks <- forM (Map.elems bitmasksCouples) $ \(tbm, te) ->
    --   writeSimpleTypes ("Graphics.Vulkan.Types.Enum."
    --                      <> T.unpack (unVkTypeName
    --                                    $ (name :: VkType -> VkTypeName) tbm)
    --                    ) $ genBitmaskPair tbm te

    moduleStructs
      <- forM
           ( groupPD
           . L.sortOn snd
           . map (\(t,p) -> (t, (p, groupedTypeModName $ vktName t)))
           . Map.elems
           . evalProtectedTypes vkXml
           . removeDisabledTypes vkXml
           $ typesStructsOrUnions)
         $ \(modN, mpd, ts) -> do

      mds <- State.get
      ((), mr) <- runModuleWriter vkXml
                      ("Graphics.Vulkan.Types.Struct."
                          <> T.unpack modN
                          -- T.unpack (unVkTypeName
                          -- $ (name :: VkType -> VkTypeName) t)
                      ) mds $ do
        writePragma "Strict"
        writePragma "DataKinds"
        forM_ ts $ \t -> genStructOrUnion (VkTypeCatUnion == vkTypeCat t) t
      State.put (globalNames mr)
      return (mr, mpd)



    return $
      [ moduleInclude
      , moduleDefines
      , moduleBaseTypes
      , moduleHandles
      , moduleFuncpointers
      , moduleOrphanBitmasks
      ] ++ moduleEnumBitmasks ++ moduleStructs
  | otherwise = error $ "Not all types were processed\n" ++ show allt8
  where
    vktName = unVkTypeName . (name :: VkType -> VkTypeName)

    mpdToName Nothing = "Core"
    mpdToName (Just ProtectDef{ protectFlag = ProtectFlag n})
      | Just modName <- T.stripPrefix "use" n = firstUp modName
      | otherwise = firstUp n

    groupPD [] = []
    groupPD ((t,(Just p, _)):xs)
      = let (ys, zs) = L.span ((Just p ==) . fst . snd) xs
        in (mpdToName $ Just p, Just p, t : map fst ys) : groupPD zs
    groupPD ((t,(Nothing, modN)):xs)
      = let (ys, zs) = L.span (\(_, (p1, modN1)) -> isNothing p1 && modN1 == modN) xs
            allTNames = map (stripVk . vktName) (t : map fst ys)
        in ( if all (T.isPrefixOf modN) allTNames
             then longestCommonPrefix allTNames
             else modN
           , Nothing, t : map fst ys) : groupPD zs

    groupEnumMods [] = []
    groupEnumMods ((tn, a):xs)
      = let modName = groupedTypeModName tn
            allTNames = map stripVk (tn : map fst ys)
            (ys, zs) = L.span (\(tn1, _) -> groupedTypeModName tn1 == modName) xs
        in ( if all (T.isPrefixOf modName) allTNames
             then longestCommonPrefix allTNames
             else modName
           , a : map snd ys) : groupEnumMods zs

    (typesNoCat, allt0)
      = Map.partition ((VkTypeNoCat ==) . vkTypeCat) globTypes
    (typesIncludes, allt1)
      = Map.partition ((VkTypeCatInclude ==) . vkTypeCat) allt0
    (typesDefine, allt2)
      = Map.partition ((VkTypeCatDefine ==) . vkTypeCat) allt1
    (typesBaseTypes, allt3)
      = Map.partition ((VkTypeCatBasetype ==) . vkTypeCat) allt2
    (typesHandles, allt4)
      = Map.partition ((VkTypeCatHandle ==) . vkTypeCat) allt3
    (typesFuncpointers, allt5)
      = Map.partition ((VkTypeCatFuncpointer ==) . vkTypeCat) allt4

    (typesBitmasks, allt6)
      = Map.partition ((VkTypeCatBitmask ==) . vkTypeCat) allt5
    (typesEnums, allt7)
      = Map.partition ((VkTypeCatEnum ==) . vkTypeCat) allt6

    (bitmasksNoRequireds, bitmasksRequireds)
      = Map.partition (isNothing . requires . (attributes :: VkType -> VkTypeAttrs)
                      ) typesBitmasks

    (typesStructsOrUnions, allt8)
      = Map.partition ((\c -> VkTypeCatStruct == c || VkTypeCatUnion == c
                       ) . vkTypeCat) allt7

    (trueEnums, bitmasksCouples)
      = Map.mapAccum (\eMap bm@VkTypeSimple
                              { attributes = VkTypeAttrs
                                 { requires = Just eName}
                              }
                          -> case Map.updateLookupWithKey
                                    (\_ _ -> Nothing) eName eMap of
                              (Nothing, _) ->
                                error "writeAllTypes/updateLookupWithKey failed!"
                              (Just ev, eMap') -> (eMap', (bm, ev))
                     ) typesEnums bitmasksRequireds


    writeSimpleTypes mname a = do
      mds <- State.get
      ((), mr) <- runModuleWriter vkXml mname mds $ do
        writePragma "Strict"
        writePragma "DataKinds"
        a
      State.put (globalNames mr)
      return (mr, Nothing)
















genBaseTypes :: Monad m => ModuleWriter m ()
genBaseTypes = hoist (`State.evalStateT` Nothing) genBaseTypes'


genBaseTypes' :: Monad m => ModuleWriter (StateT (Maybe VkTypeCategory) m) ()
genBaseTypes' = do
    vkXml <- ask
    glvl <- ModuleWriter $ RWS.gets currentSecLvl
    writeSection glvl "Types and enumerations"
    pushSecLvl $ \curlvl ->
      mapM_ (fItem curlvl) (globTypes vkXml)
        where
          fItem curlvl t = do
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

            case vkTypeCat t of
              VkTypeNoCat          -> return ()
              VkTypeCatInclude     -> return ()
              VkTypeCatDefine      -> return ()
              VkTypeCatBasetype    -> return ()
              VkTypeCatBitmask     -> genEnum t
              VkTypeCatHandle      -> return ()
              VkTypeCatEnum        -> genEnum t
              VkTypeCatFuncpointer -> return ()
              VkTypeCatStruct      -> return ()
              VkTypeCatUnion       -> return ()


genBaseStructs :: Monad m => ModuleWriter m ()
genBaseStructs = do
    vkXml <- ask
    let featureTypes = Set.fromList
                     $ globFeature vkXml >>= reqList >>= requireTypes
        extTypes = Set.fromList
                      $ Map.elems (globExtensions vkXml)
                          >>= extRequires >>= requireTypes
        excludedTypes = Set.union featureTypes extTypes

    forM_ (Map.elems $ globTypes vkXml) $ \t ->
        if (name :: VkType -> VkTypeName) t `Set.member` excludedTypes
        then pure mempty
        else case vkTypeCat t of
          VkTypeNoCat          -> return mempty
          VkTypeCatInclude     -> return mempty -- <$ genInclude t
          VkTypeCatDefine      -> return mempty
          VkTypeCatBasetype    -> return mempty
          VkTypeCatBitmask     -> return mempty
          VkTypeCatHandle      -> return mempty
          VkTypeCatEnum        -> return mempty
          VkTypeCatFuncpointer -> return mempty
          VkTypeCatStruct      -> genStruct t
          VkTypeCatUnion       -> genUnion t



genType :: Monad m => VkType -> ModuleWriter m ()
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
        | "Word8"  == n -> pure ()
        | "Word16" == n -> pure ()
        | "Word32" == n -> pure ()
        | "Word64" == n -> pure ()
        | "Int8"   == n -> pure ()
        | "Int16"  == n -> pure ()
        | "Int32"  == n -> pure ()
        | "Int64"  == n -> pure ()
        | "CChar"  == n -> pure ()
        | "CInt"   == n -> pure ()
        | "CSize"  == n -> pure ()
        | "Float"  == n -> pure ()
        | "Double" == n -> pure ()
        | otherwise -> do
        -- Guess representation of some imported types
        -- https://github.com/haskell/win32
        -- https://github.com/xmonad/X11
        case n of
          "HINSTANCE" -> do
            writeImport $ DIThing "Ptr" DITEmpty
            writeDecl . setComment rezComment $ parseDecl'
              [text|type HINSTANCE = Ptr ()|]
          "HWND" -> do
            writeImport $ DIThing "Ptr" DITEmpty
            writeDecl . setComment rezComment $ parseDecl'
              [text|type HWND = Ptr ()|]
          "HANDLE" -> do
            writeImport $ DIThing "Ptr" DITEmpty
            writeDecl . setComment rezComment $ parseDecl'
              [text|type HANDLE = Ptr ()|]
          "DWORD" -> do
            writeImport $ DIThing "Word32" DITEmpty
            writeDecl . setComment rezComment $ parseDecl'
              [text|type DWORD = Word32|]
          "DDWORD" -> do
            writeImport $ DIThing "Word64" DITEmpty
            writeDecl . setComment rezComment $ parseDecl'
              [text|type DWORD = Word64|]
          "LPCWSTR" -> do
            writeImport $ DIThing "CWchar" DITAll
            writeImport $ DIThing "Ptr" DITEmpty
            writeDecl . setComment rezComment $ parseDecl'
              [text|type LPCWSTR = Ptr CWchar|]
          "XcbWindowT" -> do
            writeImport $ DIThing "CULong" DITAll
            writeDecl . setComment rezComment $ parseDecl'
              [text|type XcbWindowT = CULong|]
          "XcbVisualidT" -> do
            writeImport $ DIThing "CULong" DITAll
            writeDecl . setComment rezComment $ parseDecl'
              [text|type XcbVisualidT = CULong|]
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
    tname = toQName vkTName
    tnametxt = unVkTypeName vkTName
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
  writeImport $ DIThing (unVkTypeName vkTRef) DITNo
  genAlias t
genBasetypeAlias t
  = error $ "genBasetypeAlias: expected a simple basetype, but got: "
         <> show t

-- | Group types into modules by their names.
--   This function handles explicitly some corner cases to avoid mutually recursive modules.
groupedTypeModName :: T.Text -> T.Text


groupedTypeModName "VkDebugUtilsMessengerCallbackDataEXT" = "DebugUtilsMessengerCallbackDataEXT"
groupedTypeModName "VkDebugUtilsMessengerCreateInfoEXT"   = "DebugUtilsMessengerCreateInfoEXT"
groupedTypeModName "VkDebugUtilsObjectNameInfoEXT"        = "DebugUtilsObjectNameInfoEXT"
groupedTypeModName "VkDebugUtilsLabelEXT"                 = "DebugUtilsLabelEXT"

groupedTypeModName "VkGraphicsPipelineCreateInfo"         = "Pipeline"

groupedTypeModName "VkPhysicalDeviceFeatures"             = "PhysicalDeviceFeatures"

groupedTypeModName n = takeFirstCamel $ stripVk n
  where
    takeFirstCamel s
      = let (s0, srest) = T.span isUpper s
            (s1, _)     = T.span isLower srest
        in s0 <> s1

stripVk :: T.Text -> T.Text
stripVk s = fromMaybe s $ T.stripPrefix "Vk" s

longestCommonPrefix :: [T.Text] -> T.Text
longestCommonPrefix = T.pack . go . map T.unpack
  where
    allStart _ [] = Just []
    allStart _ ("":_) = Nothing
    allStart c ((a:as):xs)
      | c == a    = (as:) <$> allStart c xs
      | otherwise = Nothing
    go [] = ""
    go ("":_) = ""
    go ((c:cs):xs) = case allStart c xs of
       Nothing -> ""
       Just xs' -> c : go (cs:xs')
