{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
module Write.Types.Define
  ( genDefine
  ) where

-- import           Control.Arrow                        ((***))
import           Data.Bits
import           Data.Semigroup
import qualified Data.Text                            as T
import qualified Data.Text.Read                       as T
import           Data.Word
import           Language.Haskell.Exts.SimpleComments
import           NeatInterpolation

import           VkXml.CommonTypes
import           VkXml.Sections.Enums
import           VkXml.Sections.Types

import           Write.ModuleWriter
import           Write.Types.Enum


-- | @#define@ elements in vk.xml provide a number of CPP pragmas.
--   There are two problems with them:
--
--    1. they are hard to parse;
--
--    2. haskell-src-exts does not support CPP.
--
--   Here are current workarounds I come up with:
--
--    1. pattern-match against known defines and provide my own code;
--
--    2. put unavoidable CPP code into comments.
--
--   If a new define occurs, or one of existing ones changes,
--     someone needs to add here an additional pattern match case.
--   To re-enable CPP pragmas, one needs to remove comments from generated code.
genDefine :: Monad m => VkType -> ModuleWriter m ()
genDefine t@VkTypeSimple
    { name = vkName
    , typeData = VkTypeData
       { code = c
       }
    }

  | vkName == VkTypeName "VK_MAKE_VERSION"
  && c == "#define VK_MAKE_VERSION(major, minor, patch) \\\n    (((major) << 22) | ((minor) << 12) | (patch))"
  = go (writeImport $ DIThing "Bits" DITAll)
      [text|_VK_MAKE_VERSION :: Bits a => a -> a -> a -> a|]
      [text|_VK_MAKE_VERSION major minor patch = unsafeShiftL major 22 .|. unsafeShiftL minor 12 .|. patch|]
      [text|{-# INLINE _VK_MAKE_VERSION #-}|]
      [text|###define VK_MAKE_VERSION(major, minor, patch) _VK_MAKE_VERSION major minor patch|]
      "_VK_MAKE_VERSION"

  | vkName == VkTypeName "VK_VERSION_MAJOR"
  && c == "#define VK_VERSION_MAJOR(version) ((uint32_t)(version) >> 22)"
  = go (writeImport $ DIThing "Bits" DITAll)
      [text|_VK_VERSION_MAJOR :: Bits a => a -> a|]
      [text|_VK_VERSION_MAJOR version = unsafeShiftR version 22|]
      [text|{-# INLINE _VK_VERSION_MAJOR #-}|]
      [text|###define VK_VERSION_MAJOR(version) _VK_VERSION_MAJOR version|]
      "_VK_VERSION_MAJOR"

  | vkName == VkTypeName "VK_VERSION_MINOR"
  && c == "#define VK_VERSION_MINOR(version) (((uint32_t)(version) >> 12) & 0x3ff)"
  = go (writeImport $ DIThing "Bits" DITAll)
      [text|_VK_VERSION_MINOR :: (Bits a, Num a) => a -> a|]
      [text|_VK_VERSION_MINOR version = unsafeShiftR version 12 .&. 0x3ff|]
      [text|{-# INLINE _VK_VERSION_MINOR #-}|]
      [text|###define VK_VERSION_MINOR(version) _VK_VERSION_MINOR version|]
      "_VK_VERSION_MINOR"

  | vkName == VkTypeName "VK_VERSION_PATCH"
  && c == "#define VK_VERSION_PATCH(version) ((uint32_t)(version) & 0xfff)"
  = go (writeImport $ DIThing "Bits" DITAll)
      [text|_VK_VERSION_PATCH :: (Bits a, Num a) => a -> a|]
      [text|_VK_VERSION_PATCH = (.&. 0xfff)|]
      [text|{-# INLINE _VK_VERSION_PATCH #-}|]
      [text|###define VK_VERSION_PATCH(version) _VK_VERSION_PATCH version|]
      "_VK_VERSION_PATCH"

  | vkName == VkTypeName "VK_API_VERSION" -- deprecated, so no need to use it
  = writeSection 0 . T.unlines
                   . ("| ===== @VK_API_VERSION@":) . map ("> " <>) $ T.lines c

  | vkName == VkTypeName "VK_API_VERSION_1_0"
  && c == "// Vulkan 1.0 version number\n#define VK_API_VERSION_1_0 VK_MAKE_VERSION(1, 0, 0)// Patch version should always be set to 0"
  = enumPattern VkEnum
    { _vkEnumName    = VkEnumName "VK_API_VERSION_1_0"
    , _vkEnumTName   = Nothing
    , _vkEnumComment = T.unlines . map ("> " <>) $ T.lines c
    , _vkEnumValue   = VkEnumIntegral
      (fromIntegral (unsafeShiftL 1 22 .|. unsafeShiftL 0 12 .|. 0 :: Word32))
      "(Num a, Eq a) => a"
    }

  | VkTypeName "VK_HEADER_VERSION" <- vkName -- this thing changes more often, so it is better to parse it.
  , mnumber <- snd $ T.breakOnEnd "VK_HEADER_VERSION" c
  , Right (v, _) <- T.decimal $ T.stripStart mnumber
  = enumPattern VkEnum
    { _vkEnumName    = VkEnumName "VK_HEADER_VERSION"
    , _vkEnumTName   = Nothing
    , _vkEnumComment = T.unlines . map ("> " <>) $ T.lines c
    , _vkEnumValue   = VkEnumIntegral
      (fromIntegral (v :: Word32))
      "(Num a, Eq a) => a"
    }

  | vkName == VkTypeName "VK_DEFINE_HANDLE"
  && "#define VK_DEFINE_HANDLE(object) typedef struct object##_T* object;" `T.isInfixOf` c
  = pure ()
    -- writeImport $ DIThing "Ptr" DITEmpty
    -- writeImport $ DIVar "nullPtr"
    -- writeDecl $ parseDecl' [text|
    --       instance VulkanPtr Ptr where
    --         vkNullPtr = nullPtr
    --         {-# INLINE vkNullPtr #-}
    --       |]
    -- writeExport $ DIThing "Ptr" DITEmpty
    -- writeSection 0 . T.unlines
    --              . ("| ===== @VK_DEFINE_HANDLE@":)
    --              . ("Dispatchable handles are represented as `Foreign.Ptr`":)
    --              . ("":)
    --              . map ("> " <>) $ T.lines c


  | VkTypeName "VK_DEFINE_NON_DISPATCHABLE_HANDLE" <- vkName
  = do
    writeFullImport "Graphics.Vulkan.Marshal"
    writeExport $ DIThing "VkPtr" DITAll
  -- , defA <- "#define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef struct object##_T *object;"
  -- , defB <- "#define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef uint64_t object;"
  -- , (part1, rem1) <- (T.strip *** T.strip . T.drop (T.length defA)) $ T.breakOn defA c
  -- , (part2, part3) <- (T.strip *** T.strip . T.drop (T.length defB)) $ T.breakOn defB rem1
  -- , altDef <- [text|
  --     newtype VkPtr a = VkPtr (Ptr a)
  --        deriving (Eq, Ord, Show, Storable)
  --     instance VulkanPtr VkPtr where
  --        vkNullPtr = VkPtr vkNullPtr
  --        {-# INLINE vkNullPtr #-}
  --    |]
  -- , preComm <- Just $ CodeComment AboveCode ' ' . T.unpack . T.unlines $
  --     [ "| ===== @VK_DEFINE_NON_DISPATCHABLE_HANDLE@"
  --     , "Non-dispatchable handles are represented as `VkPtr`"
  --     , ""
  --     , T.unlines . map ("> " <>) $ T.lines c
  --     , T.unlines . map ("##"<>) $
  --         T.lines part1 ++ T.lines altDef ++ T.lines part2
  --     ]
  -- , postComm <- Just $ CodeComment BelowCode ' ' . T.unpack . T.unlines $
  --     map ("##"<>) $ T.lines part3
  -- = do
  --   writePragma "CPP"
  --   writePragma "GeneralizedNewtypeDeriving"
  --   writePragma "RoleAnnotations"
  --   writeImport $ DIThing "Storable" DITEmpty
  --
  --
  --   writeDecl $ parseDecl' "type role VkPtr phantom"
  --
  --   writeDecl . setComment preComm
  --             $ parseDecl' [text|
  --                 newtype VkPtr a = VkPtr Word64
  --                   deriving (Eq, Ord, Show, Storable)
  --                 |]
  --   writeDecl . setComment postComm
  --             $ parseDecl' [text|
  --                 instance VulkanPtr VkPtr where
  --                   vkNullPtr = VkPtr 0
  --                   {-# INLINE vkNullPtr #-}
  --                 |]
  --
  --
  --   writeExport $ DIThing "VkPtr" DITAll


  | vkName == VkTypeName "VK_NULL_HANDLE"
  = do
    writeFullImport "Graphics.Vulkan.Marshal"
    writeExport $ DIThing "VulkanPtr" DITAll
    writeExport $ DIPat "VK_NULL_HANDLE"
  -- = if not $ "#define VK_NULL_HANDLE 0" `T.isInfixOf` c
  --   then error $ "Broken assertion VK_NULL_HANDLE == 0 when parsing" <> show t
  --   else do
  --     writePragma "PatternSynonyms"
  --     writePragma "ViewPatterns"
  --     let ds = parseDecls [text|
  --           class VulkanPtr ptr where
  --             vkNullPtr :: ptr a
  --
  --           isNullPtr :: (Eq (ptr a), VulkanPtr ptr) => ptr a -> Bool
  --           isNullPtr = (vkNullPtr ==)
  --           {-# INLINE isNullPtr #-}
  --
  --           pattern VK_NULL_HANDLE :: (Eq (ptr a), VulkanPtr ptr) => ptr a
  --           pattern VK_NULL_HANDLE <- (isNullPtr -> True)
  --             where
  --               VK_NULL_HANDLE = vkNullPtr
  --           |]
  --
  --     mapM_ writeDecl
  --       . insertDeclComment "VK_NULL_HANDLE" rezComment
  --       . insertDeclComment "VulkanPtr"
  --           (preComment "Unify dispatchable and non-dispatchable vulkan pointer types.")
  --       $ ds
  --
  --     writeExport $ DIThing "VulkanPtr" DITAll
  --     writeExport $ DIPat "VK_NULL_HANDLE"


  | otherwise = error
                $ "Write.Types.Define.genDefine: unknown define!\n"
               <> "Please, add a new guard to the function. Data: "
               <> show t

  where
    rezComment = preComment . T.unpack . T.unlines . map ("> " <>) $ T.lines c
    go imprts l1 l2 l3 lcpp ename = do
      writePragma "CPP"
      () <- imprts
      writeDecl . setComment rezComment $ parseDecl' l1
      writeDecl $ parseDecl' l2
      let commentDefine = Just $ CodeComment BelowCode ' ' $ T.unpack lcpp
      writeDecl . setComment commentDefine $ parseDecl' l3
      writeExport $ DIVar (T.pack ename)
genDefine t = error
  $ "genInclude: expected C-style include code, but got: "
  <> show t
