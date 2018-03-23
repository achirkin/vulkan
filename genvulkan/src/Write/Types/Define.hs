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
import           Text.RE.TDFA.Text
import           Text.Read (readMaybe)

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

    -- vkName == VkTypeName "VK_API_VERSION_XXX"
  | True <- matched (tnameTxt ?=~ [reBS|VK_API_VERSION_[^[:space:]]+|])
  , Just (major, minor, patch)
    <-   matchedText (c  ?=~ [reBS|#define[[:space:]]+VK_API_VERSION_[^[:space:]]+[[:space:]]+VK_MAKE_VERSION[[:space:]]*\(.*\)|])
    >>= \c' ->
         matchedText (c' ?=~ [reBS|\(.*\)|])
    >>= readMaybe . T.unpack
  = () <$ enumPattern VkEnum
    { _vkEnumName    = VkEnumName tnameTxt
    , _vkEnumTName   = Nothing
    , _vkEnumComment = T.unlines . map ("> " <>) $ T.lines c
    , _vkEnumValue   = VkEnumIntegral
      (fromIntegral (unsafeShiftL major 22 .|. unsafeShiftL minor 12 .|. patch :: Word32))
      "(Num a, Eq a) => a"
    }

  | VkTypeName "VK_HEADER_VERSION" <- vkName -- this thing changes more often, so it is better to parse it.
  , mnumber <- snd $ T.breakOnEnd "VK_HEADER_VERSION" c
  , Right (v, _) <- T.decimal $ T.stripStart mnumber
  = () <$ enumPattern VkEnum
    { _vkEnumName    = VkEnumName "VK_HEADER_VERSION"
    , _vkEnumTName   = Nothing
    , _vkEnumComment = T.unlines . map ("> " <>) $ T.lines c
    , _vkEnumValue   = VkEnumIntegral
      (fromIntegral (v :: Word32))
      "(Num a, Eq a) => a"
    }

  | vkName == VkTypeName "VK_DEFINE_HANDLE"
  && "#define VK_DEFINE_HANDLE(object) typedef struct object##_T* object;" `T.isInfixOf` c
  = do
    writeImport $ DIThing "Ptr" DITEmpty
    writeExport $ DIThing "Ptr" DITEmpty
    writeSection 0 . T.unlines
                 . ("| ===== @VK_DEFINE_HANDLE@":)
                 . ("Dispatchable handles are represented as `Foreign.Ptr`":)
                 . ("":)
                 . map ("> " <>) $ T.lines c


  | VkTypeName "VK_DEFINE_NON_DISPATCHABLE_HANDLE" <- vkName
  = do
    writeFullImport "Graphics.Vulkan.Marshal"
    writeExport $ DIThing "VkPtr" DITAll
    writeSection 0 . T.unlines
                 . ("| ===== @VK_DEFINE_NON_DISPATCHABLE_HANDLE@":)
                 . ("Non-dispatchable handles are represented as `VkPtr`":)
                 . ("":)
                 . map ("> " <>) $ T.lines c

  | vkName == VkTypeName "VK_NULL_HANDLE"
  = do
    writeFullImport "Graphics.Vulkan.Marshal"
    writeExport $ DIThing "VulkanPtr" DITAll
    writeExport $ DIPat "VK_NULL_HANDLE"

  | matched (c ?=~ [reBS|^struct[[:space:]]+[^[:space:]]+;$|])
  = do
    writePragma "EmptyDataDecls"
    writeDecl . setComment rezComment $ parseDecl' $
      "data " <> tnameTxt
    writeExport $ DIThing tnameTxt DITEmpty

  | otherwise = error
                $ "Write.Types.Define.genDefine: unknown define!\n"
               <> "Please, add a new guard to the function. Data: "
               <> show t

  where
    tnameTxt = unVkTypeName vkName
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
