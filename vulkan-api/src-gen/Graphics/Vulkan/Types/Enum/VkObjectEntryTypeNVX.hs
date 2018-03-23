{-# OPTIONS_HADDOCK ignore-exports#-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
module Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX
       (VkObjectEntryTypeNVX(VkObjectEntryTypeNVX,
                             VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX,
                             VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX,
                             VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX,
                             VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX,
                             VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX))
       where
import           Data.Data                       (Data)
import           Foreign.Storable                (Storable)
import           GHC.Generics                    (Generic)
import           GHC.Read                        (choose, expectP)
import           Graphics.Vulkan.Marshal         (Int32)
import           Text.ParserCombinators.ReadPrec (prec, step, (+++))
import           Text.Read                       (Read (..), parens)
import           Text.Read.Lex                   (Lexeme (..))

-- | type = @enum@
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkObjectEntryTypeNVX.html VkObjectEntryTypeNVX registry at www.khronos.org>
newtype VkObjectEntryTypeNVX = VkObjectEntryTypeNVX Int32
                                 deriving (Eq, Ord, Num, Bounded, Storable, Enum, Data, Generic)

instance Show VkObjectEntryTypeNVX where
        showsPrec _ VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX"
        showsPrec _ VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX
          = showString "VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX"
        showsPrec p (VkObjectEntryTypeNVX x)
          = showParen (p >= 11)
              (showString "VkObjectEntryTypeNVX " . showsPrec 11 x)

instance Read VkObjectEntryTypeNVX where
        readPrec
          = parens
              (choose
                 [("VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX),
                  ("VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX",
                   pure VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX)]
                 +++
                 prec 10
                   (expectP (Ident "VkObjectEntryTypeNVX") >>
                      (VkObjectEntryTypeNVX <$> step readPrec)))

pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_DESCRIPTOR_SET_NVX =
        VkObjectEntryTypeNVX 0

pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX :: VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_PIPELINE_NVX = VkObjectEntryTypeNVX 1

pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_INDEX_BUFFER_NVX =
        VkObjectEntryTypeNVX 2

pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_VERTEX_BUFFER_NVX =
        VkObjectEntryTypeNVX 3

pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX ::
        VkObjectEntryTypeNVX

pattern VK_OBJECT_ENTRY_TYPE_PUSH_CONSTANT_NVX =
        VkObjectEntryTypeNVX 4
