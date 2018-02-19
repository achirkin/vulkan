#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_EXT_validation_flags
       (-- * Vulkan extension: @VK_EXT_validation_flags@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Tobin Ehlis @tobine@
        --
        -- author: @GOOGLE@
        --
        -- type: @instance@
        --
        -- Extension number: @62@
        VkValidationFlagsEXT(..), VK_EXT_VALIDATION_FLAGS_SPEC_VERSION,
        pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION,
        VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME,
        pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT)
       where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkInstanceCreateInfo)
import           Graphics.Vulkan.Common           (VkStructureType (..),
                                                   VkValidationCheckEXT)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkValidationFlagsEXT {
--   >     VkStructureType                  sType;
--   >     const void*                      pNext;
--   >     uint32_t                         disabledValidationCheckCount;
--   >     VkValidationCheckEXT* pDisabledValidationChecks;
--   > } VkValidationFlagsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkValidationFlagsEXT.html VkValidationFlagsEXT registry at www.khronos.org>
data VkValidationFlagsEXT = VkValidationFlagsEXT## Addr## ByteArray##

instance Eq VkValidationFlagsEXT where
        (VkValidationFlagsEXT## a _) == x@(VkValidationFlagsEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkValidationFlagsEXT where
        (VkValidationFlagsEXT## a _) `compare` x@(VkValidationFlagsEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkValidationFlagsEXT where
        sizeOf ~_ = #{size VkValidationFlagsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkValidationFlagsEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkValidationFlagsEXT where
        unsafeAddr (VkValidationFlagsEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkValidationFlagsEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkValidationFlagsEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkValidationFlagsEXT where
        type StructFields VkValidationFlagsEXT =
             '["sType", "pNext", "disabledValidationCheckCount", -- ' closing tick for hsc2hs
               "pDisabledValidationChecks"]
        type CUnionType VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkValidationFlagsEXT = '[VkInstanceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkValidationFlagsEXT where
        type VkSTypeMType VkValidationFlagsEXT = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkValidationFlagsEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkValidationFlagsEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkValidationFlagsEXT, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkValidationFlagsEXT
         where
        type FieldType "sType" VkValidationFlagsEXT = VkStructureType
        type FieldOptional "sType" VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkValidationFlagsEXT =
             #{offset VkValidationFlagsEXT, sType}
        type FieldIsArray "sType" VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkValidationFlagsEXT, sType}

instance CanReadField "sType" VkValidationFlagsEXT where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkValidationFlagsEXT where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkValidationFlagsEXT where
        type VkPNextMType VkValidationFlagsEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkValidationFlagsEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkValidationFlagsEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkValidationFlagsEXT, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkValidationFlagsEXT
         where
        type FieldType "pNext" VkValidationFlagsEXT = Ptr Void
        type FieldOptional "pNext" VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkValidationFlagsEXT =
             #{offset VkValidationFlagsEXT, pNext}
        type FieldIsArray "pNext" VkValidationFlagsEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkValidationFlagsEXT, pNext}

instance CanReadField "pNext" VkValidationFlagsEXT where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkValidationFlagsEXT where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDisabledValidationCheckCount VkValidationFlagsEXT where
        type VkDisabledValidationCheckCountMType VkValidationFlagsEXT =
             Word32

        {-# NOINLINE vkDisabledValidationCheckCount #-}
        vkDisabledValidationCheckCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, disabledValidationCheckCount})

        {-# INLINE vkDisabledValidationCheckCountByteOffset #-}
        vkDisabledValidationCheckCountByteOffset ~_
          = #{offset VkValidationFlagsEXT, disabledValidationCheckCount}

        {-# INLINE readVkDisabledValidationCheckCount #-}
        readVkDisabledValidationCheckCount p
          = peekByteOff p #{offset VkValidationFlagsEXT, disabledValidationCheckCount}

        {-# INLINE writeVkDisabledValidationCheckCount #-}
        writeVkDisabledValidationCheckCount p
          = pokeByteOff p #{offset VkValidationFlagsEXT, disabledValidationCheckCount}

instance {-# OVERLAPPING #-}
         HasField "disabledValidationCheckCount" VkValidationFlagsEXT where
        type FieldType "disabledValidationCheckCount" VkValidationFlagsEXT
             = Word32
        type FieldOptional "disabledValidationCheckCount"
               VkValidationFlagsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "disabledValidationCheckCount"
               VkValidationFlagsEXT
             =
             #{offset VkValidationFlagsEXT, disabledValidationCheckCount}
        type FieldIsArray "disabledValidationCheckCount"
               VkValidationFlagsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationFlagsEXT, disabledValidationCheckCount}

instance CanReadField "disabledValidationCheckCount"
           VkValidationFlagsEXT
         where
        {-# INLINE getField #-}
        getField = vkDisabledValidationCheckCount

        {-# INLINE readField #-}
        readField = readVkDisabledValidationCheckCount

instance CanWriteField "disabledValidationCheckCount"
           VkValidationFlagsEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkDisabledValidationCheckCount

instance {-# OVERLAPPING #-}
         HasVkPDisabledValidationChecks VkValidationFlagsEXT where
        type VkPDisabledValidationChecksMType VkValidationFlagsEXT =
             Ptr VkValidationCheckEXT

        {-# NOINLINE vkPDisabledValidationChecks #-}
        vkPDisabledValidationChecks x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkValidationFlagsEXT, pDisabledValidationChecks})

        {-# INLINE vkPDisabledValidationChecksByteOffset #-}
        vkPDisabledValidationChecksByteOffset ~_
          = #{offset VkValidationFlagsEXT, pDisabledValidationChecks}

        {-# INLINE readVkPDisabledValidationChecks #-}
        readVkPDisabledValidationChecks p
          = peekByteOff p #{offset VkValidationFlagsEXT, pDisabledValidationChecks}

        {-# INLINE writeVkPDisabledValidationChecks #-}
        writeVkPDisabledValidationChecks p
          = pokeByteOff p #{offset VkValidationFlagsEXT, pDisabledValidationChecks}

instance {-# OVERLAPPING #-}
         HasField "pDisabledValidationChecks" VkValidationFlagsEXT where
        type FieldType "pDisabledValidationChecks" VkValidationFlagsEXT =
             Ptr VkValidationCheckEXT
        type FieldOptional "pDisabledValidationChecks" VkValidationFlagsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDisabledValidationChecks" VkValidationFlagsEXT =
             #{offset VkValidationFlagsEXT, pDisabledValidationChecks}
        type FieldIsArray "pDisabledValidationChecks" VkValidationFlagsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkValidationFlagsEXT, pDisabledValidationChecks}

instance CanReadField "pDisabledValidationChecks"
           VkValidationFlagsEXT
         where
        {-# INLINE getField #-}
        getField = vkPDisabledValidationChecks

        {-# INLINE readField #-}
        readField = readVkPDisabledValidationChecks

instance CanWriteField "pDisabledValidationChecks"
           VkValidationFlagsEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDisabledValidationChecks

instance Show VkValidationFlagsEXT where
        showsPrec d x
          = showString "VkValidationFlagsEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDisabledValidationCheckCount = " .
                            showsPrec d (vkDisabledValidationCheckCount x) .
                              showString ", " .
                                showString "vkPDisabledValidationChecks = " .
                                  showsPrec d (vkPDisabledValidationChecks x) . showChar '}'

pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION = 1

type VK_EXT_VALIDATION_FLAGS_SPEC_VERSION = 1

pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME :: CString

pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME <-
        (is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME -> True)
  where VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
          = _VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME

{-# INLINE _VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME #-}

_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME :: CString
_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
  = Ptr "VK_EXT_validation_flags\NUL"##

{-# INLINE is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME #-}

is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
  = eqCStrings _VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME

type VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME =
     "VK_EXT_validation_flags"

pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT :: VkStructureType

pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT =
        VkStructureType 1000061000
