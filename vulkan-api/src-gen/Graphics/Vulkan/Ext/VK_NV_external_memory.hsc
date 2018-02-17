#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_NV_external_memory
       (-- * Vulkan extension: @VK_NV_external_memory@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @57@
        --
        -- Required extensions: 'VK_NV_external_memory_capabilities'.
        --

        -- ** Required extensions: 'VK_NV_external_memory_capabilities'.
        VkExternalMemoryImageCreateInfoNV(..),
        VkExportMemoryAllocateInfoNV(..),
        VK_NV_EXTERNAL_MEMORY_SPEC_VERSION,
        pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION,
        VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkImageCreateInfo,
                                                   VkMemoryAllocateInfo)
import           Graphics.Vulkan.Common           (VkExternalMemoryHandleTypeFlagsNV,
                                                   VkStructureType,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryImageCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleTypes;
--   > } VkExternalMemoryImageCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExternalMemoryImageCreateInfoNV.html VkExternalMemoryImageCreateInfoNV registry at www.khronos.org>
data VkExternalMemoryImageCreateInfoNV = VkExternalMemoryImageCreateInfoNV## Addr##
                                                                            ByteArray##

instance Eq VkExternalMemoryImageCreateInfoNV where
        (VkExternalMemoryImageCreateInfoNV## a _) ==
          x@(VkExternalMemoryImageCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryImageCreateInfoNV where
        (VkExternalMemoryImageCreateInfoNV## a _) `compare`
          x@(VkExternalMemoryImageCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryImageCreateInfoNV where
        sizeOf ~_ = #{size VkExternalMemoryImageCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryImageCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryImageCreateInfoNV where
        unsafeAddr (VkExternalMemoryImageCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryImageCreateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryImageCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryImageCreateInfoNV where
        type StructFields VkExternalMemoryImageCreateInfoNV =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryImageCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryImageCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryImageCreateInfoNV =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalMemoryImageCreateInfoNV where
        type VkSTypeMType VkExternalMemoryImageCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryImageCreateInfoNV where
        type FieldType "sType" VkExternalMemoryImageCreateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryImageCreateInfoNV =
             #{offset VkExternalMemoryImageCreateInfoNV, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoNV, sType}

instance CanReadField "sType" VkExternalMemoryImageCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExternalMemoryImageCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalMemoryImageCreateInfoNV where
        type VkPNextMType VkExternalMemoryImageCreateInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryImageCreateInfoNV where
        type FieldType "pNext" VkExternalMemoryImageCreateInfoNV = Ptr Void
        type FieldOptional "pNext" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryImageCreateInfoNV =
             #{offset VkExternalMemoryImageCreateInfoNV, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoNV, pNext}

instance CanReadField "pNext" VkExternalMemoryImageCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExternalMemoryImageCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExternalMemoryImageCreateInfoNV where
        type VkHandleTypesMType VkExternalMemoryImageCreateInfoNV =
             VkExternalMemoryHandleTypeFlagsNV

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryImageCreateInfoNV where
        type FieldType "handleTypes" VkExternalMemoryImageCreateInfoNV =
             VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "handleTypes" VkExternalMemoryImageCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryImageCreateInfoNV =
             #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

instance CanReadField "handleTypes"
           VkExternalMemoryImageCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes"
           VkExternalMemoryImageCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExternalMemoryImageCreateInfoNV where
        showsPrec d x
          = showString "VkExternalMemoryImageCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

-- | > typedef struct VkExportMemoryAllocateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleTypes;
--   > } VkExportMemoryAllocateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkExportMemoryAllocateInfoNV.html VkExportMemoryAllocateInfoNV registry at www.khronos.org>
data VkExportMemoryAllocateInfoNV = VkExportMemoryAllocateInfoNV## Addr##
                                                                  ByteArray##

instance Eq VkExportMemoryAllocateInfoNV where
        (VkExportMemoryAllocateInfoNV## a _) ==
          x@(VkExportMemoryAllocateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryAllocateInfoNV where
        (VkExportMemoryAllocateInfoNV## a _) `compare`
          x@(VkExportMemoryAllocateInfoNV## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryAllocateInfoNV where
        sizeOf ~_ = #{size VkExportMemoryAllocateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryAllocateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryAllocateInfoNV where
        unsafeAddr (VkExportMemoryAllocateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryAllocateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryAllocateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryAllocateInfoNV where
        type StructFields VkExportMemoryAllocateInfoNV =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryAllocateInfoNV =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkExportMemoryAllocateInfoNV where
        type VkSTypeMType VkExportMemoryAllocateInfoNV = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryAllocateInfoNV where
        type FieldType "sType" VkExportMemoryAllocateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryAllocateInfoNV =
             #{offset VkExportMemoryAllocateInfoNV, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoNV, sType}

instance CanReadField "sType" VkExportMemoryAllocateInfoNV where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExportMemoryAllocateInfoNV where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportMemoryAllocateInfoNV where
        type VkPNextMType VkExportMemoryAllocateInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryAllocateInfoNV where
        type FieldType "pNext" VkExportMemoryAllocateInfoNV = Ptr Void
        type FieldOptional "pNext" VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryAllocateInfoNV =
             #{offset VkExportMemoryAllocateInfoNV, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoNV, pNext}

instance CanReadField "pNext" VkExportMemoryAllocateInfoNV where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExportMemoryAllocateInfoNV where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExportMemoryAllocateInfoNV where
        type VkHandleTypesMType VkExportMemoryAllocateInfoNV =
             VkExternalMemoryHandleTypeFlagsNV

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoNV, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportMemoryAllocateInfoNV where
        type FieldType "handleTypes" VkExportMemoryAllocateInfoNV =
             VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "handleTypes" VkExportMemoryAllocateInfoNV =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportMemoryAllocateInfoNV =
             #{offset VkExportMemoryAllocateInfoNV, handleTypes}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoNV, handleTypes}

instance CanReadField "handleTypes" VkExportMemoryAllocateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes" VkExportMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExportMemoryAllocateInfoNV where
        showsPrec d x
          = showString "VkExportMemoryAllocateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

type VK_NV_EXTERNAL_MEMORY_SPEC_VERSION = 1

pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: CString

pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME <-
        (is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME -> True)
  where VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
          = _VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME

{-# INLINE _VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME #-}

_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: CString
_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  = Ptr "VK_NV_external_memory\NUL"##

{-# INLINE is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME #-}

is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME :: CString -> Bool
is_VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME
  = eqCStrings _VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME

type VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_NV_external_memory"

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV =
        VkStructureType 1000056000

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV =
        VkStructureType 1000056001
