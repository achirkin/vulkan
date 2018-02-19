#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_KHR_external_memory
       (-- * Vulkan extension: @VK_KHR_external_memory@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @73@
        --
        -- Required extensions: 'VK_KHR_external_memory_capabilities'.
        --

        -- ** Required extensions: 'VK_KHR_external_memory_capabilities'.
        VkExternalMemoryImageCreateInfoKHR(..),
        VkExternalMemoryBufferCreateInfoKHR(..),
        VkExportMemoryAllocateInfoKHR(..),
        VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR,
        pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR,
        pattern VK_QUEUE_FAMILY_EXTERNAL_KHR, VK_QUEUE_FAMILY_EXTERNAL_KHR)
       where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkBufferCreateInfo,
                                                   VkImageCreateInfo,
                                                   VkMemoryAllocateInfo)
import           Graphics.Vulkan.Common           (VK_QUEUE_FAMILY_EXTERNAL_KHR, pattern VK_QUEUE_FAMILY_EXTERNAL_KHR,
                                                   VkExternalMemoryHandleTypeFlagsKHR,
                                                   VkResult (..),
                                                   VkStructureType,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryImageCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsKHR handleTypes;
--   > } VkExternalMemoryImageCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalMemoryImageCreateInfoKHR.html VkExternalMemoryImageCreateInfoKHR registry at www.khronos.org>
data VkExternalMemoryImageCreateInfoKHR = VkExternalMemoryImageCreateInfoKHR## Addr##
                                                                              ByteArray##

instance Eq VkExternalMemoryImageCreateInfoKHR where
        (VkExternalMemoryImageCreateInfoKHR## a _) ==
          x@(VkExternalMemoryImageCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryImageCreateInfoKHR where
        (VkExternalMemoryImageCreateInfoKHR## a _) `compare`
          x@(VkExternalMemoryImageCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryImageCreateInfoKHR where
        sizeOf ~_ = #{size VkExternalMemoryImageCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryImageCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryImageCreateInfoKHR where
        unsafeAddr (VkExternalMemoryImageCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryImageCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryImageCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryImageCreateInfoKHR where
        type StructFields VkExternalMemoryImageCreateInfoKHR =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryImageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryImageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryImageCreateInfoKHR =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalMemoryImageCreateInfoKHR where
        type VkSTypeMType VkExternalMemoryImageCreateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryImageCreateInfoKHR where
        type FieldType "sType" VkExternalMemoryImageCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryImageCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryImageCreateInfoKHR =
             #{offset VkExternalMemoryImageCreateInfoKHR, sType}
        type FieldIsArray "sType" VkExternalMemoryImageCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoKHR, sType}

instance CanReadField "sType" VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalMemoryImageCreateInfoKHR where
        type VkPNextMType VkExternalMemoryImageCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryImageCreateInfoKHR where
        type FieldType "pNext" VkExternalMemoryImageCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkExternalMemoryImageCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryImageCreateInfoKHR =
             #{offset VkExternalMemoryImageCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkExternalMemoryImageCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoKHR, pNext}

instance CanReadField "pNext" VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExternalMemoryImageCreateInfoKHR where
        type VkHandleTypesMType VkExternalMemoryImageCreateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryImageCreateInfoKHR where
        type FieldType "handleTypes" VkExternalMemoryImageCreateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "handleTypes" VkExternalMemoryImageCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryImageCreateInfoKHR =
             #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}
        type FieldIsArray "handleTypes" VkExternalMemoryImageCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoKHR, handleTypes}

instance CanReadField "handleTypes"
           VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes"
           VkExternalMemoryImageCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExternalMemoryImageCreateInfoKHR where
        showsPrec d x
          = showString "VkExternalMemoryImageCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

-- | > typedef struct VkExternalMemoryBufferCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsKHR handleTypes;
--   > } VkExternalMemoryBufferCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalMemoryBufferCreateInfoKHR.html VkExternalMemoryBufferCreateInfoKHR registry at www.khronos.org>
data VkExternalMemoryBufferCreateInfoKHR = VkExternalMemoryBufferCreateInfoKHR## Addr##
                                                                                ByteArray##

instance Eq VkExternalMemoryBufferCreateInfoKHR where
        (VkExternalMemoryBufferCreateInfoKHR## a _) ==
          x@(VkExternalMemoryBufferCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryBufferCreateInfoKHR where
        (VkExternalMemoryBufferCreateInfoKHR## a _) `compare`
          x@(VkExternalMemoryBufferCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryBufferCreateInfoKHR where
        sizeOf ~_ = #{size VkExternalMemoryBufferCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryBufferCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryBufferCreateInfoKHR
         where
        unsafeAddr (VkExternalMemoryBufferCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryBufferCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryBufferCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryBufferCreateInfoKHR where
        type StructFields VkExternalMemoryBufferCreateInfoKHR =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryBufferCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryBufferCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryBufferCreateInfoKHR =
             '[VkBufferCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkExternalMemoryBufferCreateInfoKHR where
        type VkSTypeMType VkExternalMemoryBufferCreateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExternalMemoryBufferCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryBufferCreateInfoKHR where
        type FieldType "sType" VkExternalMemoryBufferCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryBufferCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryBufferCreateInfoKHR =
             #{offset VkExternalMemoryBufferCreateInfoKHR, sType}
        type FieldIsArray "sType" VkExternalMemoryBufferCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfoKHR, sType}

instance CanReadField "sType" VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExternalMemoryBufferCreateInfoKHR where
        type VkPNextMType VkExternalMemoryBufferCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryBufferCreateInfoKHR where
        type FieldType "pNext" VkExternalMemoryBufferCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkExternalMemoryBufferCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryBufferCreateInfoKHR =
             #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkExternalMemoryBufferCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfoKHR, pNext}

instance CanReadField "pNext" VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExternalMemoryBufferCreateInfoKHR where
        type VkHandleTypesMType VkExternalMemoryBufferCreateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryBufferCreateInfoKHR where
        type FieldType "handleTypes" VkExternalMemoryBufferCreateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "handleTypes"
               VkExternalMemoryBufferCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryBufferCreateInfoKHR
             =
             #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}
        type FieldIsArray "handleTypes" VkExternalMemoryBufferCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfoKHR, handleTypes}

instance CanReadField "handleTypes"
           VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes"
           VkExternalMemoryBufferCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExternalMemoryBufferCreateInfoKHR where
        showsPrec d x
          = showString "VkExternalMemoryBufferCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

-- | > typedef struct VkExportMemoryAllocateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsKHR handleTypes;
--   > } VkExportMemoryAllocateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExportMemoryAllocateInfoKHR.html VkExportMemoryAllocateInfoKHR registry at www.khronos.org>
data VkExportMemoryAllocateInfoKHR = VkExportMemoryAllocateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkExportMemoryAllocateInfoKHR where
        (VkExportMemoryAllocateInfoKHR## a _) ==
          x@(VkExportMemoryAllocateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryAllocateInfoKHR where
        (VkExportMemoryAllocateInfoKHR## a _) `compare`
          x@(VkExportMemoryAllocateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryAllocateInfoKHR where
        sizeOf ~_ = #{size VkExportMemoryAllocateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryAllocateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryAllocateInfoKHR where
        unsafeAddr (VkExportMemoryAllocateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryAllocateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryAllocateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryAllocateInfoKHR where
        type StructFields VkExportMemoryAllocateInfoKHR =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryAllocateInfoKHR =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkExportMemoryAllocateInfoKHR where
        type VkSTypeMType VkExportMemoryAllocateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryAllocateInfoKHR where
        type FieldType "sType" VkExportMemoryAllocateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryAllocateInfoKHR =
             #{offset VkExportMemoryAllocateInfoKHR, sType}
        type FieldIsArray "sType" VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoKHR, sType}

instance CanReadField "sType" VkExportMemoryAllocateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExportMemoryAllocateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportMemoryAllocateInfoKHR where
        type VkPNextMType VkExportMemoryAllocateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryAllocateInfoKHR where
        type FieldType "pNext" VkExportMemoryAllocateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryAllocateInfoKHR =
             #{offset VkExportMemoryAllocateInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportMemoryAllocateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoKHR, pNext}

instance CanReadField "pNext" VkExportMemoryAllocateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExportMemoryAllocateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExportMemoryAllocateInfoKHR where
        type VkHandleTypesMType VkExportMemoryAllocateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoKHR, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExportMemoryAllocateInfoKHR, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoKHR, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportMemoryAllocateInfoKHR where
        type FieldType "handleTypes" VkExportMemoryAllocateInfoKHR =
             VkExternalMemoryHandleTypeFlagsKHR
        type FieldOptional "handleTypes" VkExportMemoryAllocateInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportMemoryAllocateInfoKHR =
             #{offset VkExportMemoryAllocateInfoKHR, handleTypes}
        type FieldIsArray "handleTypes" VkExportMemoryAllocateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoKHR, handleTypes}

instance CanReadField "handleTypes" VkExportMemoryAllocateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes" VkExportMemoryAllocateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExportMemoryAllocateInfoKHR where
        showsPrec d x
          = showString "VkExportMemoryAllocateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory\NUL"##

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME

type VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME =
     "VK_KHR_external_memory"

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR =
        VkStructureType 1000072000

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR =
        VkStructureType 1000072001

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR =
        VkStructureType 1000072002

pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR :: VkResult

pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR =
        VkResult (-1000072003)
