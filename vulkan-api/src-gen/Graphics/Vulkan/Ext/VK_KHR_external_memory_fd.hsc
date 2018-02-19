#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_external_memory_fd
       (-- * Vulkan extension: @VK_KHR_external_memory_fd@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHX@
        --
        -- type: @device@
        --
        -- Extension number: @75@
        --
        -- Required extensions: 'VK_KHR_external_memory'.
        --

        -- ** Required extensions: 'VK_KHR_external_memory'.
        VkImportMemoryFdInfoKHR(..), VkMemoryFdPropertiesKHR(..),
        VkMemoryGetFdInfoKHR(..), vkGetMemoryFdKHR,
        vkGetMemoryFdPropertiesKHR, VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION,
        VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR,
        pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR)
       where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkImportMemoryFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   >     int                              fd;
--   > } VkImportMemoryFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImportMemoryFdInfoKHR.html VkImportMemoryFdInfoKHR registry at www.khronos.org>
data VkImportMemoryFdInfoKHR = VkImportMemoryFdInfoKHR## Addr##
                                                        ByteArray##

instance Eq VkImportMemoryFdInfoKHR where
        (VkImportMemoryFdInfoKHR## a _) == x@(VkImportMemoryFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryFdInfoKHR where
        (VkImportMemoryFdInfoKHR## a _) `compare`
          x@(VkImportMemoryFdInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryFdInfoKHR where
        sizeOf ~_ = #{size VkImportMemoryFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImportMemoryFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportMemoryFdInfoKHR where
        unsafeAddr (VkImportMemoryFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportMemoryFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportMemoryFdInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportMemoryFdInfoKHR where
        type StructFields VkImportMemoryFdInfoKHR =
             '["sType", "pNext", "handleType", "fd"] -- ' closing tick for hsc2hs
        type CUnionType VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportMemoryFdInfoKHR =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkImportMemoryFdInfoKHR
         where
        type VkSTypeMType VkImportMemoryFdInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryFdInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportMemoryFdInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportMemoryFdInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportMemoryFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryFdInfoKHR where
        type FieldType "sType" VkImportMemoryFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryFdInfoKHR =
             #{offset VkImportMemoryFdInfoKHR, sType}
        type FieldIsArray "sType" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportMemoryFdInfoKHR, sType}

instance CanReadField "sType" VkImportMemoryFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImportMemoryFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkImportMemoryFdInfoKHR
         where
        type VkPNextMType VkImportMemoryFdInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryFdInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportMemoryFdInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportMemoryFdInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportMemoryFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryFdInfoKHR where
        type FieldType "pNext" VkImportMemoryFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryFdInfoKHR =
             #{offset VkImportMemoryFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportMemoryFdInfoKHR, pNext}

instance CanReadField "pNext" VkImportMemoryFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImportMemoryFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleType VkImportMemoryFdInfoKHR where
        type VkHandleTypeMType VkImportMemoryFdInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryFdInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportMemoryFdInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportMemoryFdInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportMemoryFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryFdInfoKHR where
        type FieldType "handleType" VkImportMemoryFdInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkImportMemoryFdInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryFdInfoKHR =
             #{offset VkImportMemoryFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryFdInfoKHR, handleType}

instance CanReadField "handleType" VkImportMemoryFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkImportMemoryFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance {-# OVERLAPPING #-} HasVkFd VkImportMemoryFdInfoKHR where
        type VkFdMType VkImportMemoryFdInfoKHR = CInt

        {-# NOINLINE vkFd #-}
        vkFd x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryFdInfoKHR, fd})

        {-# INLINE vkFdByteOffset #-}
        vkFdByteOffset ~_
          = #{offset VkImportMemoryFdInfoKHR, fd}

        {-# INLINE readVkFd #-}
        readVkFd p
          = peekByteOff p #{offset VkImportMemoryFdInfoKHR, fd}

        {-# INLINE writeVkFd #-}
        writeVkFd p
          = pokeByteOff p #{offset VkImportMemoryFdInfoKHR, fd}

instance {-# OVERLAPPING #-} HasField "fd" VkImportMemoryFdInfoKHR
         where
        type FieldType "fd" VkImportMemoryFdInfoKHR = CInt
        type FieldOptional "fd" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fd" VkImportMemoryFdInfoKHR =
             #{offset VkImportMemoryFdInfoKHR, fd}
        type FieldIsArray "fd" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportMemoryFdInfoKHR, fd}

instance CanReadField "fd" VkImportMemoryFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFd

        {-# INLINE readField #-}
        readField = readVkFd

instance CanWriteField "fd" VkImportMemoryFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFd

instance Show VkImportMemoryFdInfoKHR where
        showsPrec d x
          = showString "VkImportMemoryFdInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleType = " .
                            showsPrec d (vkHandleType x) .
                              showString ", " .
                                showString "vkFd = " . showsPrec d (vkFd x) . showChar '}'

-- | > typedef struct VkMemoryFdPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         memoryTypeBits;
--   > } VkMemoryFdPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryFdPropertiesKHR.html VkMemoryFdPropertiesKHR registry at www.khronos.org>
data VkMemoryFdPropertiesKHR = VkMemoryFdPropertiesKHR## Addr##
                                                        ByteArray##

instance Eq VkMemoryFdPropertiesKHR where
        (VkMemoryFdPropertiesKHR## a _) == x@(VkMemoryFdPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryFdPropertiesKHR where
        (VkMemoryFdPropertiesKHR## a _) `compare`
          x@(VkMemoryFdPropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryFdPropertiesKHR where
        sizeOf ~_ = #{size VkMemoryFdPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryFdPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryFdPropertiesKHR where
        unsafeAddr (VkMemoryFdPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryFdPropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryFdPropertiesKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryFdPropertiesKHR where
        type StructFields VkMemoryFdPropertiesKHR =
             '["sType", "pNext", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryFdPropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryFdPropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkMemoryFdPropertiesKHR
         where
        type VkSTypeMType VkMemoryFdPropertiesKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryFdPropertiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryFdPropertiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryFdPropertiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryFdPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryFdPropertiesKHR where
        type FieldType "sType" VkMemoryFdPropertiesKHR = VkStructureType
        type FieldOptional "sType" VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryFdPropertiesKHR =
             #{offset VkMemoryFdPropertiesKHR, sType}
        type FieldIsArray "sType" VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryFdPropertiesKHR, sType}

instance CanReadField "sType" VkMemoryFdPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMemoryFdPropertiesKHR
         where
        type VkPNextMType VkMemoryFdPropertiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryFdPropertiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryFdPropertiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryFdPropertiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryFdPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryFdPropertiesKHR where
        type FieldType "pNext" VkMemoryFdPropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryFdPropertiesKHR =
             #{offset VkMemoryFdPropertiesKHR, pNext}
        type FieldIsArray "pNext" VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryFdPropertiesKHR, pNext}

instance CanReadField "pNext" VkMemoryFdPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkMemoryTypeBits VkMemoryFdPropertiesKHR where
        type VkMemoryTypeBitsMType VkMemoryFdPropertiesKHR = Word32

        {-# NOINLINE vkMemoryTypeBits #-}
        vkMemoryTypeBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryFdPropertiesKHR, memoryTypeBits})

        {-# INLINE vkMemoryTypeBitsByteOffset #-}
        vkMemoryTypeBitsByteOffset ~_
          = #{offset VkMemoryFdPropertiesKHR, memoryTypeBits}

        {-# INLINE readVkMemoryTypeBits #-}
        readVkMemoryTypeBits p
          = peekByteOff p #{offset VkMemoryFdPropertiesKHR, memoryTypeBits}

        {-# INLINE writeVkMemoryTypeBits #-}
        writeVkMemoryTypeBits p
          = pokeByteOff p #{offset VkMemoryFdPropertiesKHR, memoryTypeBits}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryFdPropertiesKHR where
        type FieldType "memoryTypeBits" VkMemoryFdPropertiesKHR = Word32
        type FieldOptional "memoryTypeBits" VkMemoryFdPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits" VkMemoryFdPropertiesKHR =
             #{offset VkMemoryFdPropertiesKHR, memoryTypeBits}
        type FieldIsArray "memoryTypeBits" VkMemoryFdPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryFdPropertiesKHR, memoryTypeBits}

instance CanReadField "memoryTypeBits" VkMemoryFdPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMemoryTypeBits

        {-# INLINE readField #-}
        readField = readVkMemoryTypeBits

instance Show VkMemoryFdPropertiesKHR where
        showsPrec d x
          = showString "VkMemoryFdPropertiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemoryTypeBits = " .
                            showsPrec d (vkMemoryTypeBits x) . showChar '}'

-- | > typedef struct VkMemoryGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceMemory                   memory;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   > } VkMemoryGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryGetFdInfoKHR.html VkMemoryGetFdInfoKHR registry at www.khronos.org>
data VkMemoryGetFdInfoKHR = VkMemoryGetFdInfoKHR## Addr## ByteArray##

instance Eq VkMemoryGetFdInfoKHR where
        (VkMemoryGetFdInfoKHR## a _) == x@(VkMemoryGetFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryGetFdInfoKHR where
        (VkMemoryGetFdInfoKHR## a _) `compare` x@(VkMemoryGetFdInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryGetFdInfoKHR where
        sizeOf ~_ = #{size VkMemoryGetFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryGetFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryGetFdInfoKHR where
        unsafeAddr (VkMemoryGetFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryGetFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryGetFdInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryGetFdInfoKHR where
        type StructFields VkMemoryGetFdInfoKHR =
             '["sType", "pNext", "memory", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryGetFdInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkMemoryGetFdInfoKHR where
        type VkSTypeMType VkMemoryGetFdInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryGetFdInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkMemoryGetFdInfoKHR
         where
        type FieldType "sType" VkMemoryGetFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, sType}
        type FieldIsArray "sType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryGetFdInfoKHR, sType}

instance CanReadField "sType" VkMemoryGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMemoryGetFdInfoKHR where
        type VkPNextMType VkMemoryGetFdInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryGetFdInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkMemoryGetFdInfoKHR
         where
        type FieldType "pNext" VkMemoryGetFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryGetFdInfoKHR, pNext}

instance CanReadField "pNext" VkMemoryGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkMemory VkMemoryGetFdInfoKHR where
        type VkMemoryMType VkMemoryGetFdInfoKHR = VkDeviceMemory

        {-# NOINLINE vkMemory #-}
        vkMemory x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, memory})

        {-# INLINE vkMemoryByteOffset #-}
        vkMemoryByteOffset ~_
          = #{offset VkMemoryGetFdInfoKHR, memory}

        {-# INLINE readVkMemory #-}
        readVkMemory p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, memory}

        {-# INLINE writeVkMemory #-}
        writeVkMemory p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, memory}

instance {-# OVERLAPPING #-} HasField "memory" VkMemoryGetFdInfoKHR
         where
        type FieldType "memory" VkMemoryGetFdInfoKHR = VkDeviceMemory
        type FieldOptional "memory" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, memory}
        type FieldIsArray "memory" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryGetFdInfoKHR, memory}

instance CanReadField "memory" VkMemoryGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkMemory

        {-# INLINE readField #-}
        readField = readVkMemory

instance CanWriteField "memory" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkMemory

instance {-# OVERLAPPING #-} HasVkHandleType VkMemoryGetFdInfoKHR
         where
        type VkHandleTypeMType VkMemoryGetFdInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkMemoryGetFdInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkMemoryGetFdInfoKHR where
        type FieldType "handleType" VkMemoryGetFdInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetFdInfoKHR, handleType}

instance CanReadField "handleType" VkMemoryGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance Show VkMemoryGetFdInfoKHR where
        showsPrec d x
          = showString "VkMemoryGetFdInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemory = " .
                            showsPrec d (vkMemory x) .
                              showString ", " .
                                showString "vkHandleType = " .
                                  showsPrec d (vkHandleType x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetMemoryFdKHR
--   >     ( VkDevice device
--   >     , const VkMemoryGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetMemoryFdKHR.html vkGetMemoryFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryFdKHR" vkGetMemoryFdKHR ::
               VkDevice -- ^ device
                        -> Ptr VkMemoryGetFdInfoKHR -- ^ pGetFdInfo
                                                    -> Ptr CInt -- ^ pFd
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkGetMemoryFdPropertiesKHR
--   >     ( VkDevice device
--   >     , VkExternalMemoryHandleTypeFlagBitsKHR handleType
--   >     , int fd
--   >     , VkMemoryFdPropertiesKHR* pMemoryFdProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetMemoryFdPropertiesKHR.html vkGetMemoryFdPropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetMemoryFdPropertiesKHR"
               vkGetMemoryFdPropertiesKHR ::
               VkDevice -- ^ device
                        ->
                 VkExternalMemoryHandleTypeFlagBitsKHR -- ^ handleType
                                                       ->
                   CInt -- ^ fd
                        -> Ptr VkMemoryFdPropertiesKHR -- ^ pMemoryFdProperties
                                                       -> IO VkResult

pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  = Ptr "VK_KHR_external_memory_fd\NUL"##

{-# INLINE is_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME

type VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME =
     "VK_KHR_external_memory_fd"

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR =
        VkStructureType 1000074000

pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR =
        VkStructureType 1000074001

pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR =
        VkStructureType 1000074002
