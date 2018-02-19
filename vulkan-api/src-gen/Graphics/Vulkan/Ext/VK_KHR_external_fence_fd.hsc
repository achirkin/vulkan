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
module Graphics.Vulkan.Ext.VK_KHR_external_fence_fd
       (-- * Vulkan extension: @VK_KHR_external_fence_fd@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @116@
        --
        -- Required extensions: 'VK_KHR_external_fence'.
        --

        -- ** Required extensions: 'VK_KHR_external_fence'.
        VkImportFenceFdInfoKHR(..), VkFenceGetFdInfoKHR(..),
        vkImportFenceFdKHR, vkGetFenceFdKHR,
        VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION,
        VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR)
       where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkImportFenceFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence              fence;
--   >     VkFenceImportFlagsKHR  flags;
--   >     VkExternalFenceHandleTypeFlagBitsKHR   handleType;
--   >     int                                    fd;
--   > } VkImportFenceFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImportFenceFdInfoKHR.html VkImportFenceFdInfoKHR registry at www.khronos.org>
data VkImportFenceFdInfoKHR = VkImportFenceFdInfoKHR## Addr##
                                                      ByteArray##

instance Eq VkImportFenceFdInfoKHR where
        (VkImportFenceFdInfoKHR## a _) == x@(VkImportFenceFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportFenceFdInfoKHR where
        (VkImportFenceFdInfoKHR## a _) `compare`
          x@(VkImportFenceFdInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportFenceFdInfoKHR where
        sizeOf ~_ = #{size VkImportFenceFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImportFenceFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportFenceFdInfoKHR where
        unsafeAddr (VkImportFenceFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportFenceFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportFenceFdInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportFenceFdInfoKHR where
        type StructFields VkImportFenceFdInfoKHR =
             '["sType", "pNext", "fence", "flags", "handleType", "fd"] -- ' closing tick for hsc2hs
        type CUnionType VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportFenceFdInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkImportFenceFdInfoKHR
         where
        type VkSTypeMType VkImportFenceFdInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportFenceFdInfoKHR where
        type FieldType "sType" VkImportFenceFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, sType}
        type FieldIsArray "sType" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportFenceFdInfoKHR, sType}

instance CanReadField "sType" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkImportFenceFdInfoKHR
         where
        type VkPNextMType VkImportFenceFdInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportFenceFdInfoKHR where
        type FieldType "pNext" VkImportFenceFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportFenceFdInfoKHR, pNext}

instance CanReadField "pNext" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFence VkImportFenceFdInfoKHR
         where
        type VkFenceMType VkImportFenceFdInfoKHR = VkFence

        {-# NOINLINE vkFence #-}
        vkFence x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, fence})

        {-# INLINE vkFenceByteOffset #-}
        vkFenceByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, fence}

        {-# INLINE readVkFence #-}
        readVkFence p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, fence}

        {-# INLINE writeVkFence #-}
        writeVkFence p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasField "fence" VkImportFenceFdInfoKHR where
        type FieldType "fence" VkImportFenceFdInfoKHR = VkFence
        type FieldOptional "fence" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fence" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, fence}
        type FieldIsArray "fence" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportFenceFdInfoKHR, fence}

instance CanReadField "fence" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFence

        {-# INLINE readField #-}
        readField = readVkFence

instance CanWriteField "fence" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFence

instance {-# OVERLAPPING #-} HasVkFlags VkImportFenceFdInfoKHR
         where
        type VkFlagsMType VkImportFenceFdInfoKHR = VkFenceImportFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkImportFenceFdInfoKHR where
        type FieldType "flags" VkImportFenceFdInfoKHR =
             VkFenceImportFlagsKHR
        type FieldOptional "flags" VkImportFenceFdInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, flags}
        type FieldIsArray "flags" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportFenceFdInfoKHR, flags}

instance CanReadField "flags" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkHandleType VkImportFenceFdInfoKHR
         where
        type VkHandleTypeMType VkImportFenceFdInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportFenceFdInfoKHR where
        type FieldType "handleType" VkImportFenceFdInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceFdInfoKHR, handleType}

instance CanReadField "handleType" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance {-# OVERLAPPING #-} HasVkFd VkImportFenceFdInfoKHR where
        type VkFdMType VkImportFenceFdInfoKHR = CInt

        {-# NOINLINE vkFd #-}
        vkFd x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, fd})

        {-# INLINE vkFdByteOffset #-}
        vkFdByteOffset ~_
          = #{offset VkImportFenceFdInfoKHR, fd}

        {-# INLINE readVkFd #-}
        readVkFd p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, fd}

        {-# INLINE writeVkFd #-}
        writeVkFd p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, fd}

instance {-# OVERLAPPING #-} HasField "fd" VkImportFenceFdInfoKHR
         where
        type FieldType "fd" VkImportFenceFdInfoKHR = CInt
        type FieldOptional "fd" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fd" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, fd}
        type FieldIsArray "fd" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportFenceFdInfoKHR, fd}

instance CanReadField "fd" VkImportFenceFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFd

        {-# INLINE readField #-}
        readField = readVkFd

instance CanWriteField "fd" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFd

instance Show VkImportFenceFdInfoKHR where
        showsPrec d x
          = showString "VkImportFenceFdInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFence = " .
                            showsPrec d (vkFence x) .
                              showString ", " .
                                showString "vkFlags = " .
                                  showsPrec d (vkFlags x) .
                                    showString ", " .
                                      showString "vkHandleType = " .
                                        showsPrec d (vkHandleType x) .
                                          showString ", " .
                                            showString "vkFd = " .
                                              showsPrec d (vkFd x) . showChar '}'

-- | > typedef struct VkFenceGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence                                fence;
--   >     VkExternalFenceHandleTypeFlagBitsKHR   handleType;
--   > } VkFenceGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkFenceGetFdInfoKHR.html VkFenceGetFdInfoKHR registry at www.khronos.org>
data VkFenceGetFdInfoKHR = VkFenceGetFdInfoKHR## Addr## ByteArray##

instance Eq VkFenceGetFdInfoKHR where
        (VkFenceGetFdInfoKHR## a _) == x@(VkFenceGetFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFenceGetFdInfoKHR where
        (VkFenceGetFdInfoKHR## a _) `compare` x@(VkFenceGetFdInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFenceGetFdInfoKHR where
        sizeOf ~_ = #{size VkFenceGetFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFenceGetFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFenceGetFdInfoKHR where
        unsafeAddr (VkFenceGetFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFenceGetFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFenceGetFdInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFenceGetFdInfoKHR where
        type StructFields VkFenceGetFdInfoKHR =
             '["sType", "pNext", "fence", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkFenceGetFdInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkFenceGetFdInfoKHR where
        type VkSTypeMType VkFenceGetFdInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkFenceGetFdInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkFenceGetFdInfoKHR
         where
        type FieldType "sType" VkFenceGetFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkFenceGetFdInfoKHR =
             #{offset VkFenceGetFdInfoKHR, sType}
        type FieldIsArray "sType" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceGetFdInfoKHR, sType}

instance CanReadField "sType" VkFenceGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkFenceGetFdInfoKHR where
        type VkPNextMType VkFenceGetFdInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkFenceGetFdInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkFenceGetFdInfoKHR
         where
        type FieldType "pNext" VkFenceGetFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkFenceGetFdInfoKHR =
             #{offset VkFenceGetFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceGetFdInfoKHR, pNext}

instance CanReadField "pNext" VkFenceGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFence VkFenceGetFdInfoKHR where
        type VkFenceMType VkFenceGetFdInfoKHR = VkFence

        {-# NOINLINE vkFence #-}
        vkFence x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, fence})

        {-# INLINE vkFenceByteOffset #-}
        vkFenceByteOffset ~_
          = #{offset VkFenceGetFdInfoKHR, fence}

        {-# INLINE readVkFence #-}
        readVkFence p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, fence}

        {-# INLINE writeVkFence #-}
        writeVkFence p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, fence}

instance {-# OVERLAPPING #-} HasField "fence" VkFenceGetFdInfoKHR
         where
        type FieldType "fence" VkFenceGetFdInfoKHR = VkFence
        type FieldOptional "fence" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fence" VkFenceGetFdInfoKHR =
             #{offset VkFenceGetFdInfoKHR, fence}
        type FieldIsArray "fence" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceGetFdInfoKHR, fence}

instance CanReadField "fence" VkFenceGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkFence

        {-# INLINE readField #-}
        readField = readVkFence

instance CanWriteField "fence" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFence

instance {-# OVERLAPPING #-} HasVkHandleType VkFenceGetFdInfoKHR
         where
        type VkHandleTypeMType VkFenceGetFdInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkFenceGetFdInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkFenceGetFdInfoKHR where
        type FieldType "handleType" VkFenceGetFdInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkFenceGetFdInfoKHR =
             #{offset VkFenceGetFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceGetFdInfoKHR, handleType}

instance CanReadField "handleType" VkFenceGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance Show VkFenceGetFdInfoKHR where
        showsPrec d x
          = showString "VkFenceGetFdInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFence = " .
                            showsPrec d (vkFence x) .
                              showString ", " .
                                showString "vkHandleType = " .
                                  showsPrec d (vkHandleType x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR'.
--
--   > VkResult vkImportFenceFdKHR
--   >     ( VkDevice device
--   >     , const VkImportFenceFdInfoKHR* pImportFenceFdInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkImportFenceFdKHR.html vkImportFenceFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkImportFenceFdKHR" vkImportFenceFdKHR
               :: VkDevice -- ^ device
                           -> Ptr VkImportFenceFdInfoKHR -- ^ pImportFenceFdInfo
                                                         -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_TOO_MANY_OBJECTS', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetFenceFdKHR
--   >     ( VkDevice device
--   >     , const VkFenceGetFdInfoKHR* pGetFdInfo
--   >     , int* pFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetFenceFdKHR.html vkGetFenceFdKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetFenceFdKHR" vkGetFenceFdKHR ::
               VkDevice -- ^ device
                        -> Ptr VkFenceGetFdInfoKHR -- ^ pGetFdInfo
                                                   -> Ptr CInt -- ^ pFd
                                                               -> IO VkResult

pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  = Ptr "VK_KHR_external_fence_fd\NUL"##

{-# INLINE is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME

type VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME =
     "VK_KHR_external_fence_fd"

pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR =
        VkStructureType 1000115000

pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR =
        VkStructureType 1000115001
