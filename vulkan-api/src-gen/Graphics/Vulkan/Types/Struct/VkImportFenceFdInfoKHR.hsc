#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportFenceFdInfoKHR
       (VkImportFenceFdInfoKHR(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR (VkExternalFenceHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkFenceImportFlagsKHR             (VkFenceImportFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                (VkFence)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

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
