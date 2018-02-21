#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportMemoryFdInfoKHR
       (VkImportMemoryFdInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo             (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

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
