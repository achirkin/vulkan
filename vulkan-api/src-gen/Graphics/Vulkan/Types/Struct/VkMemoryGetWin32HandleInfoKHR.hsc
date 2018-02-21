#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryGetWin32HandleInfoKHR
       (VkMemoryGetWin32HandleInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                 (VkDeviceMemory)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceMemory                   memory;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   > } VkMemoryGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryGetWin32HandleInfoKHR.html VkMemoryGetWin32HandleInfoKHR registry at www.khronos.org>
data VkMemoryGetWin32HandleInfoKHR = VkMemoryGetWin32HandleInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkMemoryGetWin32HandleInfoKHR where
        (VkMemoryGetWin32HandleInfoKHR## a _) ==
          x@(VkMemoryGetWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryGetWin32HandleInfoKHR where
        (VkMemoryGetWin32HandleInfoKHR## a _) `compare`
          x@(VkMemoryGetWin32HandleInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryGetWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkMemoryGetWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryGetWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryGetWin32HandleInfoKHR where
        unsafeAddr (VkMemoryGetWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryGetWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryGetWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryGetWin32HandleInfoKHR where
        type StructFields VkMemoryGetWin32HandleInfoKHR =
             '["sType", "pNext", "memory", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryGetWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkMemoryGetWin32HandleInfoKHR where
        type VkSTypeMType VkMemoryGetWin32HandleInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryGetWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "sType" VkMemoryGetWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, sType}

instance CanReadField "sType" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryGetWin32HandleInfoKHR where
        type VkPNextMType VkMemoryGetWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "pNext" VkMemoryGetWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

instance CanReadField "pNext" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMemory VkMemoryGetWin32HandleInfoKHR where
        type VkMemoryMType VkMemoryGetWin32HandleInfoKHR = VkDeviceMemory

        {-# NOINLINE vkMemory #-}
        vkMemory x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, memory})

        {-# INLINE vkMemoryByteOffset #-}
        vkMemoryByteOffset ~_
          = #{offset VkMemoryGetWin32HandleInfoKHR, memory}

        {-# INLINE readVkMemory #-}
        readVkMemory p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, memory}

        {-# INLINE writeVkMemory #-}
        writeVkMemory p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, memory}

instance {-# OVERLAPPING #-}
         HasField "memory" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "memory" VkMemoryGetWin32HandleInfoKHR =
             VkDeviceMemory
        type FieldOptional "memory" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, memory}
        type FieldIsArray "memory" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, memory}

instance CanReadField "memory" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkMemory

        {-# INLINE readField #-}
        readField = readVkMemory

instance CanWriteField "memory" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkMemory

instance {-# OVERLAPPING #-}
         HasVkHandleType VkMemoryGetWin32HandleInfoKHR where
        type VkHandleTypeMType VkMemoryGetWin32HandleInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "handleType" VkMemoryGetWin32HandleInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkMemoryGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkMemoryGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

instance CanReadField "handleType" VkMemoryGetWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkMemoryGetWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance Show VkMemoryGetWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkMemoryGetWin32HandleInfoKHR {" .
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
