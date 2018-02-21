#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkFenceGetWin32HandleInfoKHR
       (VkFenceGetWin32HandleInfoKHR(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR (VkExternalFenceHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                (VkFence)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkFenceGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence                                fence;
--   >     VkExternalFenceHandleTypeFlagBitsKHR   handleType;
--   > } VkFenceGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkFenceGetWin32HandleInfoKHR.html VkFenceGetWin32HandleInfoKHR registry at www.khronos.org>
data VkFenceGetWin32HandleInfoKHR = VkFenceGetWin32HandleInfoKHR## Addr##
                                                                  ByteArray##

instance Eq VkFenceGetWin32HandleInfoKHR where
        (VkFenceGetWin32HandleInfoKHR## a _) ==
          x@(VkFenceGetWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFenceGetWin32HandleInfoKHR where
        (VkFenceGetWin32HandleInfoKHR## a _) `compare`
          x@(VkFenceGetWin32HandleInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFenceGetWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkFenceGetWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkFenceGetWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFenceGetWin32HandleInfoKHR where
        unsafeAddr (VkFenceGetWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFenceGetWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFenceGetWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFenceGetWin32HandleInfoKHR where
        type StructFields VkFenceGetWin32HandleInfoKHR =
             '["sType", "pNext", "fence", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkFenceGetWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkFenceGetWin32HandleInfoKHR where
        type VkSTypeMType VkFenceGetWin32HandleInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkFenceGetWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkFenceGetWin32HandleInfoKHR where
        type FieldType "sType" VkFenceGetWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkFenceGetWin32HandleInfoKHR =
             #{offset VkFenceGetWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFenceGetWin32HandleInfoKHR, sType}

instance CanReadField "sType" VkFenceGetWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkFenceGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkFenceGetWin32HandleInfoKHR where
        type VkPNextMType VkFenceGetWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkFenceGetWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkFenceGetWin32HandleInfoKHR where
        type FieldType "pNext" VkFenceGetWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkFenceGetWin32HandleInfoKHR =
             #{offset VkFenceGetWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFenceGetWin32HandleInfoKHR, pNext}

instance CanReadField "pNext" VkFenceGetWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkFenceGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFence VkFenceGetWin32HandleInfoKHR where
        type VkFenceMType VkFenceGetWin32HandleInfoKHR = VkFence

        {-# NOINLINE vkFence #-}
        vkFence x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, fence})

        {-# INLINE vkFenceByteOffset #-}
        vkFenceByteOffset ~_
          = #{offset VkFenceGetWin32HandleInfoKHR, fence}

        {-# INLINE readVkFence #-}
        readVkFence p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, fence}

        {-# INLINE writeVkFence #-}
        writeVkFence p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasField "fence" VkFenceGetWin32HandleInfoKHR where
        type FieldType "fence" VkFenceGetWin32HandleInfoKHR = VkFence
        type FieldOptional "fence" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fence" VkFenceGetWin32HandleInfoKHR =
             #{offset VkFenceGetWin32HandleInfoKHR, fence}
        type FieldIsArray "fence" VkFenceGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFenceGetWin32HandleInfoKHR, fence}

instance CanReadField "fence" VkFenceGetWin32HandleInfoKHR where
        {-# INLINE getField #-}
        getField = vkFence

        {-# INLINE readField #-}
        readField = readVkFence

instance CanWriteField "fence" VkFenceGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFence

instance {-# OVERLAPPING #-}
         HasVkHandleType VkFenceGetWin32HandleInfoKHR where
        type VkHandleTypeMType VkFenceGetWin32HandleInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetWin32HandleInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkFenceGetWin32HandleInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkFenceGetWin32HandleInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkFenceGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkFenceGetWin32HandleInfoKHR where
        type FieldType "handleType" VkFenceGetWin32HandleInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkFenceGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkFenceGetWin32HandleInfoKHR =
             #{offset VkFenceGetWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkFenceGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFenceGetWin32HandleInfoKHR, handleType}

instance CanReadField "handleType" VkFenceGetWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkFenceGetWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance Show VkFenceGetWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkFenceGetWin32HandleInfoKHR {" .
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
