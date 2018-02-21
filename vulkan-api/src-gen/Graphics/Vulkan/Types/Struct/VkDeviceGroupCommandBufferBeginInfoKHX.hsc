#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupCommandBufferBeginInfoKHX
       (VkDeviceGroupCommandBufferBeginInfoKHX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo (VkCommandBufferBeginInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupCommandBufferBeginInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   > } VkDeviceGroupCommandBufferBeginInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupCommandBufferBeginInfoKHX.html VkDeviceGroupCommandBufferBeginInfoKHX registry at www.khronos.org>
data VkDeviceGroupCommandBufferBeginInfoKHX = VkDeviceGroupCommandBufferBeginInfoKHX## Addr##
                                                                                      ByteArray##

instance Eq VkDeviceGroupCommandBufferBeginInfoKHX where
        (VkDeviceGroupCommandBufferBeginInfoKHX## a _) ==
          x@(VkDeviceGroupCommandBufferBeginInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupCommandBufferBeginInfoKHX where
        (VkDeviceGroupCommandBufferBeginInfoKHX## a _) `compare`
          x@(VkDeviceGroupCommandBufferBeginInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupCommandBufferBeginInfoKHX where
        sizeOf ~_
          = #{size VkDeviceGroupCommandBufferBeginInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupCommandBufferBeginInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupCommandBufferBeginInfoKHX
         where
        unsafeAddr (VkDeviceGroupCommandBufferBeginInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupCommandBufferBeginInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupCommandBufferBeginInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupCommandBufferBeginInfoKHX where
        type StructFields VkDeviceGroupCommandBufferBeginInfoKHX =
             '["sType", "pNext", "deviceMask"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupCommandBufferBeginInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupCommandBufferBeginInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupCommandBufferBeginInfoKHX =
             '[VkCommandBufferBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupCommandBufferBeginInfoKHX where
        type VkSTypeMType VkDeviceGroupCommandBufferBeginInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupCommandBufferBeginInfoKHX where
        type FieldType "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

instance CanReadField "sType"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupCommandBufferBeginInfoKHX where
        type VkPNextMType VkDeviceGroupCommandBufferBeginInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupCommandBufferBeginInfoKHX where
        type FieldType "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

instance CanReadField "pNext"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDeviceMask VkDeviceGroupCommandBufferBeginInfoKHX where
        type VkDeviceMaskMType VkDeviceGroupCommandBufferBeginInfoKHX =
             Word32

        {-# NOINLINE vkDeviceMask #-}
        vkDeviceMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask})

        {-# INLINE vkDeviceMaskByteOffset #-}
        vkDeviceMaskByteOffset ~_
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

        {-# INLINE readVkDeviceMask #-}
        readVkDeviceMask p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

        {-# INLINE writeVkDeviceMask #-}
        writeVkDeviceMask p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkDeviceGroupCommandBufferBeginInfoKHX where
        type FieldType "deviceMask" VkDeviceGroupCommandBufferBeginInfoKHX
             = Word32
        type FieldOptional "deviceMask"
               VkDeviceGroupCommandBufferBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask"
               VkDeviceGroupCommandBufferBeginInfoKHX
             =
             #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}
        type FieldIsArray "deviceMask"
               VkDeviceGroupCommandBufferBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

instance CanReadField "deviceMask"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceMask

        {-# INLINE readField #-}
        readField = readVkDeviceMask

instance CanWriteField "deviceMask"
           VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceMask

instance Show VkDeviceGroupCommandBufferBeginInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupCommandBufferBeginInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDeviceMask = " .
                            showsPrec d (vkDeviceMask x) . showChar '}'
