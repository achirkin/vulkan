#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfoKHR
       (VkPhysicalDeviceExternalBufferInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkBufferCreateFlags                (VkBufferCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkBufferUsageFlags                 (VkBufferUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalBufferInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBufferCreateFlags flags;
--   >     VkBufferUsageFlags               usage;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   > } VkPhysicalDeviceExternalBufferInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceExternalBufferInfoKHR.html VkPhysicalDeviceExternalBufferInfoKHR registry at www.khronos.org>
data VkPhysicalDeviceExternalBufferInfoKHR = VkPhysicalDeviceExternalBufferInfoKHR## Addr##
                                                                                    ByteArray##

instance Eq VkPhysicalDeviceExternalBufferInfoKHR where
        (VkPhysicalDeviceExternalBufferInfoKHR## a _) ==
          x@(VkPhysicalDeviceExternalBufferInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalBufferInfoKHR where
        (VkPhysicalDeviceExternalBufferInfoKHR## a _) `compare`
          x@(VkPhysicalDeviceExternalBufferInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalBufferInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalBufferInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalBufferInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceExternalBufferInfoKHR
         where
        unsafeAddr (VkPhysicalDeviceExternalBufferInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalBufferInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalBufferInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalBufferInfoKHR where
        type StructFields VkPhysicalDeviceExternalBufferInfoKHR =
             '["sType", "pNext", "flags", "usage", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalBufferInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalBufferInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalBufferInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceExternalBufferInfoKHR where
        type VkSTypeMType VkPhysicalDeviceExternalBufferInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "sType" VkPhysicalDeviceExternalBufferInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalBufferInfoKHR =
             #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}

instance CanReadField "sType" VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceExternalBufferInfoKHR where
        type VkPNextMType VkPhysicalDeviceExternalBufferInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "pNext" VkPhysicalDeviceExternalBufferInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalBufferInfoKHR =
             #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}

instance CanReadField "pNext" VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPhysicalDeviceExternalBufferInfoKHR where
        type VkFlagsMType VkPhysicalDeviceExternalBufferInfoKHR =
             VkBufferCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "flags" VkPhysicalDeviceExternalBufferInfoKHR =
             VkBufferCreateFlags
        type FieldOptional "flags" VkPhysicalDeviceExternalBufferInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPhysicalDeviceExternalBufferInfoKHR =
             #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}
        type FieldIsArray "flags" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}

instance CanReadField "flags" VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkUsage VkPhysicalDeviceExternalBufferInfoKHR where
        type VkUsageMType VkPhysicalDeviceExternalBufferInfoKHR =
             VkBufferUsageFlags

        {-# NOINLINE vkUsage #-}
        vkUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage})

        {-# INLINE vkUsageByteOffset #-}
        vkUsageByteOffset ~_
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}

        {-# INLINE readVkUsage #-}
        readVkUsage p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}

        {-# INLINE writeVkUsage #-}
        writeVkUsage p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "usage" VkPhysicalDeviceExternalBufferInfoKHR =
             VkBufferUsageFlags
        type FieldOptional "usage" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkPhysicalDeviceExternalBufferInfoKHR =
             #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}
        type FieldIsArray "usage" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}

instance CanReadField "usage" VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkUsage

        {-# INLINE readField #-}
        readField = readVkUsage

instance CanWriteField "usage"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkUsage

instance {-# OVERLAPPING #-}
         HasVkHandleType VkPhysicalDeviceExternalBufferInfoKHR where
        type VkHandleTypeMType VkPhysicalDeviceExternalBufferInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "handleType" VkPhysicalDeviceExternalBufferInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalBufferInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkPhysicalDeviceExternalBufferInfoKHR
             =
             #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}
        type FieldIsArray "handleType"
               VkPhysicalDeviceExternalBufferInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}

instance CanReadField "handleType"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType"
           VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance Show VkPhysicalDeviceExternalBufferInfoKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalBufferInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkUsage = " .
                                  showsPrec d (vkUsage x) .
                                    showString ", " .
                                      showString "vkHandleType = " .
                                        showsPrec d (vkHandleType x) . showChar '}'
