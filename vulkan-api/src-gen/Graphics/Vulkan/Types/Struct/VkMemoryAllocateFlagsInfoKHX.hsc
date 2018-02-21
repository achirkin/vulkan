#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryAllocateFlagsInfoKHX
       (VkMemoryAllocateFlagsInfoKHX(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlagsKHX (VkMemoryAllocateFlagsKHX)
import           Graphics.Vulkan.Types.Enum.VkStructureType          (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo   (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryAllocateFlagsInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMemoryAllocateFlagsKHX flags;
--   >     uint32_t                         deviceMask;
--   > } VkMemoryAllocateFlagsInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryAllocateFlagsInfoKHX.html VkMemoryAllocateFlagsInfoKHX registry at www.khronos.org>
data VkMemoryAllocateFlagsInfoKHX = VkMemoryAllocateFlagsInfoKHX## Addr##
                                                                  ByteArray##

instance Eq VkMemoryAllocateFlagsInfoKHX where
        (VkMemoryAllocateFlagsInfoKHX## a _) ==
          x@(VkMemoryAllocateFlagsInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryAllocateFlagsInfoKHX where
        (VkMemoryAllocateFlagsInfoKHX## a _) `compare`
          x@(VkMemoryAllocateFlagsInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryAllocateFlagsInfoKHX where
        sizeOf ~_ = #{size VkMemoryAllocateFlagsInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryAllocateFlagsInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryAllocateFlagsInfoKHX where
        unsafeAddr (VkMemoryAllocateFlagsInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryAllocateFlagsInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryAllocateFlagsInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryAllocateFlagsInfoKHX where
        type StructFields VkMemoryAllocateFlagsInfoKHX =
             '["sType", "pNext", "flags", "deviceMask"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryAllocateFlagsInfoKHX =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkMemoryAllocateFlagsInfoKHX where
        type VkSTypeMType VkMemoryAllocateFlagsInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryAllocateFlagsInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "sType" VkMemoryAllocateFlagsInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, sType}
        type FieldIsArray "sType" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, sType}

instance CanReadField "sType" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryAllocateFlagsInfoKHX where
        type VkPNextMType VkMemoryAllocateFlagsInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "pNext" VkMemoryAllocateFlagsInfoKHX = Ptr Void
        type FieldOptional "pNext" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, pNext}
        type FieldIsArray "pNext" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

instance CanReadField "pNext" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkMemoryAllocateFlagsInfoKHX where
        type VkFlagsMType VkMemoryAllocateFlagsInfoKHX =
             VkMemoryAllocateFlagsKHX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkMemoryAllocateFlagsInfoKHX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "flags" VkMemoryAllocateFlagsInfoKHX =
             VkMemoryAllocateFlagsKHX
        type FieldOptional "flags" VkMemoryAllocateFlagsInfoKHX = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, flags}
        type FieldIsArray "flags" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, flags}

instance CanReadField "flags" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkDeviceMask VkMemoryAllocateFlagsInfoKHX where
        type VkDeviceMaskMType VkMemoryAllocateFlagsInfoKHX = Word32

        {-# NOINLINE vkDeviceMask #-}
        vkDeviceMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask})

        {-# INLINE vkDeviceMaskByteOffset #-}
        vkDeviceMaskByteOffset ~_
          = #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

        {-# INLINE readVkDeviceMask #-}
        readVkDeviceMask p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

        {-# INLINE writeVkDeviceMask #-}
        writeVkDeviceMask p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "deviceMask" VkMemoryAllocateFlagsInfoKHX = Word32
        type FieldOptional "deviceMask" VkMemoryAllocateFlagsInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}
        type FieldIsArray "deviceMask" VkMemoryAllocateFlagsInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

instance CanReadField "deviceMask" VkMemoryAllocateFlagsInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDeviceMask

        {-# INLINE readField #-}
        readField = readVkDeviceMask

instance CanWriteField "deviceMask" VkMemoryAllocateFlagsInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDeviceMask

instance Show VkMemoryAllocateFlagsInfoKHX where
        showsPrec d x
          = showString "VkMemoryAllocateFlagsInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDeviceMask = " .
                                  showsPrec d (vkDeviceMask x) . showChar '}'
