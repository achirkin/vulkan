#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDedicatedAllocationBufferCreateInfoNV
       (VkDedicatedAllocationBufferCreateInfoNV(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                 (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBufferCreateInfo (VkBufferCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkDedicatedAllocationBufferCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         dedicatedAllocation;
--   > } VkDedicatedAllocationBufferCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDedicatedAllocationBufferCreateInfoNV.html VkDedicatedAllocationBufferCreateInfoNV registry at www.khronos.org>
data VkDedicatedAllocationBufferCreateInfoNV = VkDedicatedAllocationBufferCreateInfoNV## Addr##
                                                                                        ByteArray##

instance Eq VkDedicatedAllocationBufferCreateInfoNV where
        (VkDedicatedAllocationBufferCreateInfoNV## a _) ==
          x@(VkDedicatedAllocationBufferCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDedicatedAllocationBufferCreateInfoNV where
        (VkDedicatedAllocationBufferCreateInfoNV## a _) `compare`
          x@(VkDedicatedAllocationBufferCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDedicatedAllocationBufferCreateInfoNV where
        sizeOf ~_
          = #{size VkDedicatedAllocationBufferCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDedicatedAllocationBufferCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDedicatedAllocationBufferCreateInfoNV
         where
        unsafeAddr (VkDedicatedAllocationBufferCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDedicatedAllocationBufferCreateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDedicatedAllocationBufferCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDedicatedAllocationBufferCreateInfoNV
         where
        type StructFields VkDedicatedAllocationBufferCreateInfoNV =
             '["sType", "pNext", "dedicatedAllocation"] -- ' closing tick for hsc2hs
        type CUnionType VkDedicatedAllocationBufferCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDedicatedAllocationBufferCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDedicatedAllocationBufferCreateInfoNV =
             '[VkBufferCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDedicatedAllocationBufferCreateInfoNV where
        type VkSTypeMType VkDedicatedAllocationBufferCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationBufferCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDedicatedAllocationBufferCreateInfoNV where
        type FieldType "sType" VkDedicatedAllocationBufferCreateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDedicatedAllocationBufferCreateInfoNV =
             #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}
        type FieldIsArray "sType" VkDedicatedAllocationBufferCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

instance CanReadField "sType"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDedicatedAllocationBufferCreateInfoNV where
        type VkPNextMType VkDedicatedAllocationBufferCreateInfoNV =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDedicatedAllocationBufferCreateInfoNV where
        type FieldType "pNext" VkDedicatedAllocationBufferCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext" VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDedicatedAllocationBufferCreateInfoNV =
             #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}
        type FieldIsArray "pNext" VkDedicatedAllocationBufferCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

instance CanReadField "pNext"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDedicatedAllocation VkDedicatedAllocationBufferCreateInfoNV
         where
        type VkDedicatedAllocationMType
               VkDedicatedAllocationBufferCreateInfoNV
             = VkBool32

        {-# NOINLINE vkDedicatedAllocation #-}
        vkDedicatedAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation})

        {-# INLINE vkDedicatedAllocationByteOffset #-}
        vkDedicatedAllocationByteOffset ~_
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

        {-# INLINE readVkDedicatedAllocation #-}
        readVkDedicatedAllocation p
          = peekByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

        {-# INLINE writeVkDedicatedAllocation #-}
        writeVkDedicatedAllocation p
          = pokeByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

instance {-# OVERLAPPING #-}
         HasField "dedicatedAllocation"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        type FieldType "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             = VkBool32
        type FieldOptional "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             =
             #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}
        type FieldIsArray "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

instance CanReadField "dedicatedAllocation"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkDedicatedAllocation

        {-# INLINE readField #-}
        readField = readVkDedicatedAllocation

instance CanWriteField "dedicatedAllocation"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkDedicatedAllocation

instance Show VkDedicatedAllocationBufferCreateInfoNV where
        showsPrec d x
          = showString "VkDedicatedAllocationBufferCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDedicatedAllocation = " .
                            showsPrec d (vkDedicatedAllocation x) . showChar '}'
