#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDedicatedAllocationImageCreateInfoNV
       (VkDedicatedAllocationImageCreateInfoNV(..)) where
import           Foreign.Storable                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo (VkImageCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

-- | > typedef struct VkDedicatedAllocationImageCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         dedicatedAllocation;
--   > } VkDedicatedAllocationImageCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDedicatedAllocationImageCreateInfoNV.html VkDedicatedAllocationImageCreateInfoNV registry at www.khronos.org>
data VkDedicatedAllocationImageCreateInfoNV = VkDedicatedAllocationImageCreateInfoNV## Addr##
                                                                                      ByteArray##

instance Eq VkDedicatedAllocationImageCreateInfoNV where
        (VkDedicatedAllocationImageCreateInfoNV## a _) ==
          x@(VkDedicatedAllocationImageCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDedicatedAllocationImageCreateInfoNV where
        (VkDedicatedAllocationImageCreateInfoNV## a _) `compare`
          x@(VkDedicatedAllocationImageCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDedicatedAllocationImageCreateInfoNV where
        sizeOf ~_
          = #{size VkDedicatedAllocationImageCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDedicatedAllocationImageCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDedicatedAllocationImageCreateInfoNV
         where
        unsafeAddr (VkDedicatedAllocationImageCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDedicatedAllocationImageCreateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDedicatedAllocationImageCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDedicatedAllocationImageCreateInfoNV where
        type StructFields VkDedicatedAllocationImageCreateInfoNV =
             '["sType", "pNext", "dedicatedAllocation"] -- ' closing tick for hsc2hs
        type CUnionType VkDedicatedAllocationImageCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDedicatedAllocationImageCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDedicatedAllocationImageCreateInfoNV =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDedicatedAllocationImageCreateInfoNV where
        type VkSTypeMType VkDedicatedAllocationImageCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationImageCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDedicatedAllocationImageCreateInfoNV where
        type FieldType "sType" VkDedicatedAllocationImageCreateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkDedicatedAllocationImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDedicatedAllocationImageCreateInfoNV =
             #{offset VkDedicatedAllocationImageCreateInfoNV, sType}
        type FieldIsArray "sType" VkDedicatedAllocationImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

instance CanReadField "sType"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDedicatedAllocationImageCreateInfoNV where
        type VkPNextMType VkDedicatedAllocationImageCreateInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationImageCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDedicatedAllocationImageCreateInfoNV where
        type FieldType "pNext" VkDedicatedAllocationImageCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext" VkDedicatedAllocationImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDedicatedAllocationImageCreateInfoNV =
             #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}
        type FieldIsArray "pNext" VkDedicatedAllocationImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

instance CanReadField "pNext"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDedicatedAllocation VkDedicatedAllocationImageCreateInfoNV
         where
        type VkDedicatedAllocationMType
               VkDedicatedAllocationImageCreateInfoNV
             = VkBool32

        {-# NOINLINE vkDedicatedAllocation #-}
        vkDedicatedAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation})

        {-# INLINE vkDedicatedAllocationByteOffset #-}
        vkDedicatedAllocationByteOffset ~_
          = #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

        {-# INLINE readVkDedicatedAllocation #-}
        readVkDedicatedAllocation p
          = peekByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

        {-# INLINE writeVkDedicatedAllocation #-}
        writeVkDedicatedAllocation p
          = pokeByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

instance {-# OVERLAPPING #-}
         HasField "dedicatedAllocation"
           VkDedicatedAllocationImageCreateInfoNV
         where
        type FieldType "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             = VkBool32
        type FieldOptional "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             =
             #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}
        type FieldIsArray "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

instance CanReadField "dedicatedAllocation"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkDedicatedAllocation

        {-# INLINE readField #-}
        readField = readVkDedicatedAllocation

instance CanWriteField "dedicatedAllocation"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkDedicatedAllocation

instance Show VkDedicatedAllocationImageCreateInfoNV where
        showsPrec d x
          = showString "VkDedicatedAllocationImageCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDedicatedAllocation = " .
                            showsPrec d (vkDedicatedAllocation x) . showChar '}'
