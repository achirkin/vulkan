#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryDedicatedRequirementsKHR
       (VkMemoryDedicatedRequirementsKHR(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                       (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryRequirements2KHR (VkMemoryRequirements2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryDedicatedRequirementsKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         prefersDedicatedAllocation;
--   >     VkBool32                         requiresDedicatedAllocation;
--   > } VkMemoryDedicatedRequirementsKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryDedicatedRequirementsKHR.html VkMemoryDedicatedRequirementsKHR registry at www.khronos.org>
data VkMemoryDedicatedRequirementsKHR = VkMemoryDedicatedRequirementsKHR## Addr##
                                                                          ByteArray##

instance Eq VkMemoryDedicatedRequirementsKHR where
        (VkMemoryDedicatedRequirementsKHR## a _) ==
          x@(VkMemoryDedicatedRequirementsKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryDedicatedRequirementsKHR where
        (VkMemoryDedicatedRequirementsKHR## a _) `compare`
          x@(VkMemoryDedicatedRequirementsKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryDedicatedRequirementsKHR where
        sizeOf ~_ = #{size VkMemoryDedicatedRequirementsKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryDedicatedRequirementsKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryDedicatedRequirementsKHR where
        unsafeAddr (VkMemoryDedicatedRequirementsKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryDedicatedRequirementsKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryDedicatedRequirementsKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryDedicatedRequirementsKHR where
        type StructFields VkMemoryDedicatedRequirementsKHR =
             '["sType", "pNext", "prefersDedicatedAllocation", -- ' closing tick for hsc2hs
               "requiresDedicatedAllocation"]
        type CUnionType VkMemoryDedicatedRequirementsKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryDedicatedRequirementsKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryDedicatedRequirementsKHR =
             '[VkMemoryRequirements2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkMemoryDedicatedRequirementsKHR where
        type VkSTypeMType VkMemoryDedicatedRequirementsKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirementsKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryDedicatedRequirementsKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryDedicatedRequirementsKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirementsKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryDedicatedRequirementsKHR where
        type FieldType "sType" VkMemoryDedicatedRequirementsKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryDedicatedRequirementsKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryDedicatedRequirementsKHR =
             #{offset VkMemoryDedicatedRequirementsKHR, sType}
        type FieldIsArray "sType" VkMemoryDedicatedRequirementsKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirementsKHR, sType}

instance CanReadField "sType" VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryDedicatedRequirementsKHR where
        type VkPNextMType VkMemoryDedicatedRequirementsKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirementsKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryDedicatedRequirementsKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryDedicatedRequirementsKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirementsKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryDedicatedRequirementsKHR where
        type FieldType "pNext" VkMemoryDedicatedRequirementsKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryDedicatedRequirementsKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryDedicatedRequirementsKHR =
             #{offset VkMemoryDedicatedRequirementsKHR, pNext}
        type FieldIsArray "pNext" VkMemoryDedicatedRequirementsKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirementsKHR, pNext}

instance CanReadField "pNext" VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkPrefersDedicatedAllocation VkMemoryDedicatedRequirementsKHR
         where
        type VkPrefersDedicatedAllocationMType
               VkMemoryDedicatedRequirementsKHR
             = VkBool32

        {-# NOINLINE vkPrefersDedicatedAllocation #-}
        vkPrefersDedicatedAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation})

        {-# INLINE vkPrefersDedicatedAllocationByteOffset #-}
        vkPrefersDedicatedAllocationByteOffset ~_
          = #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation}

        {-# INLINE readVkPrefersDedicatedAllocation #-}
        readVkPrefersDedicatedAllocation p
          = peekByteOff p #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation}

        {-# INLINE writeVkPrefersDedicatedAllocation #-}
        writeVkPrefersDedicatedAllocation p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation}

instance {-# OVERLAPPING #-}
         HasField "prefersDedicatedAllocation"
           VkMemoryDedicatedRequirementsKHR
         where
        type FieldType "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             = VkBool32
        type FieldOptional "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             =
             #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation}
        type FieldIsArray "prefersDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirementsKHR, prefersDedicatedAllocation}

instance CanReadField "prefersDedicatedAllocation"
           VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE getField #-}
        getField = vkPrefersDedicatedAllocation

        {-# INLINE readField #-}
        readField = readVkPrefersDedicatedAllocation

instance CanWriteField "prefersDedicatedAllocation"
           VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPrefersDedicatedAllocation

instance {-# OVERLAPPING #-}
         HasVkRequiresDedicatedAllocation VkMemoryDedicatedRequirementsKHR
         where
        type VkRequiresDedicatedAllocationMType
               VkMemoryDedicatedRequirementsKHR
             = VkBool32

        {-# NOINLINE vkRequiresDedicatedAllocation #-}
        vkRequiresDedicatedAllocation x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation})

        {-# INLINE vkRequiresDedicatedAllocationByteOffset #-}
        vkRequiresDedicatedAllocationByteOffset ~_
          = #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation}

        {-# INLINE readVkRequiresDedicatedAllocation #-}
        readVkRequiresDedicatedAllocation p
          = peekByteOff p #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation}

        {-# INLINE writeVkRequiresDedicatedAllocation #-}
        writeVkRequiresDedicatedAllocation p
          = pokeByteOff p #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation}

instance {-# OVERLAPPING #-}
         HasField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirementsKHR
         where
        type FieldType "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             = VkBool32
        type FieldOptional "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             =
             #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation}
        type FieldIsArray "requiresDedicatedAllocation"
               VkMemoryDedicatedRequirementsKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedRequirementsKHR, requiresDedicatedAllocation}

instance CanReadField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE getField #-}
        getField = vkRequiresDedicatedAllocation

        {-# INLINE readField #-}
        readField = readVkRequiresDedicatedAllocation

instance CanWriteField "requiresDedicatedAllocation"
           VkMemoryDedicatedRequirementsKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkRequiresDedicatedAllocation

instance Show VkMemoryDedicatedRequirementsKHR where
        showsPrec d x
          = showString "VkMemoryDedicatedRequirementsKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPrefersDedicatedAllocation = " .
                            showsPrec d (vkPrefersDedicatedAllocation x) .
                              showString ", " .
                                showString "vkRequiresDedicatedAllocation = " .
                                  showsPrec d (vkRequiresDedicatedAllocation x) . showChar '}'
