#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorSetAllocateInfo
       (VkDescriptorSetAllocateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkDescriptorPool, VkDescriptorSetLayout)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorSetAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorPool       descriptorPool;
--   >     uint32_t               descriptorSetCount;
--   >     const VkDescriptorSetLayout* pSetLayouts;
--   > } VkDescriptorSetAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorSetAllocateInfo.html VkDescriptorSetAllocateInfo registry at www.khronos.org>
data VkDescriptorSetAllocateInfo = VkDescriptorSetAllocateInfo## Addr##
                                                                ByteArray##

instance Eq VkDescriptorSetAllocateInfo where
        (VkDescriptorSetAllocateInfo## a _) ==
          x@(VkDescriptorSetAllocateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetAllocateInfo where
        (VkDescriptorSetAllocateInfo## a _) `compare`
          x@(VkDescriptorSetAllocateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetAllocateInfo where
        sizeOf ~_ = #{size VkDescriptorSetAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDescriptorSetAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorSetAllocateInfo where
        unsafeAddr (VkDescriptorSetAllocateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorSetAllocateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetAllocateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorSetAllocateInfo where
        type StructFields VkDescriptorSetAllocateInfo =
             '["sType", "pNext", "descriptorPool", "descriptorSetCount", -- ' closing tick for hsc2hs
               "pSetLayouts"]
        type CUnionType VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetAllocateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkDescriptorSetAllocateInfo
         where
        type VkSTypeMType VkDescriptorSetAllocateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDescriptorSetAllocateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorSetAllocateInfo where
        type FieldType "sType" VkDescriptorSetAllocateInfo =
             VkStructureType
        type FieldOptional "sType" VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorSetAllocateInfo =
             #{offset VkDescriptorSetAllocateInfo, sType}
        type FieldIsArray "sType" VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetAllocateInfo, sType}

instance CanReadField "sType" VkDescriptorSetAllocateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDescriptorSetAllocateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDescriptorSetAllocateInfo
         where
        type VkPNextMType VkDescriptorSetAllocateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDescriptorSetAllocateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorSetAllocateInfo where
        type FieldType "pNext" VkDescriptorSetAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorSetAllocateInfo =
             #{offset VkDescriptorSetAllocateInfo, pNext}
        type FieldIsArray "pNext" VkDescriptorSetAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetAllocateInfo, pNext}

instance CanReadField "pNext" VkDescriptorSetAllocateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDescriptorSetAllocateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkDescriptorPool VkDescriptorSetAllocateInfo where
        type VkDescriptorPoolMType VkDescriptorSetAllocateInfo =
             VkDescriptorPool

        {-# NOINLINE vkDescriptorPool #-}
        vkDescriptorPool x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, descriptorPool})

        {-# INLINE vkDescriptorPoolByteOffset #-}
        vkDescriptorPoolByteOffset ~_
          = #{offset VkDescriptorSetAllocateInfo, descriptorPool}

        {-# INLINE readVkDescriptorPool #-}
        readVkDescriptorPool p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorPool}

        {-# INLINE writeVkDescriptorPool #-}
        writeVkDescriptorPool p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorPool}

instance {-# OVERLAPPING #-}
         HasField "descriptorPool" VkDescriptorSetAllocateInfo where
        type FieldType "descriptorPool" VkDescriptorSetAllocateInfo =
             VkDescriptorPool
        type FieldOptional "descriptorPool" VkDescriptorSetAllocateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorPool" VkDescriptorSetAllocateInfo =
             #{offset VkDescriptorSetAllocateInfo, descriptorPool}
        type FieldIsArray "descriptorPool" VkDescriptorSetAllocateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetAllocateInfo, descriptorPool}

instance CanReadField "descriptorPool" VkDescriptorSetAllocateInfo
         where
        {-# INLINE getField #-}
        getField = vkDescriptorPool

        {-# INLINE readField #-}
        readField = readVkDescriptorPool

instance CanWriteField "descriptorPool" VkDescriptorSetAllocateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorPool

instance {-# OVERLAPPING #-}
         HasVkDescriptorSetCount VkDescriptorSetAllocateInfo where
        type VkDescriptorSetCountMType VkDescriptorSetAllocateInfo = Word32

        {-# NOINLINE vkDescriptorSetCount #-}
        vkDescriptorSetCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, descriptorSetCount})

        {-# INLINE vkDescriptorSetCountByteOffset #-}
        vkDescriptorSetCountByteOffset ~_
          = #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}

        {-# INLINE readVkDescriptorSetCount #-}
        readVkDescriptorSetCount p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}

        {-# INLINE writeVkDescriptorSetCount #-}
        writeVkDescriptorSetCount p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}

instance {-# OVERLAPPING #-}
         HasField "descriptorSetCount" VkDescriptorSetAllocateInfo where
        type FieldType "descriptorSetCount" VkDescriptorSetAllocateInfo =
             Word32
        type FieldOptional "descriptorSetCount" VkDescriptorSetAllocateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSetCount" VkDescriptorSetAllocateInfo =
             #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}
        type FieldIsArray "descriptorSetCount" VkDescriptorSetAllocateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}

instance CanReadField "descriptorSetCount"
           VkDescriptorSetAllocateInfo
         where
        {-# INLINE getField #-}
        getField = vkDescriptorSetCount

        {-# INLINE readField #-}
        readField = readVkDescriptorSetCount

instance CanWriteField "descriptorSetCount"
           VkDescriptorSetAllocateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorSetCount

instance {-# OVERLAPPING #-}
         HasVkPSetLayouts VkDescriptorSetAllocateInfo where
        type VkPSetLayoutsMType VkDescriptorSetAllocateInfo =
             Ptr VkDescriptorSetLayout

        {-# NOINLINE vkPSetLayouts #-}
        vkPSetLayouts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, pSetLayouts})

        {-# INLINE vkPSetLayoutsByteOffset #-}
        vkPSetLayoutsByteOffset ~_
          = #{offset VkDescriptorSetAllocateInfo, pSetLayouts}

        {-# INLINE readVkPSetLayouts #-}
        readVkPSetLayouts p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, pSetLayouts}

        {-# INLINE writeVkPSetLayouts #-}
        writeVkPSetLayouts p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, pSetLayouts}

instance {-# OVERLAPPING #-}
         HasField "pSetLayouts" VkDescriptorSetAllocateInfo where
        type FieldType "pSetLayouts" VkDescriptorSetAllocateInfo =
             Ptr VkDescriptorSetLayout
        type FieldOptional "pSetLayouts" VkDescriptorSetAllocateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pSetLayouts" VkDescriptorSetAllocateInfo =
             #{offset VkDescriptorSetAllocateInfo, pSetLayouts}
        type FieldIsArray "pSetLayouts" VkDescriptorSetAllocateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetAllocateInfo, pSetLayouts}

instance CanReadField "pSetLayouts" VkDescriptorSetAllocateInfo
         where
        {-# INLINE getField #-}
        getField = vkPSetLayouts

        {-# INLINE readField #-}
        readField = readVkPSetLayouts

instance CanWriteField "pSetLayouts" VkDescriptorSetAllocateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPSetLayouts

instance Show VkDescriptorSetAllocateInfo where
        showsPrec d x
          = showString "VkDescriptorSetAllocateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkDescriptorPool = " .
                            showsPrec d (vkDescriptorPool x) .
                              showString ", " .
                                showString "vkDescriptorSetCount = " .
                                  showsPrec d (vkDescriptorSetCount x) .
                                    showString ", " .
                                      showString "vkPSetLayouts = " .
                                        showsPrec d (vkPSetLayouts x) . showChar '}'
