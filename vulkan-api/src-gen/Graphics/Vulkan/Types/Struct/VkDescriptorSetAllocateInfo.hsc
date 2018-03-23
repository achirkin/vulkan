#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorSetAllocateInfo
       (VkDescriptorSetAllocateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkDescriptorPool, VkDescriptorSetLayout)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorSetAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorPool       descriptorPool;
--   >     uint32_t               descriptorSetCount;
--   >     const VkDescriptorSetLayout* pSetLayouts;
--   > } VkDescriptorSetAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDescriptorSetAllocateInfo.html VkDescriptorSetAllocateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorSetAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorSetAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorSetAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorSetAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "descriptorPool" VkDescriptorSetAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, descriptorPool})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorPool}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorPool" VkDescriptorSetAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorPool}

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

instance {-# OVERLAPPING #-}
         CanReadField "descriptorSetCount" VkDescriptorSetAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, descriptorSetCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorSetCount" VkDescriptorSetAllocateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, descriptorSetCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pSetLayouts" VkDescriptorSetAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetAllocateInfo, pSetLayouts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetAllocateInfo, pSetLayouts}

instance {-# OVERLAPPING #-}
         CanWriteField "pSetLayouts" VkDescriptorSetAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetAllocateInfo, pSetLayouts}

instance Show VkDescriptorSetAllocateInfo where
        showsPrec d x
          = showString "VkDescriptorSetAllocateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "descriptorPool = " .
                            showsPrec d (getField @"descriptorPool" x) .
                              showString ", " .
                                showString "descriptorSetCount = " .
                                  showsPrec d (getField @"descriptorSetCount" x) .
                                    showString ", " .
                                      showString "pSetLayouts = " .
                                        showsPrec d (getField @"pSetLayouts" x) . showChar '}'
