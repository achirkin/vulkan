#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkObjectTableDescriptorSetEntryNVX
       (VkObjectTableDescriptorSetEntryNVX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX       (VkObjectEntryTypeNVX)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX (VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.Handles                         (VkDescriptorSet,
                                                                        VkPipelineLayout)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkObjectTableDescriptorSetEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipelineLayout             pipelineLayout;
--   >     VkDescriptorSet              descriptorSet;
--   > } VkObjectTableDescriptorSetEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkObjectTableDescriptorSetEntryNVX.html VkObjectTableDescriptorSetEntryNVX registry at www.khronos.org>
data VkObjectTableDescriptorSetEntryNVX = VkObjectTableDescriptorSetEntryNVX## Addr##
                                                                              ByteArray##

instance Eq VkObjectTableDescriptorSetEntryNVX where
        (VkObjectTableDescriptorSetEntryNVX## a _) ==
          x@(VkObjectTableDescriptorSetEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableDescriptorSetEntryNVX where
        (VkObjectTableDescriptorSetEntryNVX## a _) `compare`
          x@(VkObjectTableDescriptorSetEntryNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTableDescriptorSetEntryNVX where
        sizeOf ~_ = #{size VkObjectTableDescriptorSetEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTableDescriptorSetEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTableDescriptorSetEntryNVX where
        unsafeAddr (VkObjectTableDescriptorSetEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTableDescriptorSetEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTableDescriptorSetEntryNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTableDescriptorSetEntryNVX where
        type StructFields VkObjectTableDescriptorSetEntryNVX =
             '["type", "flags", "pipelineLayout", "descriptorSet"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTableDescriptorSetEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTableDescriptorSetEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTableDescriptorSetEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkType VkObjectTableDescriptorSetEntryNVX where
        type VkTypeMType VkObjectTableDescriptorSetEntryNVX =
             VkObjectEntryTypeNVX

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkObjectTableDescriptorSetEntryNVX, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasField "type" VkObjectTableDescriptorSetEntryNVX where
        type FieldType "type" VkObjectTableDescriptorSetEntryNVX =
             VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTableDescriptorSetEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTableDescriptorSetEntryNVX =
             #{offset VkObjectTableDescriptorSetEntryNVX, type}
        type FieldIsArray "type" VkObjectTableDescriptorSetEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableDescriptorSetEntryNVX, type}

instance CanReadField "type" VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkType

        {-# INLINE readField #-}
        readField = readVkType

instance CanWriteField "type" VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkType

instance {-# OVERLAPPING #-}
         HasVkFlags VkObjectTableDescriptorSetEntryNVX where
        type VkFlagsMType VkObjectTableDescriptorSetEntryNVX =
             VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkObjectTableDescriptorSetEntryNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkObjectTableDescriptorSetEntryNVX where
        type FieldType "flags" VkObjectTableDescriptorSetEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTableDescriptorSetEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTableDescriptorSetEntryNVX =
             #{offset VkObjectTableDescriptorSetEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTableDescriptorSetEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableDescriptorSetEntryNVX, flags}

instance CanReadField "flags" VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkPipelineLayout VkObjectTableDescriptorSetEntryNVX where
        type VkPipelineLayoutMType VkObjectTableDescriptorSetEntryNVX =
             VkPipelineLayout

        {-# NOINLINE vkPipelineLayout #-}
        vkPipelineLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout})

        {-# INLINE vkPipelineLayoutByteOffset #-}
        vkPipelineLayoutByteOffset ~_
          = #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

        {-# INLINE readVkPipelineLayout #-}
        readVkPipelineLayout p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

        {-# INLINE writeVkPipelineLayout #-}
        writeVkPipelineLayout p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         HasField "pipelineLayout" VkObjectTableDescriptorSetEntryNVX where
        type FieldType "pipelineLayout" VkObjectTableDescriptorSetEntryNVX
             = VkPipelineLayout
        type FieldOptional "pipelineLayout"
               VkObjectTableDescriptorSetEntryNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineLayout"
               VkObjectTableDescriptorSetEntryNVX
             =
             #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}
        type FieldIsArray "pipelineLayout"
               VkObjectTableDescriptorSetEntryNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

instance CanReadField "pipelineLayout"
           VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkPipelineLayout

        {-# INLINE readField #-}
        readField = readVkPipelineLayout

instance CanWriteField "pipelineLayout"
           VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPipelineLayout

instance {-# OVERLAPPING #-}
         HasVkDescriptorSet VkObjectTableDescriptorSetEntryNVX where
        type VkDescriptorSetMType VkObjectTableDescriptorSetEntryNVX =
             VkDescriptorSet

        {-# NOINLINE vkDescriptorSet #-}
        vkDescriptorSet x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet})

        {-# INLINE vkDescriptorSetByteOffset #-}
        vkDescriptorSetByteOffset ~_
          = #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

        {-# INLINE readVkDescriptorSet #-}
        readVkDescriptorSet p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

        {-# INLINE writeVkDescriptorSet #-}
        writeVkDescriptorSet p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

instance {-# OVERLAPPING #-}
         HasField "descriptorSet" VkObjectTableDescriptorSetEntryNVX where
        type FieldType "descriptorSet" VkObjectTableDescriptorSetEntryNVX =
             VkDescriptorSet
        type FieldOptional "descriptorSet"
               VkObjectTableDescriptorSetEntryNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSet" VkObjectTableDescriptorSetEntryNVX
             =
             #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}
        type FieldIsArray "descriptorSet"
               VkObjectTableDescriptorSetEntryNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

instance CanReadField "descriptorSet"
           VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkDescriptorSet

        {-# INLINE readField #-}
        readField = readVkDescriptorSet

instance CanWriteField "descriptorSet"
           VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorSet

instance Show VkObjectTableDescriptorSetEntryNVX where
        showsPrec d x
          = showString "VkObjectTableDescriptorSetEntryNVX {" .
              showString "vkType = " .
                showsPrec d (vkType x) .
                  showString ", " .
                    showString "vkFlags = " .
                      showsPrec d (vkFlags x) .
                        showString ", " .
                          showString "vkPipelineLayout = " .
                            showsPrec d (vkPipelineLayout x) .
                              showString ", " .
                                showString "vkDescriptorSet = " .
                                  showsPrec d (vkDescriptorSet x) . showChar '}'
