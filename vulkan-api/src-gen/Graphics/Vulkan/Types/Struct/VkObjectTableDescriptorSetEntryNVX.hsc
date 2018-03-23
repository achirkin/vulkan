#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkObjectTableDescriptorSetEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipelineLayout             pipelineLayout;
--   >     VkDescriptorSet              descriptorSet;
--   > } VkObjectTableDescriptorSetEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkObjectTableDescriptorSetEntryNVX.html VkObjectTableDescriptorSetEntryNVX registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTableDescriptorSetEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTableDescriptorSetEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, type}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTableDescriptorSetEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTableDescriptorSetEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "pipelineLayout" VkObjectTableDescriptorSetEntryNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineLayout" VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

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

instance {-# OVERLAPPING #-}
         CanReadField "descriptorSet" VkObjectTableDescriptorSetEntryNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorSet" VkObjectTableDescriptorSetEntryNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

instance Show VkObjectTableDescriptorSetEntryNVX where
        showsPrec d x
          = showString "VkObjectTableDescriptorSetEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) .
                        showString ", " .
                          showString "pipelineLayout = " .
                            showsPrec d (getField @"pipelineLayout" x) .
                              showString ", " .
                                showString "descriptorSet = " .
                                  showsPrec d (getField @"descriptorSet" x) . showChar '}'
