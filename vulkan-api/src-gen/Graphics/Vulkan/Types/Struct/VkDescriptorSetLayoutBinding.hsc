#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBinding
       (VkDescriptorSetLayoutBinding(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorType   (VkDescriptorType)
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Handles                 (VkSampler)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorSetLayoutBinding {
--   >     uint32_t               binding;
--   >     VkDescriptorType       descriptorType;
--   >     uint32_t descriptorCount;
--   >     VkShaderStageFlags     stageFlags;
--   >     const VkSampler*       pImmutableSamplers;
--   > } VkDescriptorSetLayoutBinding;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDescriptorSetLayoutBinding VkDescriptorSetLayoutBinding registry at www.khronos.org>
data VkDescriptorSetLayoutBinding = VkDescriptorSetLayoutBinding## Addr##
                                                                  ByteArray##

instance Eq VkDescriptorSetLayoutBinding where
        (VkDescriptorSetLayoutBinding## a _) ==
          x@(VkDescriptorSetLayoutBinding## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetLayoutBinding where
        (VkDescriptorSetLayoutBinding## a _) `compare`
          x@(VkDescriptorSetLayoutBinding## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetLayoutBinding where
        sizeOf ~_ = #{size VkDescriptorSetLayoutBinding}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetLayoutBinding}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorSetLayoutBinding where
        unsafeAddr (VkDescriptorSetLayoutBinding## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorSetLayoutBinding## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetLayoutBinding##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorSetLayoutBinding where
        type StructFields VkDescriptorSetLayoutBinding =
             '["binding", "descriptorType", "descriptorCount", "stageFlags", -- ' closing tick for hsc2hs
               "pImmutableSamplers"]
        type CUnionType VkDescriptorSetLayoutBinding = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetLayoutBinding = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetLayoutBinding = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "binding" VkDescriptorSetLayoutBinding where
        type FieldType "binding" VkDescriptorSetLayoutBinding = Word32
        type FieldOptional "binding" VkDescriptorSetLayoutBinding = 'False -- ' closing tick for hsc2hs
        type FieldOffset "binding" VkDescriptorSetLayoutBinding =
             #{offset VkDescriptorSetLayoutBinding, binding}
        type FieldIsArray "binding" VkDescriptorSetLayoutBinding = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBinding, binding}

instance {-# OVERLAPPING #-}
         CanReadField "binding" VkDescriptorSetLayoutBinding where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, binding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, binding}

instance {-# OVERLAPPING #-}
         CanWriteField "binding" VkDescriptorSetLayoutBinding where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, binding}

instance {-# OVERLAPPING #-}
         HasField "descriptorType" VkDescriptorSetLayoutBinding where
        type FieldType "descriptorType" VkDescriptorSetLayoutBinding =
             VkDescriptorType
        type FieldOptional "descriptorType" VkDescriptorSetLayoutBinding =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorType" VkDescriptorSetLayoutBinding =
             #{offset VkDescriptorSetLayoutBinding, descriptorType}
        type FieldIsArray "descriptorType" VkDescriptorSetLayoutBinding =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBinding, descriptorType}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorType" VkDescriptorSetLayoutBinding where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, descriptorType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorType}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorType" VkDescriptorSetLayoutBinding where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorType}

instance {-# OVERLAPPING #-}
         HasField "descriptorCount" VkDescriptorSetLayoutBinding where
        type FieldType "descriptorCount" VkDescriptorSetLayoutBinding =
             Word32
        type FieldOptional "descriptorCount" VkDescriptorSetLayoutBinding =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "descriptorCount" VkDescriptorSetLayoutBinding =
             #{offset VkDescriptorSetLayoutBinding, descriptorCount}
        type FieldIsArray "descriptorCount" VkDescriptorSetLayoutBinding =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBinding, descriptorCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorCount" VkDescriptorSetLayoutBinding where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, descriptorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorCount" VkDescriptorSetLayoutBinding where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, descriptorCount}

instance {-# OVERLAPPING #-}
         HasField "stageFlags" VkDescriptorSetLayoutBinding where
        type FieldType "stageFlags" VkDescriptorSetLayoutBinding =
             VkShaderStageFlags
        type FieldOptional "stageFlags" VkDescriptorSetLayoutBinding =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stageFlags" VkDescriptorSetLayoutBinding =
             #{offset VkDescriptorSetLayoutBinding, stageFlags}
        type FieldIsArray "stageFlags" VkDescriptorSetLayoutBinding =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBinding, stageFlags}

instance {-# OVERLAPPING #-}
         CanReadField "stageFlags" VkDescriptorSetLayoutBinding where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, stageFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, stageFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "stageFlags" VkDescriptorSetLayoutBinding where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, stageFlags}

instance {-# OVERLAPPING #-}
         HasField "pImmutableSamplers" VkDescriptorSetLayoutBinding where
        type FieldType "pImmutableSamplers" VkDescriptorSetLayoutBinding =
             Ptr VkSampler
        type FieldOptional "pImmutableSamplers"
               VkDescriptorSetLayoutBinding
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pImmutableSamplers" VkDescriptorSetLayoutBinding
             =
             #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}
        type FieldIsArray "pImmutableSamplers" VkDescriptorSetLayoutBinding
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}

instance {-# OVERLAPPING #-}
         CanReadField "pImmutableSamplers" VkDescriptorSetLayoutBinding
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}

instance {-# OVERLAPPING #-}
         CanWriteField "pImmutableSamplers" VkDescriptorSetLayoutBinding
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutBinding, pImmutableSamplers}

instance Show VkDescriptorSetLayoutBinding where
        showsPrec d x
          = showString "VkDescriptorSetLayoutBinding {" .
              showString "binding = " .
                showsPrec d (getField @"binding" x) .
                  showString ", " .
                    showString "descriptorType = " .
                      showsPrec d (getField @"descriptorType" x) .
                        showString ", " .
                          showString "descriptorCount = " .
                            showsPrec d (getField @"descriptorCount" x) .
                              showString ", " .
                                showString "stageFlags = " .
                                  showsPrec d (getField @"stageFlags" x) .
                                    showString ", " .
                                      showString "pImmutableSamplers = " .
                                        showsPrec d (getField @"pImmutableSamplers" x) .
                                          showChar '}'
