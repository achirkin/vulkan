#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkFormatProperties
       (VkFormatProperties(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags (VkFormatFeatureFlags)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkFormatProperties {
--   >     VkFormatFeatureFlags   linearTilingFeatures;
--   >     VkFormatFeatureFlags   optimalTilingFeatures;
--   >     VkFormatFeatureFlags   bufferFeatures;
--   > } VkFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkFormatProperties.html VkFormatProperties registry at www.khronos.org>
data VkFormatProperties = VkFormatProperties## Addr## ByteArray##

instance Eq VkFormatProperties where
        (VkFormatProperties## a _) == x@(VkFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFormatProperties where
        (VkFormatProperties## a _) `compare` x@(VkFormatProperties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFormatProperties where
        sizeOf ~_ = #{size VkFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFormatProperties where
        unsafeAddr (VkFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFormatProperties## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFormatProperties where
        type StructFields VkFormatProperties =
             '["linearTilingFeatures", "optimalTilingFeatures", -- ' closing tick for hsc2hs
               "bufferFeatures"]
        type CUnionType VkFormatProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFormatProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkFormatProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkLinearTilingFeatures VkFormatProperties where
        type VkLinearTilingFeaturesMType VkFormatProperties =
             VkFormatFeatureFlags

        {-# NOINLINE vkLinearTilingFeatures #-}
        vkLinearTilingFeatures x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties, linearTilingFeatures})

        {-# INLINE vkLinearTilingFeaturesByteOffset #-}
        vkLinearTilingFeaturesByteOffset ~_
          = #{offset VkFormatProperties, linearTilingFeatures}

        {-# INLINE readVkLinearTilingFeatures #-}
        readVkLinearTilingFeatures p
          = peekByteOff p #{offset VkFormatProperties, linearTilingFeatures}

        {-# INLINE writeVkLinearTilingFeatures #-}
        writeVkLinearTilingFeatures p
          = pokeByteOff p #{offset VkFormatProperties, linearTilingFeatures}

instance {-# OVERLAPPING #-}
         HasField "linearTilingFeatures" VkFormatProperties where
        type FieldType "linearTilingFeatures" VkFormatProperties =
             VkFormatFeatureFlags
        type FieldOptional "linearTilingFeatures" VkFormatProperties =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "linearTilingFeatures" VkFormatProperties =
             #{offset VkFormatProperties, linearTilingFeatures}
        type FieldIsArray "linearTilingFeatures" VkFormatProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFormatProperties, linearTilingFeatures}

instance CanReadField "linearTilingFeatures" VkFormatProperties
         where
        {-# INLINE getField #-}
        getField = vkLinearTilingFeatures

        {-# INLINE readField #-}
        readField = readVkLinearTilingFeatures

instance {-# OVERLAPPING #-}
         HasVkOptimalTilingFeatures VkFormatProperties where
        type VkOptimalTilingFeaturesMType VkFormatProperties =
             VkFormatFeatureFlags

        {-# NOINLINE vkOptimalTilingFeatures #-}
        vkOptimalTilingFeatures x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties, optimalTilingFeatures})

        {-# INLINE vkOptimalTilingFeaturesByteOffset #-}
        vkOptimalTilingFeaturesByteOffset ~_
          = #{offset VkFormatProperties, optimalTilingFeatures}

        {-# INLINE readVkOptimalTilingFeatures #-}
        readVkOptimalTilingFeatures p
          = peekByteOff p #{offset VkFormatProperties, optimalTilingFeatures}

        {-# INLINE writeVkOptimalTilingFeatures #-}
        writeVkOptimalTilingFeatures p
          = pokeByteOff p #{offset VkFormatProperties, optimalTilingFeatures}

instance {-# OVERLAPPING #-}
         HasField "optimalTilingFeatures" VkFormatProperties where
        type FieldType "optimalTilingFeatures" VkFormatProperties =
             VkFormatFeatureFlags
        type FieldOptional "optimalTilingFeatures" VkFormatProperties =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "optimalTilingFeatures" VkFormatProperties =
             #{offset VkFormatProperties, optimalTilingFeatures}
        type FieldIsArray "optimalTilingFeatures" VkFormatProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFormatProperties, optimalTilingFeatures}

instance CanReadField "optimalTilingFeatures" VkFormatProperties
         where
        {-# INLINE getField #-}
        getField = vkOptimalTilingFeatures

        {-# INLINE readField #-}
        readField = readVkOptimalTilingFeatures

instance {-# OVERLAPPING #-} HasVkBufferFeatures VkFormatProperties
         where
        type VkBufferFeaturesMType VkFormatProperties =
             VkFormatFeatureFlags

        {-# NOINLINE vkBufferFeatures #-}
        vkBufferFeatures x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties, bufferFeatures})

        {-# INLINE vkBufferFeaturesByteOffset #-}
        vkBufferFeaturesByteOffset ~_
          = #{offset VkFormatProperties, bufferFeatures}

        {-# INLINE readVkBufferFeatures #-}
        readVkBufferFeatures p
          = peekByteOff p #{offset VkFormatProperties, bufferFeatures}

        {-# INLINE writeVkBufferFeatures #-}
        writeVkBufferFeatures p
          = pokeByteOff p #{offset VkFormatProperties, bufferFeatures}

instance {-# OVERLAPPING #-}
         HasField "bufferFeatures" VkFormatProperties where
        type FieldType "bufferFeatures" VkFormatProperties =
             VkFormatFeatureFlags
        type FieldOptional "bufferFeatures" VkFormatProperties = 'True -- ' closing tick for hsc2hs
        type FieldOffset "bufferFeatures" VkFormatProperties =
             #{offset VkFormatProperties, bufferFeatures}
        type FieldIsArray "bufferFeatures" VkFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFormatProperties, bufferFeatures}

instance CanReadField "bufferFeatures" VkFormatProperties where
        {-# INLINE getField #-}
        getField = vkBufferFeatures

        {-# INLINE readField #-}
        readField = readVkBufferFeatures

instance Show VkFormatProperties where
        showsPrec d x
          = showString "VkFormatProperties {" .
              showString "vkLinearTilingFeatures = " .
                showsPrec d (vkLinearTilingFeatures x) .
                  showString ", " .
                    showString "vkOptimalTilingFeatures = " .
                      showsPrec d (vkOptimalTilingFeatures x) .
                        showString ", " .
                          showString "vkBufferFeatures = " .
                            showsPrec d (vkBufferFeatures x) . showChar '}'
