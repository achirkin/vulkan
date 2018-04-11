#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkFormatProperties
       (VkFormatProperties(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Base                                        (Addr##,
                                                                  ByteArray##,
                                                                  byteArrayContents##,
                                                                  plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormatFeatureFlags (VkFormatFeatureFlags)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkFormatProperties {
--   >     VkFormatFeatureFlags   linearTilingFeatures;
--   >     VkFormatFeatureFlags   optimalTilingFeatures;
--   >     VkFormatFeatureFlags   bufferFeatures;
--   > } VkFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFormatProperties VkFormatProperties registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "linearTilingFeatures" VkFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties, linearTilingFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties, linearTilingFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "linearTilingFeatures" VkFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties, linearTilingFeatures}

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

instance {-# OVERLAPPING #-}
         CanReadField "optimalTilingFeatures" VkFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties, optimalTilingFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties, optimalTilingFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "optimalTilingFeatures" VkFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties, optimalTilingFeatures}

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

instance {-# OVERLAPPING #-}
         CanReadField "bufferFeatures" VkFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties, bufferFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties, bufferFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "bufferFeatures" VkFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties, bufferFeatures}

instance Show VkFormatProperties where
        showsPrec d x
          = showString "VkFormatProperties {" .
              showString "linearTilingFeatures = " .
                showsPrec d (getField @"linearTilingFeatures" x) .
                  showString ", " .
                    showString "optimalTilingFeatures = " .
                      showsPrec d (getField @"optimalTilingFeatures" x) .
                        showString ", " .
                          showString "bufferFeatures = " .
                            showsPrec d (getField @"bufferFeatures" x) . showChar '}'
