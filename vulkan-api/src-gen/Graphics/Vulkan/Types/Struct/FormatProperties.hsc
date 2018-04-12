#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.FormatProperties
       (VkFormatProperties(..), VkFormatProperties2(..),
        VkFormatProperties2KHR)
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Format        (VkFormatFeatureFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

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

-- | > typedef struct VkFormatProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkFormatProperties               formatProperties;
--   > } VkFormatProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFormatProperties2 VkFormatProperties2 registry at www.khronos.org>
data VkFormatProperties2 = VkFormatProperties2## Addr## ByteArray##

instance Eq VkFormatProperties2 where
        (VkFormatProperties2## a _) == x@(VkFormatProperties2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFormatProperties2 where
        (VkFormatProperties2## a _) `compare` x@(VkFormatProperties2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFormatProperties2 where
        sizeOf ~_ = #{size VkFormatProperties2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFormatProperties2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFormatProperties2 where
        unsafeAddr (VkFormatProperties2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFormatProperties2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFormatProperties2## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFormatProperties2 where
        type StructFields VkFormatProperties2 =
             '["sType", "pNext", "formatProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFormatProperties2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkFormatProperties2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkFormatProperties2
         where
        type FieldType "sType" VkFormatProperties2 = VkStructureType
        type FieldOptional "sType" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkFormatProperties2 =
             #{offset VkFormatProperties2, sType}
        type FieldIsArray "sType" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties2, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkFormatProperties2
         where
        type FieldType "pNext" VkFormatProperties2 = Ptr Void
        type FieldOptional "pNext" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkFormatProperties2 =
             #{offset VkFormatProperties2, pNext}
        type FieldIsArray "pNext" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         HasField "formatProperties" VkFormatProperties2 where
        type FieldType "formatProperties" VkFormatProperties2 =
             VkFormatProperties
        type FieldOptional "formatProperties" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "formatProperties" VkFormatProperties2 =
             #{offset VkFormatProperties2, formatProperties}
        type FieldIsArray "formatProperties" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFormatProperties2, formatProperties}

instance {-# OVERLAPPING #-}
         CanReadField "formatProperties" VkFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2, formatProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties2, formatProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "formatProperties" VkFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties2, formatProperties}

instance Show VkFormatProperties2 where
        showsPrec d x
          = showString "VkFormatProperties2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "formatProperties = " .
                            showsPrec d (getField @"formatProperties" x) . showChar '}'

-- | Alias for `VkFormatProperties2`
type VkFormatProperties2KHR = VkFormatProperties2
