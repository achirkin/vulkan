#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.QueueFamilyProperties
       (VkQueueFamilyProperties(..), VkQueueFamilyProperties2(..),
        VkQueueFamilyProperties2KHR)
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Queue         (VkQueueFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent3D)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkQueueFamilyProperties {
--   >     VkQueueFlags           queueFlags;
--   >     uint32_t               queueCount;
--   >     uint32_t               timestampValidBits;
--   >     VkExtent3D             minImageTransferGranularity;
--   > } VkQueueFamilyProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkQueueFamilyProperties VkQueueFamilyProperties registry at www.khronos.org>
data VkQueueFamilyProperties = VkQueueFamilyProperties## Addr##
                                                        ByteArray##

instance Eq VkQueueFamilyProperties where
        (VkQueueFamilyProperties## a _) == x@(VkQueueFamilyProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkQueueFamilyProperties where
        (VkQueueFamilyProperties## a _) `compare`
          x@(VkQueueFamilyProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkQueueFamilyProperties where
        sizeOf ~_ = #{size VkQueueFamilyProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkQueueFamilyProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkQueueFamilyProperties where
        unsafeAddr (VkQueueFamilyProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkQueueFamilyProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkQueueFamilyProperties## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkQueueFamilyProperties where
        type StructFields VkQueueFamilyProperties =
             '["queueFlags", "queueCount", "timestampValidBits", -- ' closing tick for hsc2hs
               "minImageTransferGranularity"]
        type CUnionType VkQueueFamilyProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkQueueFamilyProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkQueueFamilyProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "queueFlags" VkQueueFamilyProperties where
        type FieldType "queueFlags" VkQueueFamilyProperties = VkQueueFlags
        type FieldOptional "queueFlags" VkQueueFamilyProperties = 'True -- ' closing tick for hsc2hs
        type FieldOffset "queueFlags" VkQueueFamilyProperties =
             #{offset VkQueueFamilyProperties, queueFlags}
        type FieldIsArray "queueFlags" VkQueueFamilyProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties, queueFlags}

instance {-# OVERLAPPING #-}
         CanReadField "queueFlags" VkQueueFamilyProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, queueFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties, queueFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFlags" VkQueueFamilyProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties, queueFlags}

instance {-# OVERLAPPING #-}
         HasField "queueCount" VkQueueFamilyProperties where
        type FieldType "queueCount" VkQueueFamilyProperties = Word32
        type FieldOptional "queueCount" VkQueueFamilyProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueCount" VkQueueFamilyProperties =
             #{offset VkQueueFamilyProperties, queueCount}
        type FieldIsArray "queueCount" VkQueueFamilyProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties, queueCount}

instance {-# OVERLAPPING #-}
         CanReadField "queueCount" VkQueueFamilyProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, queueCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties, queueCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queueCount" VkQueueFamilyProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties, queueCount}

instance {-# OVERLAPPING #-}
         HasField "timestampValidBits" VkQueueFamilyProperties where
        type FieldType "timestampValidBits" VkQueueFamilyProperties =
             Word32
        type FieldOptional "timestampValidBits" VkQueueFamilyProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "timestampValidBits" VkQueueFamilyProperties =
             #{offset VkQueueFamilyProperties, timestampValidBits}
        type FieldIsArray "timestampValidBits" VkQueueFamilyProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties, timestampValidBits}

instance {-# OVERLAPPING #-}
         CanReadField "timestampValidBits" VkQueueFamilyProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, timestampValidBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties, timestampValidBits}

instance {-# OVERLAPPING #-}
         CanWriteField "timestampValidBits" VkQueueFamilyProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties, timestampValidBits}

instance {-# OVERLAPPING #-}
         HasField "minImageTransferGranularity" VkQueueFamilyProperties
         where
        type FieldType "minImageTransferGranularity"
               VkQueueFamilyProperties
             = VkExtent3D
        type FieldOptional "minImageTransferGranularity"
               VkQueueFamilyProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minImageTransferGranularity"
               VkQueueFamilyProperties
             =
             #{offset VkQueueFamilyProperties, minImageTransferGranularity}
        type FieldIsArray "minImageTransferGranularity"
               VkQueueFamilyProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties, minImageTransferGranularity}

instance {-# OVERLAPPING #-}
         CanReadField "minImageTransferGranularity" VkQueueFamilyProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties, minImageTransferGranularity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties, minImageTransferGranularity}

instance {-# OVERLAPPING #-}
         CanWriteField "minImageTransferGranularity" VkQueueFamilyProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties, minImageTransferGranularity}

instance Show VkQueueFamilyProperties where
        showsPrec d x
          = showString "VkQueueFamilyProperties {" .
              showString "queueFlags = " .
                showsPrec d (getField @"queueFlags" x) .
                  showString ", " .
                    showString "queueCount = " .
                      showsPrec d (getField @"queueCount" x) .
                        showString ", " .
                          showString "timestampValidBits = " .
                            showsPrec d (getField @"timestampValidBits" x) .
                              showString ", " .
                                showString "minImageTransferGranularity = " .
                                  showsPrec d (getField @"minImageTransferGranularity" x) .
                                    showChar '}'

-- | > typedef struct VkQueueFamilyProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkQueueFamilyProperties          queueFamilyProperties;
--   > } VkQueueFamilyProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkQueueFamilyProperties2 VkQueueFamilyProperties2 registry at www.khronos.org>
data VkQueueFamilyProperties2 = VkQueueFamilyProperties2## Addr##
                                                          ByteArray##

instance Eq VkQueueFamilyProperties2 where
        (VkQueueFamilyProperties2## a _) ==
          x@(VkQueueFamilyProperties2## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkQueueFamilyProperties2 where
        (VkQueueFamilyProperties2## a _) `compare`
          x@(VkQueueFamilyProperties2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkQueueFamilyProperties2 where
        sizeOf ~_ = #{size VkQueueFamilyProperties2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkQueueFamilyProperties2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkQueueFamilyProperties2 where
        unsafeAddr (VkQueueFamilyProperties2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkQueueFamilyProperties2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkQueueFamilyProperties2## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkQueueFamilyProperties2 where
        type StructFields VkQueueFamilyProperties2 =
             '["sType", "pNext", "queueFamilyProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkQueueFamilyProperties2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkQueueFamilyProperties2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkQueueFamilyProperties2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkQueueFamilyProperties2 where
        type FieldType "sType" VkQueueFamilyProperties2 = VkStructureType
        type FieldOptional "sType" VkQueueFamilyProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkQueueFamilyProperties2 =
             #{offset VkQueueFamilyProperties2, sType}
        type FieldIsArray "sType" VkQueueFamilyProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkQueueFamilyProperties2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkQueueFamilyProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkQueueFamilyProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkQueueFamilyProperties2 where
        type FieldType "pNext" VkQueueFamilyProperties2 = Ptr Void
        type FieldOptional "pNext" VkQueueFamilyProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkQueueFamilyProperties2 =
             #{offset VkQueueFamilyProperties2, pNext}
        type FieldIsArray "pNext" VkQueueFamilyProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkQueueFamilyProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkQueueFamilyProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkQueueFamilyProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties2, pNext}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyProperties" VkQueueFamilyProperties2 where
        type FieldType "queueFamilyProperties" VkQueueFamilyProperties2 =
             VkQueueFamilyProperties
        type FieldOptional "queueFamilyProperties" VkQueueFamilyProperties2
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyProperties" VkQueueFamilyProperties2 =
             #{offset VkQueueFamilyProperties2, queueFamilyProperties}
        type FieldIsArray "queueFamilyProperties" VkQueueFamilyProperties2
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueueFamilyProperties2, queueFamilyProperties}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyProperties" VkQueueFamilyProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2, queueFamilyProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueueFamilyProperties2, queueFamilyProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyProperties" VkQueueFamilyProperties2
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueueFamilyProperties2, queueFamilyProperties}

instance Show VkQueueFamilyProperties2 where
        showsPrec d x
          = showString "VkQueueFamilyProperties2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "queueFamilyProperties = " .
                            showsPrec d (getField @"queueFamilyProperties" x) . showChar '}'

-- | Alias for `VkQueueFamilyProperties2`
type VkQueueFamilyProperties2KHR = VkQueueFamilyProperties2
