#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Sparse
       (VkSparseBufferMemoryBindInfo(..),
        VkSparseImageFormatProperties(..),
        VkSparseImageFormatProperties2(..),
        VkSparseImageFormatProperties2KHR, VkSparseImageMemoryBind(..),
        VkSparseImageMemoryBindInfo(..),
        VkSparseImageMemoryRequirements(..),
        VkSparseImageMemoryRequirements2(..),
        VkSparseImageMemoryRequirements2KHR,
        VkSparseImageOpaqueMemoryBindInfo(..), VkSparseMemoryBind(..))
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.Image         (VkImageAspectFlags)
import           Graphics.Vulkan.Types.Enum.Sparse        (VkSparseImageFormatFlags,
                                                           VkSparseMemoryBindFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer,
                                                           VkDeviceMemory,
                                                           VkImage)
import           Graphics.Vulkan.Types.Struct.Extent      (VkExtent3D)
import           Graphics.Vulkan.Types.Struct.Image       (VkImageSubresource)
import           Graphics.Vulkan.Types.Struct.Offset      (VkOffset3D)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkSparseBufferMemoryBindInfo {
--   >     VkBuffer buffer;
--   >     uint32_t               bindCount;
--   >     const VkSparseMemoryBind* pBinds;
--   > } VkSparseBufferMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseBufferMemoryBindInfo VkSparseBufferMemoryBindInfo registry at www.khronos.org>
data VkSparseBufferMemoryBindInfo = VkSparseBufferMemoryBindInfo## Addr##
                                                                  ByteArray##

instance Eq VkSparseBufferMemoryBindInfo where
        (VkSparseBufferMemoryBindInfo## a _) ==
          x@(VkSparseBufferMemoryBindInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseBufferMemoryBindInfo where
        (VkSparseBufferMemoryBindInfo## a _) `compare`
          x@(VkSparseBufferMemoryBindInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseBufferMemoryBindInfo where
        sizeOf ~_ = #{size VkSparseBufferMemoryBindInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseBufferMemoryBindInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseBufferMemoryBindInfo where
        unsafeAddr (VkSparseBufferMemoryBindInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseBufferMemoryBindInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseBufferMemoryBindInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseBufferMemoryBindInfo where
        type StructFields VkSparseBufferMemoryBindInfo =
             '["buffer", "bindCount", "pBinds"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSparseBufferMemoryBindInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "buffer" VkSparseBufferMemoryBindInfo where
        type FieldType "buffer" VkSparseBufferMemoryBindInfo = VkBuffer
        type FieldOptional "buffer" VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkSparseBufferMemoryBindInfo =
             #{offset VkSparseBufferMemoryBindInfo, buffer}
        type FieldIsArray "buffer" VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseBufferMemoryBindInfo, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkSparseBufferMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseBufferMemoryBindInfo, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseBufferMemoryBindInfo, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkSparseBufferMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseBufferMemoryBindInfo, buffer}

instance {-# OVERLAPPING #-}
         HasField "bindCount" VkSparseBufferMemoryBindInfo where
        type FieldType "bindCount" VkSparseBufferMemoryBindInfo = Word32
        type FieldOptional "bindCount" VkSparseBufferMemoryBindInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "bindCount" VkSparseBufferMemoryBindInfo =
             #{offset VkSparseBufferMemoryBindInfo, bindCount}
        type FieldIsArray "bindCount" VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseBufferMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanReadField "bindCount" VkSparseBufferMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseBufferMemoryBindInfo, bindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseBufferMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bindCount" VkSparseBufferMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseBufferMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         HasField "pBinds" VkSparseBufferMemoryBindInfo where
        type FieldType "pBinds" VkSparseBufferMemoryBindInfo =
             Ptr VkSparseMemoryBind
        type FieldOptional "pBinds" VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pBinds" VkSparseBufferMemoryBindInfo =
             #{offset VkSparseBufferMemoryBindInfo, pBinds}
        type FieldIsArray "pBinds" VkSparseBufferMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseBufferMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanReadField "pBinds" VkSparseBufferMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseBufferMemoryBindInfo, pBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseBufferMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pBinds" VkSparseBufferMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseBufferMemoryBindInfo, pBinds}

instance Show VkSparseBufferMemoryBindInfo where
        showsPrec d x
          = showString "VkSparseBufferMemoryBindInfo {" .
              showString "buffer = " .
                showsPrec d (getField @"buffer" x) .
                  showString ", " .
                    showString "bindCount = " .
                      showsPrec d (getField @"bindCount" x) .
                        showString ", " .
                          showString "pBinds = " .
                            showsPrec d (getField @"pBinds" x) . showChar '}'

-- | > typedef struct VkSparseImageFormatProperties {
--   >     VkImageAspectFlags     aspectMask;
--   >     VkExtent3D             imageGranularity;
--   >     VkSparseImageFormatFlags flags;
--   > } VkSparseImageFormatProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageFormatProperties VkSparseImageFormatProperties registry at www.khronos.org>
data VkSparseImageFormatProperties = VkSparseImageFormatProperties## Addr##
                                                                    ByteArray##

instance Eq VkSparseImageFormatProperties where
        (VkSparseImageFormatProperties## a _) ==
          x@(VkSparseImageFormatProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageFormatProperties where
        (VkSparseImageFormatProperties## a _) `compare`
          x@(VkSparseImageFormatProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageFormatProperties where
        sizeOf ~_ = #{size VkSparseImageFormatProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageFormatProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageFormatProperties where
        unsafeAddr (VkSparseImageFormatProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageFormatProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageFormatProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageFormatProperties where
        type StructFields VkSparseImageFormatProperties =
             '["aspectMask", "imageGranularity", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageFormatProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageFormatProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkSparseImageFormatProperties where
        type FieldType "aspectMask" VkSparseImageFormatProperties =
             VkImageAspectFlags
        type FieldOptional "aspectMask" VkSparseImageFormatProperties =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkSparseImageFormatProperties =
             #{offset VkSparseImageFormatProperties, aspectMask}
        type FieldIsArray "aspectMask" VkSparseImageFormatProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties, aspectMask}

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkSparseImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkSparseImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties, aspectMask}

instance {-# OVERLAPPING #-}
         HasField "imageGranularity" VkSparseImageFormatProperties where
        type FieldType "imageGranularity" VkSparseImageFormatProperties =
             VkExtent3D
        type FieldOptional "imageGranularity" VkSparseImageFormatProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageGranularity" VkSparseImageFormatProperties =
             #{offset VkSparseImageFormatProperties, imageGranularity}
        type FieldIsArray "imageGranularity" VkSparseImageFormatProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties, imageGranularity}

instance {-# OVERLAPPING #-}
         CanReadField "imageGranularity" VkSparseImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties, imageGranularity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties, imageGranularity}

instance {-# OVERLAPPING #-}
         CanWriteField "imageGranularity" VkSparseImageFormatProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties, imageGranularity}

instance {-# OVERLAPPING #-}
         HasField "flags" VkSparseImageFormatProperties where
        type FieldType "flags" VkSparseImageFormatProperties =
             VkSparseImageFormatFlags
        type FieldOptional "flags" VkSparseImageFormatProperties = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSparseImageFormatProperties =
             #{offset VkSparseImageFormatProperties, flags}
        type FieldIsArray "flags" VkSparseImageFormatProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkSparseImageFormatProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkSparseImageFormatProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties, flags}

instance Show VkSparseImageFormatProperties where
        showsPrec d x
          = showString "VkSparseImageFormatProperties {" .
              showString "aspectMask = " .
                showsPrec d (getField @"aspectMask" x) .
                  showString ", " .
                    showString "imageGranularity = " .
                      showsPrec d (getField @"imageGranularity" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) . showChar '}'

-- | > typedef struct VkSparseImageFormatProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkSparseImageFormatProperties    properties;
--   > } VkSparseImageFormatProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageFormatProperties2 VkSparseImageFormatProperties2 registry at www.khronos.org>
data VkSparseImageFormatProperties2 = VkSparseImageFormatProperties2## Addr##
                                                                      ByteArray##

instance Eq VkSparseImageFormatProperties2 where
        (VkSparseImageFormatProperties2## a _) ==
          x@(VkSparseImageFormatProperties2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageFormatProperties2 where
        (VkSparseImageFormatProperties2## a _) `compare`
          x@(VkSparseImageFormatProperties2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageFormatProperties2 where
        sizeOf ~_ = #{size VkSparseImageFormatProperties2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageFormatProperties2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageFormatProperties2 where
        unsafeAddr (VkSparseImageFormatProperties2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageFormatProperties2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageFormatProperties2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageFormatProperties2 where
        type StructFields VkSparseImageFormatProperties2 =
             '["sType", "pNext", "properties"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageFormatProperties2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageFormatProperties2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSparseImageFormatProperties2 where
        type FieldType "sType" VkSparseImageFormatProperties2 =
             VkStructureType
        type FieldOptional "sType" VkSparseImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSparseImageFormatProperties2 =
             #{offset VkSparseImageFormatProperties2, sType}
        type FieldIsArray "sType" VkSparseImageFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSparseImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSparseImageFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSparseImageFormatProperties2 where
        type FieldType "pNext" VkSparseImageFormatProperties2 = Ptr Void
        type FieldOptional "pNext" VkSparseImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSparseImageFormatProperties2 =
             #{offset VkSparseImageFormatProperties2, pNext}
        type FieldIsArray "pNext" VkSparseImageFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSparseImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSparseImageFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         HasField "properties" VkSparseImageFormatProperties2 where
        type FieldType "properties" VkSparseImageFormatProperties2 =
             VkSparseImageFormatProperties
        type FieldOptional "properties" VkSparseImageFormatProperties2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "properties" VkSparseImageFormatProperties2 =
             #{offset VkSparseImageFormatProperties2, properties}
        type FieldIsArray "properties" VkSparseImageFormatProperties2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2, properties}

instance {-# OVERLAPPING #-}
         CanReadField "properties" VkSparseImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2, properties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties2, properties}

instance {-# OVERLAPPING #-}
         CanWriteField "properties" VkSparseImageFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2, properties}

instance Show VkSparseImageFormatProperties2 where
        showsPrec d x
          = showString "VkSparseImageFormatProperties2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "properties = " .
                            showsPrec d (getField @"properties" x) . showChar '}'

-- | Alias for `VkSparseImageFormatProperties2`
type VkSparseImageFormatProperties2KHR =
     VkSparseImageFormatProperties2

-- | > typedef struct VkSparseImageMemoryBind {
--   >     VkImageSubresource     subresource;
--   >     VkOffset3D             offset;
--   >     VkExtent3D             extent;
--   >     VkDeviceMemory         memory;
--   >     VkDeviceSize           memoryOffset;
--   >     VkSparseMemoryBindFlagsflags;
--   > } VkSparseImageMemoryBind;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageMemoryBind VkSparseImageMemoryBind registry at www.khronos.org>
data VkSparseImageMemoryBind = VkSparseImageMemoryBind## Addr##
                                                        ByteArray##

instance Eq VkSparseImageMemoryBind where
        (VkSparseImageMemoryBind## a _) == x@(VkSparseImageMemoryBind## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageMemoryBind where
        (VkSparseImageMemoryBind## a _) `compare`
          x@(VkSparseImageMemoryBind## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageMemoryBind where
        sizeOf ~_ = #{size VkSparseImageMemoryBind}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSparseImageMemoryBind}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageMemoryBind where
        unsafeAddr (VkSparseImageMemoryBind## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageMemoryBind## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageMemoryBind## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageMemoryBind where
        type StructFields VkSparseImageMemoryBind =
             '["subresource", "offset", "extent", "memory", "memoryOffset", -- ' closing tick for hsc2hs
               "flags"]
        type CUnionType VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageMemoryBind = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "subresource" VkSparseImageMemoryBind where
        type FieldType "subresource" VkSparseImageMemoryBind =
             VkImageSubresource
        type FieldOptional "subresource" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subresource" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, subresource}
        type FieldIsArray "subresource" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryBind, subresource}

instance {-# OVERLAPPING #-}
         CanReadField "subresource" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, subresource})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, subresource}

instance {-# OVERLAPPING #-}
         CanWriteField "subresource" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, subresource}

instance {-# OVERLAPPING #-}
         HasField "offset" VkSparseImageMemoryBind where
        type FieldType "offset" VkSparseImageMemoryBind = VkOffset3D
        type FieldOptional "offset" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, offset}
        type FieldIsArray "offset" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseImageMemoryBind, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, offset}

instance {-# OVERLAPPING #-}
         HasField "extent" VkSparseImageMemoryBind where
        type FieldType "extent" VkSparseImageMemoryBind = VkExtent3D
        type FieldOptional "extent" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extent" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, extent}
        type FieldIsArray "extent" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseImageMemoryBind, extent}

instance {-# OVERLAPPING #-}
         CanReadField "extent" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, extent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, extent}

instance {-# OVERLAPPING #-}
         CanWriteField "extent" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, extent}

instance {-# OVERLAPPING #-}
         HasField "memory" VkSparseImageMemoryBind where
        type FieldType "memory" VkSparseImageMemoryBind = VkDeviceMemory
        type FieldOptional "memory" VkSparseImageMemoryBind = 'True -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, memory}
        type FieldIsArray "memory" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseImageMemoryBind, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, memory}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkSparseImageMemoryBind where
        type FieldType "memoryOffset" VkSparseImageMemoryBind =
             VkDeviceSize
        type FieldOptional "memoryOffset" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, memoryOffset}
        type FieldIsArray "memoryOffset" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-}
         CanReadField "memoryOffset" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, memoryOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryOffset" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-}
         HasField "flags" VkSparseImageMemoryBind where
        type FieldType "flags" VkSparseImageMemoryBind =
             VkSparseMemoryBindFlags
        type FieldOptional "flags" VkSparseImageMemoryBind = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSparseImageMemoryBind =
             #{offset VkSparseImageMemoryBind, flags}
        type FieldIsArray "flags" VkSparseImageMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseImageMemoryBind, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkSparseImageMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBind, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBind, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkSparseImageMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBind, flags}

instance Show VkSparseImageMemoryBind where
        showsPrec d x
          = showString "VkSparseImageMemoryBind {" .
              showString "subresource = " .
                showsPrec d (getField @"subresource" x) .
                  showString ", " .
                    showString "offset = " .
                      showsPrec d (getField @"offset" x) .
                        showString ", " .
                          showString "extent = " .
                            showsPrec d (getField @"extent" x) .
                              showString ", " .
                                showString "memory = " .
                                  showsPrec d (getField @"memory" x) .
                                    showString ", " .
                                      showString "memoryOffset = " .
                                        showsPrec d (getField @"memoryOffset" x) .
                                          showString ", " .
                                            showString "flags = " .
                                              showsPrec d (getField @"flags" x) . showChar '}'

-- | > typedef struct VkSparseImageMemoryBindInfo {
--   >     VkImage image;
--   >     uint32_t               bindCount;
--   >     const VkSparseImageMemoryBind* pBinds;
--   > } VkSparseImageMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageMemoryBindInfo VkSparseImageMemoryBindInfo registry at www.khronos.org>
data VkSparseImageMemoryBindInfo = VkSparseImageMemoryBindInfo## Addr##
                                                                ByteArray##

instance Eq VkSparseImageMemoryBindInfo where
        (VkSparseImageMemoryBindInfo## a _) ==
          x@(VkSparseImageMemoryBindInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageMemoryBindInfo where
        (VkSparseImageMemoryBindInfo## a _) `compare`
          x@(VkSparseImageMemoryBindInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageMemoryBindInfo where
        sizeOf ~_ = #{size VkSparseImageMemoryBindInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSparseImageMemoryBindInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageMemoryBindInfo where
        unsafeAddr (VkSparseImageMemoryBindInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageMemoryBindInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageMemoryBindInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageMemoryBindInfo where
        type StructFields VkSparseImageMemoryBindInfo =
             '["image", "bindCount", "pBinds"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageMemoryBindInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "image" VkSparseImageMemoryBindInfo where
        type FieldType "image" VkSparseImageMemoryBindInfo = VkImage
        type FieldOptional "image" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkSparseImageMemoryBindInfo =
             #{offset VkSparseImageMemoryBindInfo, image}
        type FieldIsArray "image" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkSparseImageMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBindInfo, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkSparseImageMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         HasField "bindCount" VkSparseImageMemoryBindInfo where
        type FieldType "bindCount" VkSparseImageMemoryBindInfo = Word32
        type FieldOptional "bindCount" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "bindCount" VkSparseImageMemoryBindInfo =
             #{offset VkSparseImageMemoryBindInfo, bindCount}
        type FieldIsArray "bindCount" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanReadField "bindCount" VkSparseImageMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBindInfo, bindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bindCount" VkSparseImageMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         HasField "pBinds" VkSparseImageMemoryBindInfo where
        type FieldType "pBinds" VkSparseImageMemoryBindInfo =
             Ptr VkSparseImageMemoryBind
        type FieldOptional "pBinds" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pBinds" VkSparseImageMemoryBindInfo =
             #{offset VkSparseImageMemoryBindInfo, pBinds}
        type FieldIsArray "pBinds" VkSparseImageMemoryBindInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanReadField "pBinds" VkSparseImageMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryBindInfo, pBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pBinds" VkSparseImageMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryBindInfo, pBinds}

instance Show VkSparseImageMemoryBindInfo where
        showsPrec d x
          = showString "VkSparseImageMemoryBindInfo {" .
              showString "image = " .
                showsPrec d (getField @"image" x) .
                  showString ", " .
                    showString "bindCount = " .
                      showsPrec d (getField @"bindCount" x) .
                        showString ", " .
                          showString "pBinds = " .
                            showsPrec d (getField @"pBinds" x) . showChar '}'

-- | > typedef struct VkSparseImageMemoryRequirements {
--   >     VkSparseImageFormatProperties formatProperties;
--   >     uint32_t               imageMipTailFirstLod;
--   >     VkDeviceSize           imageMipTailSize;
--   >     VkDeviceSize           imageMipTailOffset;
--   >     VkDeviceSize           imageMipTailStride;
--   > } VkSparseImageMemoryRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageMemoryRequirements VkSparseImageMemoryRequirements registry at www.khronos.org>
data VkSparseImageMemoryRequirements = VkSparseImageMemoryRequirements## Addr##
                                                                        ByteArray##

instance Eq VkSparseImageMemoryRequirements where
        (VkSparseImageMemoryRequirements## a _) ==
          x@(VkSparseImageMemoryRequirements## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageMemoryRequirements where
        (VkSparseImageMemoryRequirements## a _) `compare`
          x@(VkSparseImageMemoryRequirements## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageMemoryRequirements where
        sizeOf ~_ = #{size VkSparseImageMemoryRequirements}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageMemoryRequirements}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageMemoryRequirements where
        unsafeAddr (VkSparseImageMemoryRequirements## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageMemoryRequirements## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageMemoryRequirements##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageMemoryRequirements where
        type StructFields VkSparseImageMemoryRequirements =
             '["formatProperties", "imageMipTailFirstLod", "imageMipTailSize", -- ' closing tick for hsc2hs
               "imageMipTailOffset", "imageMipTailStride"]
        type CUnionType VkSparseImageMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageMemoryRequirements = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageMemoryRequirements = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "formatProperties" VkSparseImageMemoryRequirements where
        type FieldType "formatProperties" VkSparseImageMemoryRequirements =
             VkSparseImageFormatProperties
        type FieldOptional "formatProperties"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "formatProperties" VkSparseImageMemoryRequirements
             =
             #{offset VkSparseImageMemoryRequirements, formatProperties}
        type FieldIsArray "formatProperties"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements, formatProperties}

instance {-# OVERLAPPING #-}
         CanReadField "formatProperties" VkSparseImageMemoryRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, formatProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, formatProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "formatProperties" VkSparseImageMemoryRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, formatProperties}

instance {-# OVERLAPPING #-}
         HasField "imageMipTailFirstLod" VkSparseImageMemoryRequirements
         where
        type FieldType "imageMipTailFirstLod"
               VkSparseImageMemoryRequirements
             = Word32
        type FieldOptional "imageMipTailFirstLod"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageMipTailFirstLod"
               VkSparseImageMemoryRequirements
             =
             #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}
        type FieldIsArray "imageMipTailFirstLod"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}

instance {-# OVERLAPPING #-}
         CanReadField "imageMipTailFirstLod" VkSparseImageMemoryRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}

instance {-# OVERLAPPING #-}
         CanWriteField "imageMipTailFirstLod"
           VkSparseImageMemoryRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailFirstLod}

instance {-# OVERLAPPING #-}
         HasField "imageMipTailSize" VkSparseImageMemoryRequirements where
        type FieldType "imageMipTailSize" VkSparseImageMemoryRequirements =
             VkDeviceSize
        type FieldOptional "imageMipTailSize"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageMipTailSize" VkSparseImageMemoryRequirements
             =
             #{offset VkSparseImageMemoryRequirements, imageMipTailSize}
        type FieldIsArray "imageMipTailSize"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements, imageMipTailSize}

instance {-# OVERLAPPING #-}
         CanReadField "imageMipTailSize" VkSparseImageMemoryRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailSize}

instance {-# OVERLAPPING #-}
         CanWriteField "imageMipTailSize" VkSparseImageMemoryRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailSize}

instance {-# OVERLAPPING #-}
         HasField "imageMipTailOffset" VkSparseImageMemoryRequirements where
        type FieldType "imageMipTailOffset" VkSparseImageMemoryRequirements
             = VkDeviceSize
        type FieldOptional "imageMipTailOffset"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageMipTailOffset"
               VkSparseImageMemoryRequirements
             =
             #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}
        type FieldIsArray "imageMipTailOffset"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}

instance {-# OVERLAPPING #-}
         CanReadField "imageMipTailOffset" VkSparseImageMemoryRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "imageMipTailOffset" VkSparseImageMemoryRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailOffset}

instance {-# OVERLAPPING #-}
         HasField "imageMipTailStride" VkSparseImageMemoryRequirements where
        type FieldType "imageMipTailStride" VkSparseImageMemoryRequirements
             = VkDeviceSize
        type FieldOptional "imageMipTailStride"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageMipTailStride"
               VkSparseImageMemoryRequirements
             =
             #{offset VkSparseImageMemoryRequirements, imageMipTailStride}
        type FieldIsArray "imageMipTailStride"
               VkSparseImageMemoryRequirements
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements, imageMipTailStride}

instance {-# OVERLAPPING #-}
         CanReadField "imageMipTailStride" VkSparseImageMemoryRequirements
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements, imageMipTailStride})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailStride}

instance {-# OVERLAPPING #-}
         CanWriteField "imageMipTailStride" VkSparseImageMemoryRequirements
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements, imageMipTailStride}

instance Show VkSparseImageMemoryRequirements where
        showsPrec d x
          = showString "VkSparseImageMemoryRequirements {" .
              showString "formatProperties = " .
                showsPrec d (getField @"formatProperties" x) .
                  showString ", " .
                    showString "imageMipTailFirstLod = " .
                      showsPrec d (getField @"imageMipTailFirstLod" x) .
                        showString ", " .
                          showString "imageMipTailSize = " .
                            showsPrec d (getField @"imageMipTailSize" x) .
                              showString ", " .
                                showString "imageMipTailOffset = " .
                                  showsPrec d (getField @"imageMipTailOffset" x) .
                                    showString ", " .
                                      showString "imageMipTailStride = " .
                                        showsPrec d (getField @"imageMipTailStride" x) .
                                          showChar '}'

-- | > typedef struct VkSparseImageMemoryRequirements2 {
--   >     VkStructureType sType;
--   >     void*                                       pNext;
--   >     VkSparseImageMemoryRequirements                                      memoryRequirements;
--   > } VkSparseImageMemoryRequirements2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageMemoryRequirements2 VkSparseImageMemoryRequirements2 registry at www.khronos.org>
data VkSparseImageMemoryRequirements2 = VkSparseImageMemoryRequirements2## Addr##
                                                                          ByteArray##

instance Eq VkSparseImageMemoryRequirements2 where
        (VkSparseImageMemoryRequirements2## a _) ==
          x@(VkSparseImageMemoryRequirements2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageMemoryRequirements2 where
        (VkSparseImageMemoryRequirements2## a _) `compare`
          x@(VkSparseImageMemoryRequirements2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageMemoryRequirements2 where
        sizeOf ~_ = #{size VkSparseImageMemoryRequirements2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageMemoryRequirements2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageMemoryRequirements2 where
        unsafeAddr (VkSparseImageMemoryRequirements2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageMemoryRequirements2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageMemoryRequirements2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageMemoryRequirements2 where
        type StructFields VkSparseImageMemoryRequirements2 =
             '["sType", "pNext", "memoryRequirements"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageMemoryRequirements2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageMemoryRequirements2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageMemoryRequirements2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSparseImageMemoryRequirements2 where
        type FieldType "sType" VkSparseImageMemoryRequirements2 =
             VkStructureType
        type FieldOptional "sType" VkSparseImageMemoryRequirements2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSparseImageMemoryRequirements2 =
             #{offset VkSparseImageMemoryRequirements2, sType}
        type FieldIsArray "sType" VkSparseImageMemoryRequirements2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSparseImageMemoryRequirements2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSparseImageMemoryRequirements2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSparseImageMemoryRequirements2 where
        type FieldType "pNext" VkSparseImageMemoryRequirements2 = Ptr Void
        type FieldOptional "pNext" VkSparseImageMemoryRequirements2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSparseImageMemoryRequirements2 =
             #{offset VkSparseImageMemoryRequirements2, pNext}
        type FieldIsArray "pNext" VkSparseImageMemoryRequirements2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSparseImageMemoryRequirements2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSparseImageMemoryRequirements2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryRequirements" VkSparseImageMemoryRequirements2
         where
        type FieldType "memoryRequirements"
               VkSparseImageMemoryRequirements2
             = VkSparseImageMemoryRequirements
        type FieldOptional "memoryRequirements"
               VkSparseImageMemoryRequirements2
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryRequirements"
               VkSparseImageMemoryRequirements2
             =
             #{offset VkSparseImageMemoryRequirements2, memoryRequirements}
        type FieldIsArray "memoryRequirements"
               VkSparseImageMemoryRequirements2
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageMemoryRequirements2, memoryRequirements}

instance {-# OVERLAPPING #-}
         CanReadField "memoryRequirements" VkSparseImageMemoryRequirements2
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageMemoryRequirements2, memoryRequirements})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageMemoryRequirements2, memoryRequirements}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryRequirements" VkSparseImageMemoryRequirements2
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageMemoryRequirements2, memoryRequirements}

instance Show VkSparseImageMemoryRequirements2 where
        showsPrec d x
          = showString "VkSparseImageMemoryRequirements2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryRequirements = " .
                            showsPrec d (getField @"memoryRequirements" x) . showChar '}'

-- | Alias for `VkSparseImageMemoryRequirements2`
type VkSparseImageMemoryRequirements2KHR =
     VkSparseImageMemoryRequirements2

-- | > typedef struct VkSparseImageOpaqueMemoryBindInfo {
--   >     VkImage image;
--   >     uint32_t               bindCount;
--   >     const VkSparseMemoryBind* pBinds;
--   > } VkSparseImageOpaqueMemoryBindInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseImageOpaqueMemoryBindInfo VkSparseImageOpaqueMemoryBindInfo registry at www.khronos.org>
data VkSparseImageOpaqueMemoryBindInfo = VkSparseImageOpaqueMemoryBindInfo## Addr##
                                                                            ByteArray##

instance Eq VkSparseImageOpaqueMemoryBindInfo where
        (VkSparseImageOpaqueMemoryBindInfo## a _) ==
          x@(VkSparseImageOpaqueMemoryBindInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageOpaqueMemoryBindInfo where
        (VkSparseImageOpaqueMemoryBindInfo## a _) `compare`
          x@(VkSparseImageOpaqueMemoryBindInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageOpaqueMemoryBindInfo where
        sizeOf ~_ = #{size VkSparseImageOpaqueMemoryBindInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageOpaqueMemoryBindInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageOpaqueMemoryBindInfo where
        unsafeAddr (VkSparseImageOpaqueMemoryBindInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageOpaqueMemoryBindInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageOpaqueMemoryBindInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageOpaqueMemoryBindInfo where
        type StructFields VkSparseImageOpaqueMemoryBindInfo =
             '["image", "bindCount", "pBinds"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageOpaqueMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageOpaqueMemoryBindInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageOpaqueMemoryBindInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "image" VkSparseImageOpaqueMemoryBindInfo where
        type FieldType "image" VkSparseImageOpaqueMemoryBindInfo = VkImage
        type FieldOptional "image" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkSparseImageOpaqueMemoryBindInfo =
             #{offset VkSparseImageOpaqueMemoryBindInfo, image}
        type FieldIsArray "image" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageOpaqueMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkSparseImageOpaqueMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageOpaqueMemoryBindInfo, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkSparseImageOpaqueMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, image}

instance {-# OVERLAPPING #-}
         HasField "bindCount" VkSparseImageOpaqueMemoryBindInfo where
        type FieldType "bindCount" VkSparseImageOpaqueMemoryBindInfo =
             Word32
        type FieldOptional "bindCount" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "bindCount" VkSparseImageOpaqueMemoryBindInfo =
             #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}
        type FieldIsArray "bindCount" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanReadField "bindCount" VkSparseImageOpaqueMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         CanWriteField "bindCount" VkSparseImageOpaqueMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, bindCount}

instance {-# OVERLAPPING #-}
         HasField "pBinds" VkSparseImageOpaqueMemoryBindInfo where
        type FieldType "pBinds" VkSparseImageOpaqueMemoryBindInfo =
             Ptr VkSparseMemoryBind
        type FieldOptional "pBinds" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pBinds" VkSparseImageOpaqueMemoryBindInfo =
             #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}
        type FieldIsArray "pBinds" VkSparseImageOpaqueMemoryBindInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanReadField "pBinds" VkSparseImageOpaqueMemoryBindInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}

instance {-# OVERLAPPING #-}
         CanWriteField "pBinds" VkSparseImageOpaqueMemoryBindInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageOpaqueMemoryBindInfo, pBinds}

instance Show VkSparseImageOpaqueMemoryBindInfo where
        showsPrec d x
          = showString "VkSparseImageOpaqueMemoryBindInfo {" .
              showString "image = " .
                showsPrec d (getField @"image" x) .
                  showString ", " .
                    showString "bindCount = " .
                      showsPrec d (getField @"bindCount" x) .
                        showString ", " .
                          showString "pBinds = " .
                            showsPrec d (getField @"pBinds" x) . showChar '}'

-- | > typedef struct VkSparseMemoryBind {
--   >     VkDeviceSize           resourceOffset;
--   >     VkDeviceSize           size;
--   >     VkDeviceMemory         memory;
--   >     VkDeviceSize           memoryOffset;
--   >     VkSparseMemoryBindFlagsflags;
--   > } VkSparseMemoryBind;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSparseMemoryBind VkSparseMemoryBind registry at www.khronos.org>
data VkSparseMemoryBind = VkSparseMemoryBind## Addr## ByteArray##

instance Eq VkSparseMemoryBind where
        (VkSparseMemoryBind## a _) == x@(VkSparseMemoryBind## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseMemoryBind where
        (VkSparseMemoryBind## a _) `compare` x@(VkSparseMemoryBind## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseMemoryBind where
        sizeOf ~_ = #{size VkSparseMemoryBind}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSparseMemoryBind}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseMemoryBind where
        unsafeAddr (VkSparseMemoryBind## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseMemoryBind## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseMemoryBind## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseMemoryBind where
        type StructFields VkSparseMemoryBind =
             '["resourceOffset", "size", "memory", "memoryOffset", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSparseMemoryBind = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "resourceOffset" VkSparseMemoryBind where
        type FieldType "resourceOffset" VkSparseMemoryBind = VkDeviceSize
        type FieldOptional "resourceOffset" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "resourceOffset" VkSparseMemoryBind =
             #{offset VkSparseMemoryBind, resourceOffset}
        type FieldIsArray "resourceOffset" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseMemoryBind, resourceOffset}

instance {-# OVERLAPPING #-}
         CanReadField "resourceOffset" VkSparseMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, resourceOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseMemoryBind, resourceOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "resourceOffset" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseMemoryBind, resourceOffset}

instance {-# OVERLAPPING #-} HasField "size" VkSparseMemoryBind
         where
        type FieldType "size" VkSparseMemoryBind = VkDeviceSize
        type FieldOptional "size" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkSparseMemoryBind =
             #{offset VkSparseMemoryBind, size}
        type FieldIsArray "size" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseMemoryBind, size}

instance {-# OVERLAPPING #-} CanReadField "size" VkSparseMemoryBind
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseMemoryBind, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseMemoryBind, size}

instance {-# OVERLAPPING #-} HasField "memory" VkSparseMemoryBind
         where
        type FieldType "memory" VkSparseMemoryBind = VkDeviceMemory
        type FieldOptional "memory" VkSparseMemoryBind = 'True -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkSparseMemoryBind =
             #{offset VkSparseMemoryBind, memory}
        type FieldIsArray "memory" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseMemoryBind, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkSparseMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseMemoryBind, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseMemoryBind, memory}

instance {-# OVERLAPPING #-}
         HasField "memoryOffset" VkSparseMemoryBind where
        type FieldType "memoryOffset" VkSparseMemoryBind = VkDeviceSize
        type FieldOptional "memoryOffset" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryOffset" VkSparseMemoryBind =
             #{offset VkSparseMemoryBind, memoryOffset}
        type FieldIsArray "memoryOffset" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-}
         CanReadField "memoryOffset" VkSparseMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, memoryOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryOffset" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseMemoryBind, memoryOffset}

instance {-# OVERLAPPING #-} HasField "flags" VkSparseMemoryBind
         where
        type FieldType "flags" VkSparseMemoryBind = VkSparseMemoryBindFlags
        type FieldOptional "flags" VkSparseMemoryBind = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSparseMemoryBind =
             #{offset VkSparseMemoryBind, flags}
        type FieldIsArray "flags" VkSparseMemoryBind = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSparseMemoryBind, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkSparseMemoryBind where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseMemoryBind, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseMemoryBind, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkSparseMemoryBind where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseMemoryBind, flags}

instance Show VkSparseMemoryBind where
        showsPrec d x
          = showString "VkSparseMemoryBind {" .
              showString "resourceOffset = " .
                showsPrec d (getField @"resourceOffset" x) .
                  showString ", " .
                    showString "size = " .
                      showsPrec d (getField @"size" x) .
                        showString ", " .
                          showString "memory = " .
                            showsPrec d (getField @"memory" x) .
                              showString ", " .
                                showString "memoryOffset = " .
                                  showsPrec d (getField @"memoryOffset" x) .
                                    showString ", " .
                                      showString "flags = " .
                                        showsPrec d (getField @"flags" x) . showChar '}'
