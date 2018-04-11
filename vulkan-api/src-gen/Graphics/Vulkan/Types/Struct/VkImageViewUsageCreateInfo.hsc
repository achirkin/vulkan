#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageViewUsageCreateInfo
       (VkImageViewUsageCreateInfo(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Base                                           (Addr##,
                                                                     ByteArray##,
                                                                     byteArrayContents##,
                                                                     plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags       (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo (VkImageViewCreateInfo)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkImageViewUsageCreateInfo {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkImageUsageFlags usage;
--   > } VkImageViewUsageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageViewUsageCreateInfo VkImageViewUsageCreateInfo registry at www.khronos.org>
data VkImageViewUsageCreateInfo = VkImageViewUsageCreateInfo## Addr##
                                                              ByteArray##

instance Eq VkImageViewUsageCreateInfo where
        (VkImageViewUsageCreateInfo## a _) ==
          x@(VkImageViewUsageCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageViewUsageCreateInfo where
        (VkImageViewUsageCreateInfo## a _) `compare`
          x@(VkImageViewUsageCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageViewUsageCreateInfo where
        sizeOf ~_ = #{size VkImageViewUsageCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageViewUsageCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageViewUsageCreateInfo where
        unsafeAddr (VkImageViewUsageCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageViewUsageCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageViewUsageCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageViewUsageCreateInfo where
        type StructFields VkImageViewUsageCreateInfo =
             '["sType", "pNext", "usage"] -- ' closing tick for hsc2hs
        type CUnionType VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageViewUsageCreateInfo =
             '[VkImageViewCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageViewUsageCreateInfo where
        type FieldType "sType" VkImageViewUsageCreateInfo = VkStructureType
        type FieldOptional "sType" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageViewUsageCreateInfo =
             #{offset VkImageViewUsageCreateInfo, sType}
        type FieldIsArray "sType" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageViewUsageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewUsageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageViewUsageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageViewUsageCreateInfo where
        type FieldType "pNext" VkImageViewUsageCreateInfo = Ptr Void
        type FieldOptional "pNext" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageViewUsageCreateInfo =
             #{offset VkImageViewUsageCreateInfo, pNext}
        type FieldIsArray "pNext" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageViewUsageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewUsageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageViewUsageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "usage" VkImageViewUsageCreateInfo where
        type FieldType "usage" VkImageViewUsageCreateInfo =
             VkImageUsageFlags
        type FieldOptional "usage" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkImageViewUsageCreateInfo =
             #{offset VkImageViewUsageCreateInfo, usage}
        type FieldIsArray "usage" VkImageViewUsageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfo, usage}

instance {-# OVERLAPPING #-}
         CanReadField "usage" VkImageViewUsageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfo, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewUsageCreateInfo, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkImageViewUsageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfo, usage}

instance Show VkImageViewUsageCreateInfo where
        showsPrec d x
          = showString "VkImageViewUsageCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "usage = " .
                            showsPrec d (getField @"usage" x) . showChar '}'
