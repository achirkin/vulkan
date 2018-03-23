#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupCommandBufferBeginInfo
       (VkDeviceGroupCommandBufferBeginInfo(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo (VkCommandBufferBeginInfo)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupCommandBufferBeginInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   > } VkDeviceGroupCommandBufferBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDeviceGroupCommandBufferBeginInfo.html VkDeviceGroupCommandBufferBeginInfo registry at www.khronos.org>
data VkDeviceGroupCommandBufferBeginInfo = VkDeviceGroupCommandBufferBeginInfo## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupCommandBufferBeginInfo where
        (VkDeviceGroupCommandBufferBeginInfo## a _) ==
          x@(VkDeviceGroupCommandBufferBeginInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupCommandBufferBeginInfo where
        (VkDeviceGroupCommandBufferBeginInfo## a _) `compare`
          x@(VkDeviceGroupCommandBufferBeginInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupCommandBufferBeginInfo where
        sizeOf ~_ = #{size VkDeviceGroupCommandBufferBeginInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupCommandBufferBeginInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupCommandBufferBeginInfo
         where
        unsafeAddr (VkDeviceGroupCommandBufferBeginInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupCommandBufferBeginInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupCommandBufferBeginInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupCommandBufferBeginInfo where
        type StructFields VkDeviceGroupCommandBufferBeginInfo =
             '["sType", "pNext", "deviceMask"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupCommandBufferBeginInfo =
             '[VkCommandBufferBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupCommandBufferBeginInfo where
        type FieldType "sType" VkDeviceGroupCommandBufferBeginInfo =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupCommandBufferBeginInfo =
             #{offset VkDeviceGroupCommandBufferBeginInfo, sType}
        type FieldIsArray "sType" VkDeviceGroupCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupCommandBufferBeginInfo where
        type FieldType "pNext" VkDeviceGroupCommandBufferBeginInfo =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupCommandBufferBeginInfo =
             #{offset VkDeviceGroupCommandBufferBeginInfo, pNext}
        type FieldIsArray "pNext" VkDeviceGroupCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkDeviceGroupCommandBufferBeginInfo where
        type FieldType "deviceMask" VkDeviceGroupCommandBufferBeginInfo =
             Word32
        type FieldOptional "deviceMask" VkDeviceGroupCommandBufferBeginInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkDeviceGroupCommandBufferBeginInfo =
             #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask}
        type FieldIsArray "deviceMask" VkDeviceGroupCommandBufferBeginInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkDeviceGroupCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkDeviceGroupCommandBufferBeginInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask}

instance Show VkDeviceGroupCommandBufferBeginInfo where
        showsPrec d x
          = showString "VkDeviceGroupCommandBufferBeginInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceMask = " .
                            showsPrec d (getField @"deviceMask" x) . showChar '}'
