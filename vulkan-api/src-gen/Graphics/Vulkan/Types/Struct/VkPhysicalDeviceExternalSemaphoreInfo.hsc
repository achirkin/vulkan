#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalSemaphoreInfo
       (VkPhysicalDeviceExternalSemaphoreInfo(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Base                                                      (Addr##,
                                                                                ByteArray##,
                                                                                byteArrayContents##,
                                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlags (VkExternalSemaphoreHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalSemaphoreInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   > } VkPhysicalDeviceExternalSemaphoreInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceExternalSemaphoreInfo VkPhysicalDeviceExternalSemaphoreInfo registry at www.khronos.org>
data VkPhysicalDeviceExternalSemaphoreInfo = VkPhysicalDeviceExternalSemaphoreInfo## Addr##
                                                                                    ByteArray##

instance Eq VkPhysicalDeviceExternalSemaphoreInfo where
        (VkPhysicalDeviceExternalSemaphoreInfo## a _) ==
          x@(VkPhysicalDeviceExternalSemaphoreInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalSemaphoreInfo where
        (VkPhysicalDeviceExternalSemaphoreInfo## a _) `compare`
          x@(VkPhysicalDeviceExternalSemaphoreInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalSemaphoreInfo where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalSemaphoreInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalSemaphoreInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceExternalSemaphoreInfo
         where
        unsafeAddr (VkPhysicalDeviceExternalSemaphoreInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalSemaphoreInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalSemaphoreInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalSemaphoreInfo where
        type StructFields VkPhysicalDeviceExternalSemaphoreInfo =
             '["sType", "pNext", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalSemaphoreInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalSemaphoreInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalSemaphoreInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalSemaphoreInfo where
        type FieldType "sType" VkPhysicalDeviceExternalSemaphoreInfo =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalSemaphoreInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalSemaphoreInfo =
             #{offset VkPhysicalDeviceExternalSemaphoreInfo, sType}
        type FieldIsArray "sType" VkPhysicalDeviceExternalSemaphoreInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalSemaphoreInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceExternalSemaphoreInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalSemaphoreInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceExternalSemaphoreInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalSemaphoreInfo where
        type FieldType "pNext" VkPhysicalDeviceExternalSemaphoreInfo =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalSemaphoreInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalSemaphoreInfo =
             #{offset VkPhysicalDeviceExternalSemaphoreInfo, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceExternalSemaphoreInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalSemaphoreInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceExternalSemaphoreInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalSemaphoreInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceExternalSemaphoreInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalSemaphoreInfo where
        type FieldType "handleType" VkPhysicalDeviceExternalSemaphoreInfo =
             VkExternalSemaphoreHandleTypeFlagBits
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalSemaphoreInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkPhysicalDeviceExternalSemaphoreInfo
             =
             #{offset VkPhysicalDeviceExternalSemaphoreInfo, handleType}
        type FieldIsArray "handleType"
               VkPhysicalDeviceExternalSemaphoreInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalSemaphoreInfo, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkPhysicalDeviceExternalSemaphoreInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalSemaphoreInfo, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfo, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkPhysicalDeviceExternalSemaphoreInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfo, handleType}

instance Show VkPhysicalDeviceExternalSemaphoreInfo where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalSemaphoreInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) . showChar '}'
