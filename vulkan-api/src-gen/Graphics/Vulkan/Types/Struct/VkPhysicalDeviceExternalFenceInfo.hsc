#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalFenceInfo
       (VkPhysicalDeviceExternalFenceInfo(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Base                                                  (Addr##,
                                                                            ByteArray##,
                                                                            byteArrayContents##,
                                                                            plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags (VkExternalFenceHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalFenceInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalFenceHandleTypeFlagBits handleType;
--   > } VkPhysicalDeviceExternalFenceInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceExternalFenceInfo VkPhysicalDeviceExternalFenceInfo registry at www.khronos.org>
data VkPhysicalDeviceExternalFenceInfo = VkPhysicalDeviceExternalFenceInfo## Addr##
                                                                            ByteArray##

instance Eq VkPhysicalDeviceExternalFenceInfo where
        (VkPhysicalDeviceExternalFenceInfo## a _) ==
          x@(VkPhysicalDeviceExternalFenceInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalFenceInfo where
        (VkPhysicalDeviceExternalFenceInfo## a _) `compare`
          x@(VkPhysicalDeviceExternalFenceInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalFenceInfo where
        sizeOf ~_ = #{size VkPhysicalDeviceExternalFenceInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalFenceInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceExternalFenceInfo where
        unsafeAddr (VkPhysicalDeviceExternalFenceInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalFenceInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalFenceInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalFenceInfo where
        type StructFields VkPhysicalDeviceExternalFenceInfo =
             '["sType", "pNext", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalFenceInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalFenceInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalFenceInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalFenceInfo where
        type FieldType "sType" VkPhysicalDeviceExternalFenceInfo =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalFenceInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalFenceInfo =
             #{offset VkPhysicalDeviceExternalFenceInfo, sType}
        type FieldIsArray "sType" VkPhysicalDeviceExternalFenceInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalFenceInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceExternalFenceInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalFenceInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalFenceInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceExternalFenceInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalFenceInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalFenceInfo where
        type FieldType "pNext" VkPhysicalDeviceExternalFenceInfo = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalFenceInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalFenceInfo =
             #{offset VkPhysicalDeviceExternalFenceInfo, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceExternalFenceInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalFenceInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceExternalFenceInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalFenceInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalFenceInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceExternalFenceInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalFenceInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalFenceInfo where
        type FieldType "handleType" VkPhysicalDeviceExternalFenceInfo =
             VkExternalFenceHandleTypeFlagBits
        type FieldOptional "handleType" VkPhysicalDeviceExternalFenceInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkPhysicalDeviceExternalFenceInfo =
             #{offset VkPhysicalDeviceExternalFenceInfo, handleType}
        type FieldIsArray "handleType" VkPhysicalDeviceExternalFenceInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalFenceInfo, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkPhysicalDeviceExternalFenceInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalFenceInfo, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalFenceInfo, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkPhysicalDeviceExternalFenceInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalFenceInfo, handleType}

instance Show VkPhysicalDeviceExternalFenceInfo where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalFenceInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) . showChar '}'
