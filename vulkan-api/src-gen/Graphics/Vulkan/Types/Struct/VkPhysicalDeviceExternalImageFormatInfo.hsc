#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalImageFormatInfo
       (VkPhysicalDeviceExternalImageFormatInfo(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags    (VkExternalMemoryHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2 (VkPhysicalDeviceImageFormatInfo2)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalImageFormatInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   > } VkPhysicalDeviceExternalImageFormatInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceExternalImageFormatInfo.html VkPhysicalDeviceExternalImageFormatInfo registry at www.khronos.org>
data VkPhysicalDeviceExternalImageFormatInfo = VkPhysicalDeviceExternalImageFormatInfo## Addr##
                                                                                        ByteArray##

instance Eq VkPhysicalDeviceExternalImageFormatInfo where
        (VkPhysicalDeviceExternalImageFormatInfo## a _) ==
          x@(VkPhysicalDeviceExternalImageFormatInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalImageFormatInfo where
        (VkPhysicalDeviceExternalImageFormatInfo## a _) `compare`
          x@(VkPhysicalDeviceExternalImageFormatInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalImageFormatInfo where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalImageFormatInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalImageFormatInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceExternalImageFormatInfo
         where
        unsafeAddr (VkPhysicalDeviceExternalImageFormatInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalImageFormatInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalImageFormatInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalImageFormatInfo
         where
        type StructFields VkPhysicalDeviceExternalImageFormatInfo =
             '["sType", "pNext", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalImageFormatInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalImageFormatInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalImageFormatInfo =
             '[VkPhysicalDeviceImageFormatInfo2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalImageFormatInfo where
        type FieldType "sType" VkPhysicalDeviceExternalImageFormatInfo =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalImageFormatInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalImageFormatInfo =
             #{offset VkPhysicalDeviceExternalImageFormatInfo, sType}
        type FieldIsArray "sType" VkPhysicalDeviceExternalImageFormatInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalImageFormatInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceExternalImageFormatInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalImageFormatInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceExternalImageFormatInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalImageFormatInfo where
        type FieldType "pNext" VkPhysicalDeviceExternalImageFormatInfo =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalImageFormatInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalImageFormatInfo =
             #{offset VkPhysicalDeviceExternalImageFormatInfo, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceExternalImageFormatInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalImageFormatInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceExternalImageFormatInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalImageFormatInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceExternalImageFormatInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalImageFormatInfo where
        type FieldType "handleType" VkPhysicalDeviceExternalImageFormatInfo
             = VkExternalMemoryHandleTypeFlagBits
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalImageFormatInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType"
               VkPhysicalDeviceExternalImageFormatInfo
             =
             #{offset VkPhysicalDeviceExternalImageFormatInfo, handleType}
        type FieldIsArray "handleType"
               VkPhysicalDeviceExternalImageFormatInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalImageFormatInfo, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkPhysicalDeviceExternalImageFormatInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalImageFormatInfo, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfo, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkPhysicalDeviceExternalImageFormatInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfo, handleType}

instance Show VkPhysicalDeviceExternalImageFormatInfo where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalImageFormatInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) . showChar '}'
