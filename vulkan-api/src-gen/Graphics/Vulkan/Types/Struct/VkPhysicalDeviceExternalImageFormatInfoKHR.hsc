#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalImageFormatInfoKHR
       (VkPhysicalDeviceExternalImageFormatInfoKHR(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR
                                                                                   (VkExternalMemoryHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2KHR
                                                                                   (VkPhysicalDeviceImageFormatInfo2KHR)
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalImageFormatInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   > } VkPhysicalDeviceExternalImageFormatInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceExternalImageFormatInfoKHR.html VkPhysicalDeviceExternalImageFormatInfoKHR registry at www.khronos.org>
data VkPhysicalDeviceExternalImageFormatInfoKHR = VkPhysicalDeviceExternalImageFormatInfoKHR## Addr##
                                                                                              ByteArray##

instance Eq VkPhysicalDeviceExternalImageFormatInfoKHR where
        (VkPhysicalDeviceExternalImageFormatInfoKHR## a _) ==
          x@(VkPhysicalDeviceExternalImageFormatInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalImageFormatInfoKHR where
        (VkPhysicalDeviceExternalImageFormatInfoKHR## a _) `compare`
          x@(VkPhysicalDeviceExternalImageFormatInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalImageFormatInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalImageFormatInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalImageFormatInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        unsafeAddr (VkPhysicalDeviceExternalImageFormatInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalImageFormatInfoKHR## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalImageFormatInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        type StructFields VkPhysicalDeviceExternalImageFormatInfoKHR =
             '["sType", "pNext", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalImageFormatInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalImageFormatInfoKHR =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalImageFormatInfoKHR =
             '[VkPhysicalDeviceImageFormatInfo2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalImageFormatInfoKHR where
        type FieldType "sType" VkPhysicalDeviceExternalImageFormatInfoKHR =
             VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalImageFormatInfoKHR
             =
             #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalImageFormatInfoKHR where
        type FieldType "pNext" VkPhysicalDeviceExternalImageFormatInfoKHR =
             Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalImageFormatInfoKHR
             =
             #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        type FieldType "handleType"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             =
             #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, handleType}
        type FieldIsArray "handleType"
               VkPhysicalDeviceExternalImageFormatInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType"
           VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType"
           VkPhysicalDeviceExternalImageFormatInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalImageFormatInfoKHR, handleType}

instance Show VkPhysicalDeviceExternalImageFormatInfoKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalImageFormatInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) . showChar '}'
