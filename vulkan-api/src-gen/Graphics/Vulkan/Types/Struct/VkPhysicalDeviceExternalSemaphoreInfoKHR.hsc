#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalSemaphoreInfoKHR
       (VkPhysicalDeviceExternalSemaphoreInfoKHR(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlagsKHR
                                                                                   (VkExternalSemaphoreHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                   (VkStructureType)
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalSemaphoreInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalSemaphoreHandleTypeFlagBitsKHR handleType;
--   > } VkPhysicalDeviceExternalSemaphoreInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceExternalSemaphoreInfoKHR.html VkPhysicalDeviceExternalSemaphoreInfoKHR registry at www.khronos.org>
data VkPhysicalDeviceExternalSemaphoreInfoKHR = VkPhysicalDeviceExternalSemaphoreInfoKHR## Addr##
                                                                                          ByteArray##

instance Eq VkPhysicalDeviceExternalSemaphoreInfoKHR where
        (VkPhysicalDeviceExternalSemaphoreInfoKHR## a _) ==
          x@(VkPhysicalDeviceExternalSemaphoreInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalSemaphoreInfoKHR where
        (VkPhysicalDeviceExternalSemaphoreInfoKHR## a _) `compare`
          x@(VkPhysicalDeviceExternalSemaphoreInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalSemaphoreInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalSemaphoreInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalSemaphoreInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        unsafeAddr (VkPhysicalDeviceExternalSemaphoreInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalSemaphoreInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalSemaphoreInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        type StructFields VkPhysicalDeviceExternalSemaphoreInfoKHR =
             '["sType", "pNext", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalSemaphoreInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalSemaphoreInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalSemaphoreInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR where
        type FieldType "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR =
             #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR where
        type FieldType "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR =
             #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        type FieldType "handleType"
               VkPhysicalDeviceExternalSemaphoreInfoKHR
             = VkExternalSemaphoreHandleTypeFlagBitsKHR
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalSemaphoreInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType"
               VkPhysicalDeviceExternalSemaphoreInfoKHR
             =
             #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType}
        type FieldIsArray "handleType"
               VkPhysicalDeviceExternalSemaphoreInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkPhysicalDeviceExternalSemaphoreInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalSemaphoreInfoKHR, handleType}

instance Show VkPhysicalDeviceExternalSemaphoreInfoKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalSemaphoreInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) . showChar '}'
