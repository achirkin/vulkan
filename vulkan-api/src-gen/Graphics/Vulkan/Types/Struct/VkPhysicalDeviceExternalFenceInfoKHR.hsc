#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalFenceInfoKHR
       (VkPhysicalDeviceExternalFenceInfoKHR(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlagsKHR (VkExternalFenceHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalFenceInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalFenceHandleTypeFlagBitsKHR handleType;
--   > } VkPhysicalDeviceExternalFenceInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceExternalFenceInfoKHR.html VkPhysicalDeviceExternalFenceInfoKHR registry at www.khronos.org>
data VkPhysicalDeviceExternalFenceInfoKHR = VkPhysicalDeviceExternalFenceInfoKHR## Addr##
                                                                                  ByteArray##

instance Eq VkPhysicalDeviceExternalFenceInfoKHR where
        (VkPhysicalDeviceExternalFenceInfoKHR## a _) ==
          x@(VkPhysicalDeviceExternalFenceInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalFenceInfoKHR where
        (VkPhysicalDeviceExternalFenceInfoKHR## a _) `compare`
          x@(VkPhysicalDeviceExternalFenceInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalFenceInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalFenceInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalFenceInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceExternalFenceInfoKHR
         where
        unsafeAddr (VkPhysicalDeviceExternalFenceInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalFenceInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalFenceInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalFenceInfoKHR where
        type StructFields VkPhysicalDeviceExternalFenceInfoKHR =
             '["sType", "pNext", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalFenceInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalFenceInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalFenceInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalFenceInfoKHR where
        type FieldType "sType" VkPhysicalDeviceExternalFenceInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalFenceInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalFenceInfoKHR =
             #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceExternalFenceInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceExternalFenceInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceExternalFenceInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalFenceInfoKHR where
        type FieldType "pNext" VkPhysicalDeviceExternalFenceInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalFenceInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalFenceInfoKHR =
             #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceExternalFenceInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceExternalFenceInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceExternalFenceInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalFenceInfoKHR where
        type FieldType "handleType" VkPhysicalDeviceExternalFenceInfoKHR =
             VkExternalFenceHandleTypeFlagBitsKHR
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalFenceInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkPhysicalDeviceExternalFenceInfoKHR
             =
             #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType}
        type FieldIsArray "handleType" VkPhysicalDeviceExternalFenceInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkPhysicalDeviceExternalFenceInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkPhysicalDeviceExternalFenceInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalFenceInfoKHR, handleType}

instance Show VkPhysicalDeviceExternalFenceInfoKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalFenceInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) . showChar '}'
