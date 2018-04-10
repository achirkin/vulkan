#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDevicePushDescriptorPropertiesKHR
       (VkPhysicalDevicePushDescriptorPropertiesKHR(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDevicePushDescriptorPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxPushDescriptors;
--   > } VkPhysicalDevicePushDescriptorPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDevicePushDescriptorPropertiesKHR VkPhysicalDevicePushDescriptorPropertiesKHR registry at www.khronos.org>
data VkPhysicalDevicePushDescriptorPropertiesKHR = VkPhysicalDevicePushDescriptorPropertiesKHR## Addr##
                                                                                                ByteArray##

instance Eq VkPhysicalDevicePushDescriptorPropertiesKHR where
        (VkPhysicalDevicePushDescriptorPropertiesKHR## a _) ==
          x@(VkPhysicalDevicePushDescriptorPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDevicePushDescriptorPropertiesKHR where
        (VkPhysicalDevicePushDescriptorPropertiesKHR## a _) `compare`
          x@(VkPhysicalDevicePushDescriptorPropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDevicePushDescriptorPropertiesKHR where
        sizeOf ~_
          = #{size VkPhysicalDevicePushDescriptorPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDevicePushDescriptorPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        unsafeAddr (VkPhysicalDevicePushDescriptorPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDevicePushDescriptorPropertiesKHR## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDevicePushDescriptorPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        type StructFields VkPhysicalDevicePushDescriptorPropertiesKHR =
             '["sType", "pNext", "maxPushDescriptors"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDevicePushDescriptorPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDevicePushDescriptorPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDevicePushDescriptorPropertiesKHR =
             '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDevicePushDescriptorPropertiesKHR where
        type FieldType "sType" VkPhysicalDevicePushDescriptorPropertiesKHR
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             =
             #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}
        type FieldIsArray "sType"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDevicePushDescriptorPropertiesKHR where
        type FieldType "pNext" VkPhysicalDevicePushDescriptorPropertiesKHR
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             =
             #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxPushDescriptors"
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        type FieldType "maxPushDescriptors"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = Word32
        type FieldOptional "maxPushDescriptors"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPushDescriptors"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             =
             #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}
        type FieldIsArray "maxPushDescriptors"
               VkPhysicalDevicePushDescriptorPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}

instance {-# OVERLAPPING #-}
         CanReadField "maxPushDescriptors"
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPushDescriptors"
           VkPhysicalDevicePushDescriptorPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevicePushDescriptorPropertiesKHR, maxPushDescriptors}

instance Show VkPhysicalDevicePushDescriptorPropertiesKHR where
        showsPrec d x
          = showString "VkPhysicalDevicePushDescriptorPropertiesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxPushDescriptors = " .
                            showsPrec d (getField @"maxPushDescriptors" x) . showChar '}'
