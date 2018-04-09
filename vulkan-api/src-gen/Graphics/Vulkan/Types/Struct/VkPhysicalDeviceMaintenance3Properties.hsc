#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceMaintenance3Properties
       (VkPhysicalDeviceMaintenance3Properties(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMaintenance3Properties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxPerSetDescriptors;
--   >     VkDeviceSize                     maxMemoryAllocationSize;
--   > } VkPhysicalDeviceMaintenance3Properties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDeviceMaintenance3PropertiesVkPhysicalDeviceMaintenance3Properties registry at www.khronos.org>
data VkPhysicalDeviceMaintenance3Properties = VkPhysicalDeviceMaintenance3Properties## Addr##
                                                                                      ByteArray##

instance Eq VkPhysicalDeviceMaintenance3Properties where
        (VkPhysicalDeviceMaintenance3Properties## a _) ==
          x@(VkPhysicalDeviceMaintenance3Properties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMaintenance3Properties where
        (VkPhysicalDeviceMaintenance3Properties## a _) `compare`
          x@(VkPhysicalDeviceMaintenance3Properties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMaintenance3Properties where
        sizeOf ~_
          = #{size VkPhysicalDeviceMaintenance3Properties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMaintenance3Properties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMaintenance3Properties
         where
        unsafeAddr (VkPhysicalDeviceMaintenance3Properties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMaintenance3Properties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMaintenance3Properties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMaintenance3Properties where
        type StructFields VkPhysicalDeviceMaintenance3Properties =
             '["sType", "pNext", "maxPerSetDescriptors", -- ' closing tick for hsc2hs
               "maxMemoryAllocationSize"]
        type CUnionType VkPhysicalDeviceMaintenance3Properties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceMaintenance3Properties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceMaintenance3Properties =
             '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMaintenance3Properties where
        type FieldType "sType" VkPhysicalDeviceMaintenance3Properties =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMaintenance3Properties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceMaintenance3Properties =
             #{offset VkPhysicalDeviceMaintenance3Properties, sType}
        type FieldIsArray "sType" VkPhysicalDeviceMaintenance3Properties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMaintenance3Properties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceMaintenance3Properties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMaintenance3Properties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMaintenance3Properties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceMaintenance3Properties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMaintenance3Properties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMaintenance3Properties where
        type FieldType "pNext" VkPhysicalDeviceMaintenance3Properties =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMaintenance3Properties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceMaintenance3Properties =
             #{offset VkPhysicalDeviceMaintenance3Properties, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceMaintenance3Properties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMaintenance3Properties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceMaintenance3Properties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMaintenance3Properties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMaintenance3Properties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceMaintenance3Properties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMaintenance3Properties, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxPerSetDescriptors"
           VkPhysicalDeviceMaintenance3Properties
         where
        type FieldType "maxPerSetDescriptors"
               VkPhysicalDeviceMaintenance3Properties
             = Word32
        type FieldOptional "maxPerSetDescriptors"
               VkPhysicalDeviceMaintenance3Properties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxPerSetDescriptors"
               VkPhysicalDeviceMaintenance3Properties
             =
             #{offset VkPhysicalDeviceMaintenance3Properties, maxPerSetDescriptors}
        type FieldIsArray "maxPerSetDescriptors"
               VkPhysicalDeviceMaintenance3Properties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMaintenance3Properties, maxPerSetDescriptors}

instance {-# OVERLAPPING #-}
         CanReadField "maxPerSetDescriptors"
           VkPhysicalDeviceMaintenance3Properties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMaintenance3Properties, maxPerSetDescriptors})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMaintenance3Properties, maxPerSetDescriptors}

instance {-# OVERLAPPING #-}
         CanWriteField "maxPerSetDescriptors"
           VkPhysicalDeviceMaintenance3Properties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMaintenance3Properties, maxPerSetDescriptors}

instance {-# OVERLAPPING #-}
         HasField "maxMemoryAllocationSize"
           VkPhysicalDeviceMaintenance3Properties
         where
        type FieldType "maxMemoryAllocationSize"
               VkPhysicalDeviceMaintenance3Properties
             = VkDeviceSize
        type FieldOptional "maxMemoryAllocationSize"
               VkPhysicalDeviceMaintenance3Properties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMemoryAllocationSize"
               VkPhysicalDeviceMaintenance3Properties
             =
             #{offset VkPhysicalDeviceMaintenance3Properties, maxMemoryAllocationSize}
        type FieldIsArray "maxMemoryAllocationSize"
               VkPhysicalDeviceMaintenance3Properties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMaintenance3Properties, maxMemoryAllocationSize}

instance {-# OVERLAPPING #-}
         CanReadField "maxMemoryAllocationSize"
           VkPhysicalDeviceMaintenance3Properties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMaintenance3Properties, maxMemoryAllocationSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceMaintenance3Properties, maxMemoryAllocationSize}

instance {-# OVERLAPPING #-}
         CanWriteField "maxMemoryAllocationSize"
           VkPhysicalDeviceMaintenance3Properties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceMaintenance3Properties, maxMemoryAllocationSize}

instance Show VkPhysicalDeviceMaintenance3Properties where
        showsPrec d x
          = showString "VkPhysicalDeviceMaintenance3Properties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxPerSetDescriptors = " .
                            showsPrec d (getField @"maxPerSetDescriptors" x) .
                              showString ", " .
                                showString "maxMemoryAllocationSize = " .
                                  showsPrec d (getField @"maxMemoryAllocationSize" x) . showChar '}'
