#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSamplerYcbcrConversionFeatures
       (VkPhysicalDeviceSamplerYcbcrConversionFeatures(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Base                                               (Addr##, ByteArray##,
                                                                         byteArrayContents##,
                                                                         plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                        (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo        (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2 (VkPhysicalDeviceFeatures2)
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSamplerYcbcrConversionFeatures {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         samplerYcbcrConversion;
--   > } VkPhysicalDeviceSamplerYcbcrConversionFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPhysicalDeviceSamplerYcbcrConversionFeatures VkPhysicalDeviceSamplerYcbcrConversionFeatures registry at www.khronos.org>
data VkPhysicalDeviceSamplerYcbcrConversionFeatures = VkPhysicalDeviceSamplerYcbcrConversionFeatures## Addr##
                                                                                                      ByteArray##

instance Eq VkPhysicalDeviceSamplerYcbcrConversionFeatures where
        (VkPhysicalDeviceSamplerYcbcrConversionFeatures## a _) ==
          x@(VkPhysicalDeviceSamplerYcbcrConversionFeatures## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSamplerYcbcrConversionFeatures where
        (VkPhysicalDeviceSamplerYcbcrConversionFeatures## a _) `compare`
          x@(VkPhysicalDeviceSamplerYcbcrConversionFeatures## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceSamplerYcbcrConversionFeatures}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSamplerYcbcrConversionFeatures}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        unsafeAddr (VkPhysicalDeviceSamplerYcbcrConversionFeatures## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceSamplerYcbcrConversionFeatures## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceSamplerYcbcrConversionFeatures##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        type StructFields VkPhysicalDeviceSamplerYcbcrConversionFeatures =
             '["sType", "pNext", "samplerYcbcrConversion"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceSamplerYcbcrConversionFeatures =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceSamplerYcbcrConversionFeatures =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceSamplerYcbcrConversionFeatures =
             '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        type FieldType "sType"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             =
             #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        type FieldType "pNext"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             =
             #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, pNext}

instance {-# OVERLAPPING #-}
         HasField "samplerYcbcrConversion"
           VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        type FieldType "samplerYcbcrConversion"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             = VkBool32
        type FieldOptional "samplerYcbcrConversion"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "samplerYcbcrConversion"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             =
             #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, samplerYcbcrConversion}
        type FieldIsArray "samplerYcbcrConversion"
               VkPhysicalDeviceSamplerYcbcrConversionFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, samplerYcbcrConversion}

instance {-# OVERLAPPING #-}
         CanReadField "samplerYcbcrConversion"
           VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, samplerYcbcrConversion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, samplerYcbcrConversion}

instance {-# OVERLAPPING #-}
         CanWriteField "samplerYcbcrConversion"
           VkPhysicalDeviceSamplerYcbcrConversionFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeatures, samplerYcbcrConversion}

instance Show VkPhysicalDeviceSamplerYcbcrConversionFeatures where
        showsPrec d x
          = showString "VkPhysicalDeviceSamplerYcbcrConversionFeatures {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "samplerYcbcrConversion = " .
                            showsPrec d (getField @"samplerYcbcrConversion" x) . showChar '}'
