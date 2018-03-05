#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
       (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                           (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo           (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR (VkPhysicalDeviceFeatures2KHR)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         samplerYcbcrConversion;
--   > } VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR.html VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR registry at www.khronos.org>
data VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR = VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## Addr##
                                                                                                            ByteArray##

instance Eq VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR where
        (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## a _) ==
          x@(VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## a _) `compare`
          x@(VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        unsafeAddr (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        type StructFields VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = '["sType", "pNext", "samplerYcbcrConversion"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = '[VkPhysicalDeviceFeatures2KHR, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        type FieldType "sType"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             =
             #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        type FieldType "pNext"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             =
             #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "samplerYcbcrConversion"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        type FieldType "samplerYcbcrConversion"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = VkBool32
        type FieldOptional "samplerYcbcrConversion"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "samplerYcbcrConversion"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             =
             #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion}
        type FieldIsArray "samplerYcbcrConversion"
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion}

instance {-# OVERLAPPING #-}
         CanReadField "samplerYcbcrConversion"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion}

instance {-# OVERLAPPING #-}
         CanWriteField "samplerYcbcrConversion"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion}

instance Show VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        showsPrec d x
          = showString "VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "samplerYcbcrConversion = " .
                            showsPrec d (getField @"samplerYcbcrConversion" x) . showChar '}'
