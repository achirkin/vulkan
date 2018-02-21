#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR where
        type VkSTypeMType VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, sType}

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

instance CanReadField "sType"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR where
        type VkPNextMType VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, pNext}

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

instance CanReadField "pNext"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSamplerYcbcrConversion
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        type VkSamplerYcbcrConversionMType
               VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
             = VkBool32

        {-# NOINLINE vkSamplerYcbcrConversion #-}
        vkSamplerYcbcrConversion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion})

        {-# INLINE vkSamplerYcbcrConversionByteOffset #-}
        vkSamplerYcbcrConversionByteOffset ~_
          = #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion}

        {-# INLINE readVkSamplerYcbcrConversion #-}
        readVkSamplerYcbcrConversion p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion}

        {-# INLINE writeVkSamplerYcbcrConversion #-}
        writeVkSamplerYcbcrConversion p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR, samplerYcbcrConversion}

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

instance CanReadField "samplerYcbcrConversion"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkSamplerYcbcrConversion

        {-# INLINE readField #-}
        readField = readVkSamplerYcbcrConversion

instance CanWriteField "samplerYcbcrConversion"
           VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSamplerYcbcrConversion

instance Show VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR
         where
        showsPrec d x
          = showString "VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSamplerYcbcrConversion = " .
                            showsPrec d (vkSamplerYcbcrConversion x) . showChar '}'
