#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_EXT_sampler_filter_minmax
       (-- * Vulkan extension: @VK_EXT_sampler_filter_minmax@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @131@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkSamplerReductionModeCreateInfoEXT(..),
        VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..),
        VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION,
        pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION,
        VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME,
        pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT,
        pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT,
        pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Common           (VkBool32,
                                                   VkFormatFeatureFlagBits (..),
                                                   VkSamplerReductionModeEXT,
                                                   VkStructureType,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkSamplerReductionModeCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSamplerReductionModeEXT reductionMode;
--   > } VkSamplerReductionModeCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSamplerReductionModeCreateInfoEXT.html VkSamplerReductionModeCreateInfoEXT registry at www.khronos.org>
data VkSamplerReductionModeCreateInfoEXT = VkSamplerReductionModeCreateInfoEXT## Addr##
                                                                                ByteArray##

instance Eq VkSamplerReductionModeCreateInfoEXT where
        (VkSamplerReductionModeCreateInfoEXT## a _) ==
          x@(VkSamplerReductionModeCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerReductionModeCreateInfoEXT where
        (VkSamplerReductionModeCreateInfoEXT## a _) `compare`
          x@(VkSamplerReductionModeCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerReductionModeCreateInfoEXT where
        sizeOf ~_ = #{size VkSamplerReductionModeCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerReductionModeCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSamplerReductionModeCreateInfoEXT
         where
        unsafeAddr (VkSamplerReductionModeCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSamplerReductionModeCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerReductionModeCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSamplerReductionModeCreateInfoEXT where
        type StructFields VkSamplerReductionModeCreateInfoEXT =
             '["sType", "pNext", "reductionMode"] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkSamplerReductionModeCreateInfoEXT where
        type VkSTypeMType VkSamplerReductionModeCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerReductionModeCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSamplerReductionModeCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerReductionModeCreateInfoEXT where
        type FieldType "sType" VkSamplerReductionModeCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkSamplerReductionModeCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSamplerReductionModeCreateInfoEXT =
             #{offset VkSamplerReductionModeCreateInfoEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerReductionModeCreateInfoEXT, sType}

instance CanReadField "sType" VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSamplerReductionModeCreateInfoEXT where
        type VkPNextMType VkSamplerReductionModeCreateInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerReductionModeCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerReductionModeCreateInfoEXT where
        type FieldType "pNext" VkSamplerReductionModeCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkSamplerReductionModeCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSamplerReductionModeCreateInfoEXT =
             #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerReductionModeCreateInfoEXT, pNext}

instance CanReadField "pNext" VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkReductionMode VkSamplerReductionModeCreateInfoEXT where
        type VkReductionModeMType VkSamplerReductionModeCreateInfoEXT =
             VkSamplerReductionModeEXT

        {-# NOINLINE vkReductionMode #-}
        vkReductionMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode})

        {-# INLINE vkReductionModeByteOffset #-}
        vkReductionModeByteOffset ~_
          = #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

        {-# INLINE readVkReductionMode #-}
        readVkReductionMode p
          = peekByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

        {-# INLINE writeVkReductionMode #-}
        writeVkReductionMode p
          = pokeByteOff p #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

instance {-# OVERLAPPING #-}
         HasField "reductionMode" VkSamplerReductionModeCreateInfoEXT where
        type FieldType "reductionMode" VkSamplerReductionModeCreateInfoEXT
             = VkSamplerReductionModeEXT
        type FieldOptional "reductionMode"
               VkSamplerReductionModeCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "reductionMode"
               VkSamplerReductionModeCreateInfoEXT
             =
             #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerReductionModeCreateInfoEXT, reductionMode}

instance CanReadField "reductionMode"
           VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkReductionMode

        {-# INLINE readField #-}
        readField = readVkReductionMode

instance CanWriteField "reductionMode"
           VkSamplerReductionModeCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkReductionMode

instance Show VkSamplerReductionModeCreateInfoEXT where
        showsPrec d x
          = showString "VkSamplerReductionModeCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkReductionMode = " .
                            showsPrec d (vkReductionMode x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkBool32               filterMinmaxSingleComponentFormats;
--   >     VkBool32               filterMinmaxImageComponentMapping;
--   > } VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT.html VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT = VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT## Addr##
                                                                                                          ByteArray##

instance Eq VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
        (VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT## a _) ==
          x@(VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
        (VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT## a _) `compare`
          x@(VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        unsafeAddr (VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        type StructFields VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             =
             '["sType", "pNext", "filterMinmaxSingleComponentFormats", -- ' closing tick for hsc2hs
               "filterMinmaxImageComponentMapping"]

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
        type VkSTypeMType VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             =
             #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, sType}

instance CanReadField "sType"
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
        type VkPNextMType VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             =
             #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkFilterMinmaxSingleComponentFormats
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        type VkFilterMinmaxSingleComponentFormatsMType
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = VkBool32

        {-# NOINLINE vkFilterMinmaxSingleComponentFormats #-}
        vkFilterMinmaxSingleComponentFormats x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxSingleComponentFormats})

        {-# INLINE vkFilterMinmaxSingleComponentFormatsByteOffset #-}
        vkFilterMinmaxSingleComponentFormatsByteOffset ~_
          = #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxSingleComponentFormats}

        {-# INLINE readVkFilterMinmaxSingleComponentFormats #-}
        readVkFilterMinmaxSingleComponentFormats p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxSingleComponentFormats}

        {-# INLINE writeVkFilterMinmaxSingleComponentFormats #-}
        writeVkFilterMinmaxSingleComponentFormats p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxSingleComponentFormats}

instance {-# OVERLAPPING #-}
         HasField "filterMinmaxSingleComponentFormats"
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        type FieldType "filterMinmaxSingleComponentFormats"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = VkBool32
        type FieldOptional "filterMinmaxSingleComponentFormats"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "filterMinmaxSingleComponentFormats"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             =
             #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxSingleComponentFormats}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxSingleComponentFormats}

instance CanReadField "filterMinmaxSingleComponentFormats"
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkFilterMinmaxSingleComponentFormats

        {-# INLINE readField #-}
        readField = readVkFilterMinmaxSingleComponentFormats

instance {-# OVERLAPPING #-}
         HasVkFilterMinmaxImageComponentMapping
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        type VkFilterMinmaxImageComponentMappingMType
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = VkBool32

        {-# NOINLINE vkFilterMinmaxImageComponentMapping #-}
        vkFilterMinmaxImageComponentMapping x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxImageComponentMapping})

        {-# INLINE vkFilterMinmaxImageComponentMappingByteOffset #-}
        vkFilterMinmaxImageComponentMappingByteOffset ~_
          = #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxImageComponentMapping}

        {-# INLINE readVkFilterMinmaxImageComponentMapping #-}
        readVkFilterMinmaxImageComponentMapping p
          = peekByteOff p #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxImageComponentMapping}

        {-# INLINE writeVkFilterMinmaxImageComponentMapping #-}
        writeVkFilterMinmaxImageComponentMapping p
          = pokeByteOff p #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxImageComponentMapping}

instance {-# OVERLAPPING #-}
         HasField "filterMinmaxImageComponentMapping"
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        type FieldType "filterMinmaxImageComponentMapping"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = VkBool32
        type FieldOptional "filterMinmaxImageComponentMapping"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "filterMinmaxImageComponentMapping"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             =
             #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxImageComponentMapping}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT, filterMinmaxImageComponentMapping}

instance CanReadField "filterMinmaxImageComponentMapping"
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkFilterMinmaxImageComponentMapping

        {-# INLINE readField #-}
        readField = readVkFilterMinmaxImageComponentMapping

instance Show VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        showsPrec d x
          = showString "VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFilterMinmaxSingleComponentFormats = " .
                            showsPrec d (vkFilterMinmaxSingleComponentFormats x) .
                              showString ", " .
                                showString "vkFilterMinmaxImageComponentMapping = " .
                                  showsPrec d (vkFilterMinmaxImageComponentMapping x) . showChar '}'

pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 1

type VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 1

pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: CString

pattern VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME <-
        (is_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME -> True)
  where VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
          = _VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME

{-# INLINE _VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME #-}

_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: CString
_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
  = Ptr "VK_EXT_sampler_filter_minmax\NUL"##

{-# INLINE is_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME #-}

is_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
  = eqCStrings _VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME

type VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME =
     "VK_EXT_sampler_filter_minmax"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
        = VkStructureType 1000130000

pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT =
        VkStructureType 1000130001

-- | Format can be used with min/max reduction filtering
--
--   bitpos = @16@
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT ::
        VkFormatFeatureFlagBits

pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT =
        VkFormatFeatureFlagBits 65536
