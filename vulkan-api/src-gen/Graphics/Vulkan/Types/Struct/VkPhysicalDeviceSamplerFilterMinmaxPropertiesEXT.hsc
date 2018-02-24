#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
       (VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                             (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                  pNext;
--   >     VkBool32               filterMinmaxSingleComponentFormats;
--   >     VkBool32               filterMinmaxImageComponentMapping;
--   > } VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT.html VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT registry at www.khronos.org>
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
        type CUnionType VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

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
        type FieldIsArray "sType"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "sType"
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
        type FieldIsArray "pNext"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "pNext"
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
        type FieldIsArray "filterMinmaxSingleComponentFormats"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "filterMinmaxSingleComponentFormats"
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkFilterMinmaxSingleComponentFormats

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
        type FieldIsArray "filterMinmaxImageComponentMapping"
               VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "filterMinmaxImageComponentMapping"
           VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkFilterMinmaxImageComponentMapping

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
