#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
       (VkPhysicalDeviceSparseProperties(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes     (VkBool32)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSparseProperties {
--   >     VkBool32               residencyStandard2DBlockShape;
--   >     VkBool32               residencyStandard2DMultisampleBlockShape;
--   >     VkBool32               residencyStandard3DBlockShape;
--   >     VkBool32               residencyAlignedMipSize;
--   >     VkBool32               residencyNonResidentStrict;
--   > } VkPhysicalDeviceSparseProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceSparseProperties.html VkPhysicalDeviceSparseProperties registry at www.khronos.org>
data VkPhysicalDeviceSparseProperties = VkPhysicalDeviceSparseProperties## Addr##
                                                                          ByteArray##

instance Eq VkPhysicalDeviceSparseProperties where
        (VkPhysicalDeviceSparseProperties## a _) ==
          x@(VkPhysicalDeviceSparseProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSparseProperties where
        (VkPhysicalDeviceSparseProperties## a _) `compare`
          x@(VkPhysicalDeviceSparseProperties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSparseProperties where
        sizeOf ~_ = #{size VkPhysicalDeviceSparseProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSparseProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceSparseProperties where
        unsafeAddr (VkPhysicalDeviceSparseProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceSparseProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceSparseProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceSparseProperties where
        type StructFields VkPhysicalDeviceSparseProperties =
             '["residencyStandard2DBlockShape", -- ' closing tick for hsc2hs
               "residencyStandard2DMultisampleBlockShape",
               "residencyStandard3DBlockShape", "residencyAlignedMipSize",
               "residencyNonResidentStrict"]
        type CUnionType VkPhysicalDeviceSparseProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceSparseProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceSparseProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkResidencyStandard2DBlockShape VkPhysicalDeviceSparseProperties
         where
        type VkResidencyStandard2DBlockShapeMType
               VkPhysicalDeviceSparseProperties
             = VkBool32

        {-# NOINLINE vkResidencyStandard2DBlockShape #-}
        vkResidencyStandard2DBlockShape x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DBlockShape})

        {-# INLINE vkResidencyStandard2DBlockShapeByteOffset #-}
        vkResidencyStandard2DBlockShapeByteOffset ~_
          = #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DBlockShape}

        {-# INLINE readVkResidencyStandard2DBlockShape #-}
        readVkResidencyStandard2DBlockShape p
          = peekByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DBlockShape}

        {-# INLINE writeVkResidencyStandard2DBlockShape #-}
        writeVkResidencyStandard2DBlockShape p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DBlockShape}

instance {-# OVERLAPPING #-}
         HasField "residencyStandard2DBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        type FieldType "residencyStandard2DBlockShape"
               VkPhysicalDeviceSparseProperties
             = VkBool32
        type FieldOptional "residencyStandard2DBlockShape"
               VkPhysicalDeviceSparseProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "residencyStandard2DBlockShape"
               VkPhysicalDeviceSparseProperties
             =
             #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DBlockShape}
        type FieldIsArray "residencyStandard2DBlockShape"
               VkPhysicalDeviceSparseProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DBlockShape}

instance CanReadField "residencyStandard2DBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        {-# INLINE getField #-}
        getField = vkResidencyStandard2DBlockShape

        {-# INLINE readField #-}
        readField = readVkResidencyStandard2DBlockShape

instance {-# OVERLAPPING #-}
         HasVkResidencyStandard2DMultisampleBlockShape
           VkPhysicalDeviceSparseProperties
         where
        type VkResidencyStandard2DMultisampleBlockShapeMType
               VkPhysicalDeviceSparseProperties
             = VkBool32

        {-# NOINLINE vkResidencyStandard2DMultisampleBlockShape #-}
        vkResidencyStandard2DMultisampleBlockShape x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DMultisampleBlockShape})

        {-# INLINE vkResidencyStandard2DMultisampleBlockShapeByteOffset #-}
        vkResidencyStandard2DMultisampleBlockShapeByteOffset ~_
          = #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DMultisampleBlockShape}

        {-# INLINE readVkResidencyStandard2DMultisampleBlockShape #-}
        readVkResidencyStandard2DMultisampleBlockShape p
          = peekByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DMultisampleBlockShape}

        {-# INLINE writeVkResidencyStandard2DMultisampleBlockShape #-}
        writeVkResidencyStandard2DMultisampleBlockShape p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DMultisampleBlockShape}

instance {-# OVERLAPPING #-}
         HasField "residencyStandard2DMultisampleBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        type FieldType "residencyStandard2DMultisampleBlockShape"
               VkPhysicalDeviceSparseProperties
             = VkBool32
        type FieldOptional "residencyStandard2DMultisampleBlockShape"
               VkPhysicalDeviceSparseProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "residencyStandard2DMultisampleBlockShape"
               VkPhysicalDeviceSparseProperties
             =
             #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DMultisampleBlockShape}
        type FieldIsArray "residencyStandard2DMultisampleBlockShape"
               VkPhysicalDeviceSparseProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DMultisampleBlockShape}

instance CanReadField "residencyStandard2DMultisampleBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        {-# INLINE getField #-}
        getField = vkResidencyStandard2DMultisampleBlockShape

        {-# INLINE readField #-}
        readField = readVkResidencyStandard2DMultisampleBlockShape

instance {-# OVERLAPPING #-}
         HasVkResidencyStandard3DBlockShape VkPhysicalDeviceSparseProperties
         where
        type VkResidencyStandard3DBlockShapeMType
               VkPhysicalDeviceSparseProperties
             = VkBool32

        {-# NOINLINE vkResidencyStandard3DBlockShape #-}
        vkResidencyStandard3DBlockShape x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseProperties, residencyStandard3DBlockShape})

        {-# INLINE vkResidencyStandard3DBlockShapeByteOffset #-}
        vkResidencyStandard3DBlockShapeByteOffset ~_
          = #{offset VkPhysicalDeviceSparseProperties, residencyStandard3DBlockShape}

        {-# INLINE readVkResidencyStandard3DBlockShape #-}
        readVkResidencyStandard3DBlockShape p
          = peekByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard3DBlockShape}

        {-# INLINE writeVkResidencyStandard3DBlockShape #-}
        writeVkResidencyStandard3DBlockShape p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard3DBlockShape}

instance {-# OVERLAPPING #-}
         HasField "residencyStandard3DBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        type FieldType "residencyStandard3DBlockShape"
               VkPhysicalDeviceSparseProperties
             = VkBool32
        type FieldOptional "residencyStandard3DBlockShape"
               VkPhysicalDeviceSparseProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "residencyStandard3DBlockShape"
               VkPhysicalDeviceSparseProperties
             =
             #{offset VkPhysicalDeviceSparseProperties, residencyStandard3DBlockShape}
        type FieldIsArray "residencyStandard3DBlockShape"
               VkPhysicalDeviceSparseProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseProperties, residencyStandard3DBlockShape}

instance CanReadField "residencyStandard3DBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        {-# INLINE getField #-}
        getField = vkResidencyStandard3DBlockShape

        {-# INLINE readField #-}
        readField = readVkResidencyStandard3DBlockShape

instance {-# OVERLAPPING #-}
         HasVkResidencyAlignedMipSize VkPhysicalDeviceSparseProperties where
        type VkResidencyAlignedMipSizeMType
               VkPhysicalDeviceSparseProperties
             = VkBool32

        {-# NOINLINE vkResidencyAlignedMipSize #-}
        vkResidencyAlignedMipSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseProperties, residencyAlignedMipSize})

        {-# INLINE vkResidencyAlignedMipSizeByteOffset #-}
        vkResidencyAlignedMipSizeByteOffset ~_
          = #{offset VkPhysicalDeviceSparseProperties, residencyAlignedMipSize}

        {-# INLINE readVkResidencyAlignedMipSize #-}
        readVkResidencyAlignedMipSize p
          = peekByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyAlignedMipSize}

        {-# INLINE writeVkResidencyAlignedMipSize #-}
        writeVkResidencyAlignedMipSize p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyAlignedMipSize}

instance {-# OVERLAPPING #-}
         HasField "residencyAlignedMipSize" VkPhysicalDeviceSparseProperties
         where
        type FieldType "residencyAlignedMipSize"
               VkPhysicalDeviceSparseProperties
             = VkBool32
        type FieldOptional "residencyAlignedMipSize"
               VkPhysicalDeviceSparseProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "residencyAlignedMipSize"
               VkPhysicalDeviceSparseProperties
             =
             #{offset VkPhysicalDeviceSparseProperties, residencyAlignedMipSize}
        type FieldIsArray "residencyAlignedMipSize"
               VkPhysicalDeviceSparseProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseProperties, residencyAlignedMipSize}

instance CanReadField "residencyAlignedMipSize"
           VkPhysicalDeviceSparseProperties
         where
        {-# INLINE getField #-}
        getField = vkResidencyAlignedMipSize

        {-# INLINE readField #-}
        readField = readVkResidencyAlignedMipSize

instance {-# OVERLAPPING #-}
         HasVkResidencyNonResidentStrict VkPhysicalDeviceSparseProperties
         where
        type VkResidencyNonResidentStrictMType
               VkPhysicalDeviceSparseProperties
             = VkBool32

        {-# NOINLINE vkResidencyNonResidentStrict #-}
        vkResidencyNonResidentStrict x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseProperties, residencyNonResidentStrict})

        {-# INLINE vkResidencyNonResidentStrictByteOffset #-}
        vkResidencyNonResidentStrictByteOffset ~_
          = #{offset VkPhysicalDeviceSparseProperties, residencyNonResidentStrict}

        {-# INLINE readVkResidencyNonResidentStrict #-}
        readVkResidencyNonResidentStrict p
          = peekByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyNonResidentStrict}

        {-# INLINE writeVkResidencyNonResidentStrict #-}
        writeVkResidencyNonResidentStrict p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyNonResidentStrict}

instance {-# OVERLAPPING #-}
         HasField "residencyNonResidentStrict"
           VkPhysicalDeviceSparseProperties
         where
        type FieldType "residencyNonResidentStrict"
               VkPhysicalDeviceSparseProperties
             = VkBool32
        type FieldOptional "residencyNonResidentStrict"
               VkPhysicalDeviceSparseProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "residencyNonResidentStrict"
               VkPhysicalDeviceSparseProperties
             =
             #{offset VkPhysicalDeviceSparseProperties, residencyNonResidentStrict}
        type FieldIsArray "residencyNonResidentStrict"
               VkPhysicalDeviceSparseProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSparseProperties, residencyNonResidentStrict}

instance CanReadField "residencyNonResidentStrict"
           VkPhysicalDeviceSparseProperties
         where
        {-# INLINE getField #-}
        getField = vkResidencyNonResidentStrict

        {-# INLINE readField #-}
        readField = readVkResidencyNonResidentStrict

instance Show VkPhysicalDeviceSparseProperties where
        showsPrec d x
          = showString "VkPhysicalDeviceSparseProperties {" .
              showString "vkResidencyStandard2DBlockShape = " .
                showsPrec d (vkResidencyStandard2DBlockShape x) .
                  showString ", " .
                    showString "vkResidencyStandard2DMultisampleBlockShape = " .
                      showsPrec d (vkResidencyStandard2DMultisampleBlockShape x) .
                        showString ", " .
                          showString "vkResidencyStandard3DBlockShape = " .
                            showsPrec d (vkResidencyStandard3DBlockShape x) .
                              showString ", " .
                                showString "vkResidencyAlignedMipSize = " .
                                  showsPrec d (vkResidencyAlignedMipSize x) .
                                    showString ", " .
                                      showString "vkResidencyNonResidentStrict = " .
                                        showsPrec d (vkResidencyNonResidentStrict x) . showChar '}'
