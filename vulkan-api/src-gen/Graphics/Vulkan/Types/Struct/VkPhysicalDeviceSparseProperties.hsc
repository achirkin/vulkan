#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSparseProperties
       (VkPhysicalDeviceSparseProperties(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes  (VkBool32)
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSparseProperties {
--   >     VkBool32               residencyStandard2DBlockShape;
--   >     VkBool32               residencyStandard2DMultisampleBlockShape;
--   >     VkBool32               residencyStandard3DBlockShape;
--   >     VkBool32               residencyAlignedMipSize;
--   >     VkBool32               residencyNonResidentStrict;
--   > } VkPhysicalDeviceSparseProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceSparseProperties.html VkPhysicalDeviceSparseProperties registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "residencyStandard2DBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DBlockShape})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DBlockShape}

instance {-# OVERLAPPING #-}
         CanWriteField "residencyStandard2DBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DBlockShape}

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

instance {-# OVERLAPPING #-}
         CanReadField "residencyStandard2DMultisampleBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DMultisampleBlockShape})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DMultisampleBlockShape}

instance {-# OVERLAPPING #-}
         CanWriteField "residencyStandard2DMultisampleBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard2DMultisampleBlockShape}

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

instance {-# OVERLAPPING #-}
         CanReadField "residencyStandard3DBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseProperties, residencyStandard3DBlockShape})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard3DBlockShape}

instance {-# OVERLAPPING #-}
         CanWriteField "residencyStandard3DBlockShape"
           VkPhysicalDeviceSparseProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyStandard3DBlockShape}

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

instance {-# OVERLAPPING #-}
         CanReadField "residencyAlignedMipSize"
           VkPhysicalDeviceSparseProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseProperties, residencyAlignedMipSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyAlignedMipSize}

instance {-# OVERLAPPING #-}
         CanWriteField "residencyAlignedMipSize"
           VkPhysicalDeviceSparseProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyAlignedMipSize}

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

instance {-# OVERLAPPING #-}
         CanReadField "residencyNonResidentStrict"
           VkPhysicalDeviceSparseProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseProperties, residencyNonResidentStrict})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyNonResidentStrict}

instance {-# OVERLAPPING #-}
         CanWriteField "residencyNonResidentStrict"
           VkPhysicalDeviceSparseProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseProperties, residencyNonResidentStrict}

instance Show VkPhysicalDeviceSparseProperties where
        showsPrec d x
          = showString "VkPhysicalDeviceSparseProperties {" .
              showString "residencyStandard2DBlockShape = " .
                showsPrec d (getField @"residencyStandard2DBlockShape" x) .
                  showString ", " .
                    showString "residencyStandard2DMultisampleBlockShape = " .
                      showsPrec d
                        (getField @"residencyStandard2DMultisampleBlockShape" x)
                        .
                        showString ", " .
                          showString "residencyStandard3DBlockShape = " .
                            showsPrec d (getField @"residencyStandard3DBlockShape" x) .
                              showString ", " .
                                showString "residencyAlignedMipSize = " .
                                  showsPrec d (getField @"residencyAlignedMipSize" x) .
                                    showString ", " .
                                      showString "residencyNonResidentStrict = " .
                                        showsPrec d (getField @"residencyNonResidentStrict" x) .
                                          showChar '}'
