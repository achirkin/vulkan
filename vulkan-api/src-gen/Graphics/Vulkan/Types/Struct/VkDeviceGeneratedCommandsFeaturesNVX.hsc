#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGeneratedCommandsFeaturesNVX
       (VkDeviceGeneratedCommandsFeaturesNVX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGeneratedCommandsFeaturesNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         computeBindingPointSupport;
--   > } VkDeviceGeneratedCommandsFeaturesNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGeneratedCommandsFeaturesNVX.html VkDeviceGeneratedCommandsFeaturesNVX registry at www.khronos.org>
data VkDeviceGeneratedCommandsFeaturesNVX = VkDeviceGeneratedCommandsFeaturesNVX## Addr##
                                                                                  ByteArray##

instance Eq VkDeviceGeneratedCommandsFeaturesNVX where
        (VkDeviceGeneratedCommandsFeaturesNVX## a _) ==
          x@(VkDeviceGeneratedCommandsFeaturesNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGeneratedCommandsFeaturesNVX where
        (VkDeviceGeneratedCommandsFeaturesNVX## a _) `compare`
          x@(VkDeviceGeneratedCommandsFeaturesNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGeneratedCommandsFeaturesNVX where
        sizeOf ~_
          = #{size VkDeviceGeneratedCommandsFeaturesNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGeneratedCommandsFeaturesNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGeneratedCommandsFeaturesNVX
         where
        unsafeAddr (VkDeviceGeneratedCommandsFeaturesNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGeneratedCommandsFeaturesNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGeneratedCommandsFeaturesNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGeneratedCommandsFeaturesNVX where
        type StructFields VkDeviceGeneratedCommandsFeaturesNVX =
             '["sType", "pNext", "computeBindingPointSupport"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGeneratedCommandsFeaturesNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGeneratedCommandsFeaturesNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGeneratedCommandsFeaturesNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGeneratedCommandsFeaturesNVX where
        type VkSTypeMType VkDeviceGeneratedCommandsFeaturesNVX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGeneratedCommandsFeaturesNVX where
        type FieldType "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}
        type FieldIsArray "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

instance CanReadField "sType" VkDeviceGeneratedCommandsFeaturesNVX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGeneratedCommandsFeaturesNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGeneratedCommandsFeaturesNVX where
        type VkPNextMType VkDeviceGeneratedCommandsFeaturesNVX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGeneratedCommandsFeaturesNVX where
        type FieldType "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}
        type FieldIsArray "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

instance CanReadField "pNext" VkDeviceGeneratedCommandsFeaturesNVX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGeneratedCommandsFeaturesNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkComputeBindingPointSupport
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        type VkComputeBindingPointSupportMType
               VkDeviceGeneratedCommandsFeaturesNVX
             = VkBool32

        {-# NOINLINE vkComputeBindingPointSupport #-}
        vkComputeBindingPointSupport x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport})

        {-# INLINE vkComputeBindingPointSupportByteOffset #-}
        vkComputeBindingPointSupportByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

        {-# INLINE readVkComputeBindingPointSupport #-}
        readVkComputeBindingPointSupport p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

        {-# INLINE writeVkComputeBindingPointSupport #-}
        writeVkComputeBindingPointSupport p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

instance {-# OVERLAPPING #-}
         HasField "computeBindingPointSupport"
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        type FieldType "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             = VkBool32
        type FieldOptional "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             =
             #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}
        type FieldIsArray "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

instance CanReadField "computeBindingPointSupport"
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        {-# INLINE getField #-}
        getField = vkComputeBindingPointSupport

        {-# INLINE readField #-}
        readField = readVkComputeBindingPointSupport

instance CanWriteField "computeBindingPointSupport"
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkComputeBindingPointSupport

instance Show VkDeviceGeneratedCommandsFeaturesNVX where
        showsPrec d x
          = showString "VkDeviceGeneratedCommandsFeaturesNVX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkComputeBindingPointSupport = " .
                            showsPrec d (vkComputeBindingPointSupport x) . showChar '}'
