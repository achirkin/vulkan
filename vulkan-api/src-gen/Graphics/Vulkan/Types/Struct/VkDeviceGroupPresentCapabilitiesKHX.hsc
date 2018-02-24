#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentCapabilitiesKHX
       (VkDeviceGroupPresentCapabilitiesKHX(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                                                (KnownNat,
                                                                              natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                                   (VK_MAX_DEVICE_GROUP_SIZE_KHX,
                                                                              pattern VK_MAX_DEVICE_GROUP_SIZE_KHX)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHX (VkDeviceGroupPresentModeFlagsKHX)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupPresentCapabilitiesKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         presentMask[VK_MAX_DEVICE_GROUP_SIZE_KHX];
--   >     VkDeviceGroupPresentModeFlagsKHX modes;
--   > } VkDeviceGroupPresentCapabilitiesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupPresentCapabilitiesKHX.html VkDeviceGroupPresentCapabilitiesKHX registry at www.khronos.org>
data VkDeviceGroupPresentCapabilitiesKHX = VkDeviceGroupPresentCapabilitiesKHX## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupPresentCapabilitiesKHX where
        (VkDeviceGroupPresentCapabilitiesKHX## a _) ==
          x@(VkDeviceGroupPresentCapabilitiesKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentCapabilitiesKHX where
        (VkDeviceGroupPresentCapabilitiesKHX## a _) `compare`
          x@(VkDeviceGroupPresentCapabilitiesKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentCapabilitiesKHX where
        sizeOf ~_ = #{size VkDeviceGroupPresentCapabilitiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupPresentCapabilitiesKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupPresentCapabilitiesKHX
         where
        unsafeAddr (VkDeviceGroupPresentCapabilitiesKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupPresentCapabilitiesKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupPresentCapabilitiesKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupPresentCapabilitiesKHX where
        type StructFields VkDeviceGroupPresentCapabilitiesKHX =
             '["sType", "pNext", "presentMask", "modes"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupPresentCapabilitiesKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupPresentCapabilitiesKHX = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupPresentCapabilitiesKHX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGroupPresentCapabilitiesKHX where
        type VkSTypeMType VkDeviceGroupPresentCapabilitiesKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "sType" VkDeviceGroupPresentCapabilitiesKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupPresentCapabilitiesKHX =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, sType}

instance CanReadField "sType" VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGroupPresentCapabilitiesKHX where
        type VkPNextMType VkDeviceGroupPresentCapabilitiesKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, pNext}

instance CanReadField "pNext" VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkPresentMaskArray VkDeviceGroupPresentCapabilitiesKHX where
        type VkPresentMaskArrayMType VkDeviceGroupPresentCapabilitiesKHX =
             Word32

        {-# NOINLINE vkPresentMaskArray #-}
        vkPresentMaskArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word32) +
                    #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}))

        {-# INLINE vkPresentMaskArrayByteOffset #-}
        vkPresentMaskArrayByteOffset ~_
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}

        {-# INLINE readVkPresentMaskArray #-}
        readVkPresentMaskArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask})

        {-# INLINE writeVkPresentMaskArray #-}
        writeVkPresentMaskArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask})

instance {-# OVERLAPPING #-}
         HasField "presentMask" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "presentMask" VkDeviceGroupPresentCapabilitiesKHX =
             Word32
        type FieldOptional "presentMask"
               VkDeviceGroupPresentCapabilitiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "presentMask" VkDeviceGroupPresentCapabilitiesKHX
             =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}
        type FieldIsArray "presentMask" VkDeviceGroupPresentCapabilitiesKHX
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, presentMask}

instance (KnownNat idx,
          IndexInBounds "presentMask" idx
            VkDeviceGroupPresentCapabilitiesKHX) =>
         CanReadFieldArray "presentMask" idx
           VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 0
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 1
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 2
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 3
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}
        type FieldArrayLength "presentMask"
               VkDeviceGroupPresentCapabilitiesKHX
             = VK_MAX_DEVICE_GROUP_SIZE_KHX

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_DEVICE_GROUP_SIZE_KHX

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkPresentMaskArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkPresentMaskArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "presentMask" idx
            VkDeviceGroupPresentCapabilitiesKHX) =>
         CanWriteFieldArray "presentMask" idx
           VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 0
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 1
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 2
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 3
                         VkDeviceGroupPresentCapabilitiesKHX
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkPresentMaskArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkModes VkDeviceGroupPresentCapabilitiesKHX where
        type VkModesMType VkDeviceGroupPresentCapabilitiesKHX =
             VkDeviceGroupPresentModeFlagsKHX

        {-# NOINLINE vkModes #-}
        vkModes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHX, modes})

        {-# INLINE vkModesByteOffset #-}
        vkModesByteOffset ~_
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

        {-# INLINE readVkModes #-}
        readVkModes p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

        {-# INLINE writeVkModes #-}
        writeVkModes p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

instance {-# OVERLAPPING #-}
         HasField "modes" VkDeviceGroupPresentCapabilitiesKHX where
        type FieldType "modes" VkDeviceGroupPresentCapabilitiesKHX =
             VkDeviceGroupPresentModeFlagsKHX
        type FieldOptional "modes" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "modes" VkDeviceGroupPresentCapabilitiesKHX =
             #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}
        type FieldIsArray "modes" VkDeviceGroupPresentCapabilitiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHX, modes}

instance CanReadField "modes" VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# INLINE getField #-}
        getField = vkModes

        {-# INLINE readField #-}
        readField = readVkModes

instance CanWriteField "modes" VkDeviceGroupPresentCapabilitiesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkModes

instance Show VkDeviceGroupPresentCapabilitiesKHX where
        showsPrec d x
          = showString "VkDeviceGroupPresentCapabilitiesKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPresentMaskArray = [" .
                            showsPrec d
                              (map (vkPresentMaskArray x) [1 .. VK_MAX_DEVICE_GROUP_SIZE_KHX])
                              .
                              showChar ']' .
                                showString ", " .
                                  showString "vkModes = " . showsPrec d (vkModes x) . showChar '}'
