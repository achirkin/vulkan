#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupPropertiesKHX
       (VkPhysicalDeviceGroupPropertiesKHX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                               (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                  (VK_MAX_DEVICE_GROUP_SIZE_KHX,
                                                             pattern VK_MAX_DEVICE_GROUP_SIZE_KHX)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkPhysicalDevice)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceGroupPropertiesKHX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     VkPhysicalDevice                 physicalDevices[VK_MAX_DEVICE_GROUP_SIZE_KHX];
--   >     VkBool32                         subsetAllocation;
--   > } VkPhysicalDeviceGroupPropertiesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceGroupPropertiesKHX.html VkPhysicalDeviceGroupPropertiesKHX registry at www.khronos.org>
data VkPhysicalDeviceGroupPropertiesKHX = VkPhysicalDeviceGroupPropertiesKHX## Addr##
                                                                              ByteArray##

instance Eq VkPhysicalDeviceGroupPropertiesKHX where
        (VkPhysicalDeviceGroupPropertiesKHX## a _) ==
          x@(VkPhysicalDeviceGroupPropertiesKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceGroupPropertiesKHX where
        (VkPhysicalDeviceGroupPropertiesKHX## a _) `compare`
          x@(VkPhysicalDeviceGroupPropertiesKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceGroupPropertiesKHX where
        sizeOf ~_ = #{size VkPhysicalDeviceGroupPropertiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceGroupPropertiesKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceGroupPropertiesKHX where
        unsafeAddr (VkPhysicalDeviceGroupPropertiesKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceGroupPropertiesKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceGroupPropertiesKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceGroupPropertiesKHX where
        type StructFields VkPhysicalDeviceGroupPropertiesKHX =
             '["sType", "pNext", "physicalDeviceCount", "physicalDevices", -- ' closing tick for hsc2hs
               "subsetAllocation"]
        type CUnionType VkPhysicalDeviceGroupPropertiesKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceGroupPropertiesKHX = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceGroupPropertiesKHX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceGroupPropertiesKHX where
        type FieldType "sType" VkPhysicalDeviceGroupPropertiesKHX =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceGroupPropertiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceGroupPropertiesKHX =
             #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}
        type FieldIsArray "sType" VkPhysicalDeviceGroupPropertiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceGroupPropertiesKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceGroupPropertiesKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceGroupPropertiesKHX where
        type FieldType "pNext" VkPhysicalDeviceGroupPropertiesKHX =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceGroupPropertiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceGroupPropertiesKHX =
             #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceGroupPropertiesKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceGroupPropertiesKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceGroupPropertiesKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "physicalDeviceCount" VkPhysicalDeviceGroupPropertiesKHX
         where
        type FieldType "physicalDeviceCount"
               VkPhysicalDeviceGroupPropertiesKHX
             = Word32
        type FieldOptional "physicalDeviceCount"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "physicalDeviceCount"
               VkPhysicalDeviceGroupPropertiesKHX
             =
             #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}
        type FieldIsArray "physicalDeviceCount"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         CanReadField "physicalDeviceCount"
           VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         CanWriteField "physicalDeviceCount"
           VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         HasField "physicalDevices" VkPhysicalDeviceGroupPropertiesKHX where
        type FieldType "physicalDevices" VkPhysicalDeviceGroupPropertiesKHX
             = VkPhysicalDevice
        type FieldOptional "physicalDevices"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "physicalDevices"
               VkPhysicalDeviceGroupPropertiesKHX
             =
             #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}
        type FieldIsArray "physicalDevices"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "physicalDevices" idx
            VkPhysicalDeviceGroupPropertiesKHX) =>
         CanReadFieldArray "physicalDevices" idx
           VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 0
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 1
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 2
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 3
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}
        type FieldArrayLength "physicalDevices"
               VkPhysicalDeviceGroupPropertiesKHX
             = VK_MAX_DEVICE_GROUP_SIZE_KHX

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_DEVICE_GROUP_SIZE_KHX

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}
                      +
                      sizeOf (undefined :: VkPhysicalDevice) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}
                 +
                 sizeOf (undefined :: VkPhysicalDevice) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "physicalDevices" idx
            VkPhysicalDeviceGroupPropertiesKHX) =>
         CanWriteFieldArray "physicalDevices" idx
           VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "physicalDevices" 0
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "physicalDevices" 1
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "physicalDevices" 2
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "physicalDevices" 3
                         VkPhysicalDeviceGroupPropertiesKHX
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceGroupPropertiesKHX, physicalDevices}
                 +
                 sizeOf (undefined :: VkPhysicalDevice) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "subsetAllocation" VkPhysicalDeviceGroupPropertiesKHX
         where
        type FieldType "subsetAllocation"
               VkPhysicalDeviceGroupPropertiesKHX
             = VkBool32
        type FieldOptional "subsetAllocation"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subsetAllocation"
               VkPhysicalDeviceGroupPropertiesKHX
             =
             #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}
        type FieldIsArray "subsetAllocation"
               VkPhysicalDeviceGroupPropertiesKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "subsetAllocation" VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "subsetAllocation" VkPhysicalDeviceGroupPropertiesKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupPropertiesKHX, subsetAllocation}

instance Show VkPhysicalDeviceGroupPropertiesKHX where
        showsPrec d x
          = showString "VkPhysicalDeviceGroupPropertiesKHX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "physicalDeviceCount = " .
                            showsPrec d (getField @"physicalDeviceCount" x) .
                              showString ", " .
                                (showString "physicalDevices = [" .
                                   showsPrec d
                                     (let s = sizeOf
                                                (undefined ::
                                                   FieldType "physicalDevices"
                                                     VkPhysicalDeviceGroupPropertiesKHX)
                                          o = fieldOffset @"physicalDevices"
                                                @VkPhysicalDeviceGroupPropertiesKHX
                                          f i
                                            = peekByteOff (unsafePtr x) i ::
                                                IO
                                                  (FieldType "physicalDevices"
                                                     VkPhysicalDeviceGroupPropertiesKHX)
                                        in
                                        unsafeDupablePerformIO . mapM f $
                                          map (\ i -> o + i * s)
                                            [0 .. VK_MAX_DEVICE_GROUP_SIZE_KHX - 1])
                                     . showChar ']')
                                  .
                                  showString ", " .
                                    showString "subsetAllocation = " .
                                      showsPrec d (getField @"subsetAllocation" x) . showChar '}'
