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
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceGroupProperties
       (VkPhysicalDeviceGroupProperties(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                               (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                  (VK_MAX_DEVICE_GROUP_SIZE,
                                                             pattern VK_MAX_DEVICE_GROUP_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkPhysicalDevice)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceGroupProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     VkPhysicalDevice                 physicalDevices[VK_MAX_DEVICE_GROUP_SIZE];
--   >     VkBool32                         subsetAllocation;
--   > } VkPhysicalDeviceGroupProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceGroupProperties.html VkPhysicalDeviceGroupProperties registry at www.khronos.org>
data VkPhysicalDeviceGroupProperties = VkPhysicalDeviceGroupProperties## Addr##
                                                                        ByteArray##

instance Eq VkPhysicalDeviceGroupProperties where
        (VkPhysicalDeviceGroupProperties## a _) ==
          x@(VkPhysicalDeviceGroupProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceGroupProperties where
        (VkPhysicalDeviceGroupProperties## a _) `compare`
          x@(VkPhysicalDeviceGroupProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceGroupProperties where
        sizeOf ~_ = #{size VkPhysicalDeviceGroupProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceGroupProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceGroupProperties where
        unsafeAddr (VkPhysicalDeviceGroupProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceGroupProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceGroupProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceGroupProperties where
        type StructFields VkPhysicalDeviceGroupProperties =
             '["sType", "pNext", "physicalDeviceCount", "physicalDevices", -- ' closing tick for hsc2hs
               "subsetAllocation"]
        type CUnionType VkPhysicalDeviceGroupProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceGroupProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceGroupProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceGroupProperties where
        type FieldType "sType" VkPhysicalDeviceGroupProperties =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceGroupProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceGroupProperties =
             #{offset VkPhysicalDeviceGroupProperties, sType}
        type FieldIsArray "sType" VkPhysicalDeviceGroupProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceGroupProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceGroupProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceGroupProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceGroupProperties where
        type FieldType "pNext" VkPhysicalDeviceGroupProperties = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceGroupProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceGroupProperties =
             #{offset VkPhysicalDeviceGroupProperties, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceGroupProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceGroupProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceGroupProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceGroupProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "physicalDeviceCount" VkPhysicalDeviceGroupProperties
         where
        type FieldType "physicalDeviceCount"
               VkPhysicalDeviceGroupProperties
             = Word32
        type FieldOptional "physicalDeviceCount"
               VkPhysicalDeviceGroupProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "physicalDeviceCount"
               VkPhysicalDeviceGroupProperties
             =
             #{offset VkPhysicalDeviceGroupProperties, physicalDeviceCount}
        type FieldIsArray "physicalDeviceCount"
               VkPhysicalDeviceGroupProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupProperties, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         CanReadField "physicalDeviceCount" VkPhysicalDeviceGroupProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupProperties, physicalDeviceCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceGroupProperties, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         CanWriteField "physicalDeviceCount" VkPhysicalDeviceGroupProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupProperties, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         HasField "physicalDevices" VkPhysicalDeviceGroupProperties where
        type FieldType "physicalDevices" VkPhysicalDeviceGroupProperties =
             VkPhysicalDevice
        type FieldOptional "physicalDevices"
               VkPhysicalDeviceGroupProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "physicalDevices" VkPhysicalDeviceGroupProperties
             =
             #{offset VkPhysicalDeviceGroupProperties, physicalDevices}
        type FieldIsArray "physicalDevices" VkPhysicalDeviceGroupProperties
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupProperties, physicalDevices}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "physicalDevices" idx
            VkPhysicalDeviceGroupProperties) =>
         CanReadFieldArray "physicalDevices" idx
           VkPhysicalDeviceGroupProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 0
                         VkPhysicalDeviceGroupProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 1
                         VkPhysicalDeviceGroupProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 2
                         VkPhysicalDeviceGroupProperties
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "physicalDevices" 3
                         VkPhysicalDeviceGroupProperties
                       #-}
        type FieldArrayLength "physicalDevices"
               VkPhysicalDeviceGroupProperties
             = VK_MAX_DEVICE_GROUP_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_DEVICE_GROUP_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPhysicalDeviceGroupProperties, physicalDevices}
                      +
                      sizeOf (undefined :: VkPhysicalDevice) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPhysicalDeviceGroupProperties, physicalDevices}
                 +
                 sizeOf (undefined :: VkPhysicalDevice) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "physicalDevices" idx
            VkPhysicalDeviceGroupProperties) =>
         CanWriteFieldArray "physicalDevices" idx
           VkPhysicalDeviceGroupProperties
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "physicalDevices" 0
                         VkPhysicalDeviceGroupProperties
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "physicalDevices" 1
                         VkPhysicalDeviceGroupProperties
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "physicalDevices" 2
                         VkPhysicalDeviceGroupProperties
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "physicalDevices" 3
                         VkPhysicalDeviceGroupProperties
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPhysicalDeviceGroupProperties, physicalDevices}
                 +
                 sizeOf (undefined :: VkPhysicalDevice) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "subsetAllocation" VkPhysicalDeviceGroupProperties where
        type FieldType "subsetAllocation" VkPhysicalDeviceGroupProperties =
             VkBool32
        type FieldOptional "subsetAllocation"
               VkPhysicalDeviceGroupProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subsetAllocation" VkPhysicalDeviceGroupProperties
             =
             #{offset VkPhysicalDeviceGroupProperties, subsetAllocation}
        type FieldIsArray "subsetAllocation"
               VkPhysicalDeviceGroupProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceGroupProperties, subsetAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "subsetAllocation" VkPhysicalDeviceGroupProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceGroupProperties, subsetAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceGroupProperties, subsetAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "subsetAllocation" VkPhysicalDeviceGroupProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceGroupProperties, subsetAllocation}

instance Show VkPhysicalDeviceGroupProperties where
        showsPrec d x
          = showString "VkPhysicalDeviceGroupProperties {" .
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
                                                     VkPhysicalDeviceGroupProperties)
                                          o = fieldOffset @"physicalDevices"
                                                @VkPhysicalDeviceGroupProperties
                                          f i
                                            = peekByteOff (unsafePtr x) i ::
                                                IO
                                                  (FieldType "physicalDevices"
                                                     VkPhysicalDeviceGroupProperties)
                                        in
                                        unsafeDupablePerformIO . mapM f $
                                          map (\ i -> o + i * s)
                                            [0 .. VK_MAX_DEVICE_GROUP_SIZE - 1])
                                     . showChar ']')
                                  .
                                  showString ", " .
                                    showString "subsetAllocation = " .
                                      showsPrec d (getField @"subsetAllocation" x) . showChar '}'
