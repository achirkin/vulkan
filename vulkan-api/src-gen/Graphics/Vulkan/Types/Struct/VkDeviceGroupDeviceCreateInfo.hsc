#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupDeviceCreateInfo
       (VkDeviceGroupDeviceCreateInfo(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Base                                        (Addr##,
                                                                  ByteArray##,
                                                                  byteArrayContents##,
                                                                  plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                   (VkPhysicalDevice)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo (VkDeviceCreateInfo)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupDeviceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     const VkPhysicalDevice*  pPhysicalDevices;
--   > } VkDeviceGroupDeviceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupDeviceCreateInfo VkDeviceGroupDeviceCreateInfo registry at www.khronos.org>
data VkDeviceGroupDeviceCreateInfo = VkDeviceGroupDeviceCreateInfo## Addr##
                                                                    ByteArray##

instance Eq VkDeviceGroupDeviceCreateInfo where
        (VkDeviceGroupDeviceCreateInfo## a _) ==
          x@(VkDeviceGroupDeviceCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupDeviceCreateInfo where
        (VkDeviceGroupDeviceCreateInfo## a _) `compare`
          x@(VkDeviceGroupDeviceCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupDeviceCreateInfo where
        sizeOf ~_ = #{size VkDeviceGroupDeviceCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupDeviceCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupDeviceCreateInfo where
        unsafeAddr (VkDeviceGroupDeviceCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupDeviceCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupDeviceCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupDeviceCreateInfo where
        type StructFields VkDeviceGroupDeviceCreateInfo =
             '["sType", "pNext", "physicalDeviceCount", "pPhysicalDevices"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupDeviceCreateInfo =
             '[VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupDeviceCreateInfo where
        type FieldType "sType" VkDeviceGroupDeviceCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupDeviceCreateInfo =
             #{offset VkDeviceGroupDeviceCreateInfo, sType}
        type FieldIsArray "sType" VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupDeviceCreateInfo where
        type FieldType "pNext" VkDeviceGroupDeviceCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupDeviceCreateInfo =
             #{offset VkDeviceGroupDeviceCreateInfo, pNext}
        type FieldIsArray "pNext" VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "physicalDeviceCount" VkDeviceGroupDeviceCreateInfo where
        type FieldType "physicalDeviceCount" VkDeviceGroupDeviceCreateInfo
             = Word32
        type FieldOptional "physicalDeviceCount"
               VkDeviceGroupDeviceCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "physicalDeviceCount"
               VkDeviceGroupDeviceCreateInfo
             =
             #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount}
        type FieldIsArray "physicalDeviceCount"
               VkDeviceGroupDeviceCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         CanReadField "physicalDeviceCount" VkDeviceGroupDeviceCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         CanWriteField "physicalDeviceCount" VkDeviceGroupDeviceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         HasField "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo where
        type FieldType "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo =
             Ptr VkPhysicalDevice
        type FieldOptional "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo =
             #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices}
        type FieldIsArray "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices}

instance {-# OVERLAPPING #-}
         CanReadField "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices}

instance {-# OVERLAPPING #-}
         CanWriteField "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices}

instance Show VkDeviceGroupDeviceCreateInfo where
        showsPrec d x
          = showString "VkDeviceGroupDeviceCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "physicalDeviceCount = " .
                            showsPrec d (getField @"physicalDeviceCount" x) .
                              showString ", " .
                                showString "pPhysicalDevices = " .
                                  showsPrec d (getField @"pPhysicalDevices" x) . showChar '}'
