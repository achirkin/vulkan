#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentInfoKHR
       (VkDeviceGroupPresentInfoKHR(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHR (VkDeviceGroupPresentModeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR               (VkPresentInfoKHR)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupPresentInfoKHR {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     uint32_t         swapchainCount;
--   >     const uint32_t* pDeviceMasks;
--   >     VkDeviceGroupPresentModeFlagBitsKHR mode;
--   > } VkDeviceGroupPresentInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDeviceGroupPresentInfoKHR.html VkDeviceGroupPresentInfoKHR registry at www.khronos.org>
data VkDeviceGroupPresentInfoKHR = VkDeviceGroupPresentInfoKHR## Addr##
                                                                ByteArray##

instance Eq VkDeviceGroupPresentInfoKHR where
        (VkDeviceGroupPresentInfoKHR## a _) ==
          x@(VkDeviceGroupPresentInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentInfoKHR where
        (VkDeviceGroupPresentInfoKHR## a _) `compare`
          x@(VkDeviceGroupPresentInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentInfoKHR where
        sizeOf ~_ = #{size VkDeviceGroupPresentInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupPresentInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupPresentInfoKHR where
        unsafeAddr (VkDeviceGroupPresentInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupPresentInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupPresentInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupPresentInfoKHR where
        type StructFields VkDeviceGroupPresentInfoKHR =
             '["sType", "pNext", "swapchainCount", "pDeviceMasks", "mode"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupPresentInfoKHR =
             '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupPresentInfoKHR where
        type FieldType "sType" VkDeviceGroupPresentInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupPresentInfoKHR =
             #{offset VkDeviceGroupPresentInfoKHR, sType}
        type FieldIsArray "sType" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupPresentInfoKHR where
        type FieldType "pNext" VkDeviceGroupPresentInfoKHR = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupPresentInfoKHR =
             #{offset VkDeviceGroupPresentInfoKHR, pNext}
        type FieldIsArray "pNext" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkDeviceGroupPresentInfoKHR where
        type FieldType "swapchainCount" VkDeviceGroupPresentInfoKHR =
             Word32
        type FieldOptional "swapchainCount" VkDeviceGroupPresentInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "swapchainCount" VkDeviceGroupPresentInfoKHR =
             #{offset VkDeviceGroupPresentInfoKHR, swapchainCount}
        type FieldIsArray "swapchainCount" VkDeviceGroupPresentInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         CanReadField "swapchainCount" VkDeviceGroupPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHR, swapchainCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchainCount" VkDeviceGroupPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "pDeviceMasks" VkDeviceGroupPresentInfoKHR where
        type FieldType "pDeviceMasks" VkDeviceGroupPresentInfoKHR =
             Ptr Word32
        type FieldOptional "pDeviceMasks" VkDeviceGroupPresentInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceMasks" VkDeviceGroupPresentInfoKHR =
             #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks}
        type FieldIsArray "pDeviceMasks" VkDeviceGroupPresentInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceMasks" VkDeviceGroupPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceMasks" VkDeviceGroupPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks}

instance {-# OVERLAPPING #-}
         HasField "mode" VkDeviceGroupPresentInfoKHR where
        type FieldType "mode" VkDeviceGroupPresentInfoKHR =
             VkDeviceGroupPresentModeFlagBitsKHR
        type FieldOptional "mode" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mode" VkDeviceGroupPresentInfoKHR =
             #{offset VkDeviceGroupPresentInfoKHR, mode}
        type FieldIsArray "mode" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHR, mode}

instance {-# OVERLAPPING #-}
         CanReadField "mode" VkDeviceGroupPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHR, mode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHR, mode}

instance {-# OVERLAPPING #-}
         CanWriteField "mode" VkDeviceGroupPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHR, mode}

instance Show VkDeviceGroupPresentInfoKHR where
        showsPrec d x
          = showString "VkDeviceGroupPresentInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "swapchainCount = " .
                            showsPrec d (getField @"swapchainCount" x) .
                              showString ", " .
                                showString "pDeviceMasks = " .
                                  showsPrec d (getField @"pDeviceMasks" x) .
                                    showString ", " .
                                      showString "mode = " .
                                        showsPrec d (getField @"mode" x) . showChar '}'
