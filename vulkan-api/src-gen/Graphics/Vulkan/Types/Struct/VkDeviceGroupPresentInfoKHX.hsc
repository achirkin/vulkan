#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupPresentInfoKHX
       (VkDeviceGroupPresentInfoKHX(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHX (VkDeviceGroupPresentModeFlagBitsKHX)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPresentInfoKHR               (VkPresentInfoKHR)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupPresentInfoKHX {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     uint32_t         swapchainCount;
--   >     const uint32_t* pDeviceMasks;
--   >     VkDeviceGroupPresentModeFlagBitsKHX mode;
--   > } VkDeviceGroupPresentInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupPresentInfoKHX.html VkDeviceGroupPresentInfoKHX registry at www.khronos.org>
data VkDeviceGroupPresentInfoKHX = VkDeviceGroupPresentInfoKHX## Addr##
                                                                ByteArray##

instance Eq VkDeviceGroupPresentInfoKHX where
        (VkDeviceGroupPresentInfoKHX## a _) ==
          x@(VkDeviceGroupPresentInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentInfoKHX where
        (VkDeviceGroupPresentInfoKHX## a _) `compare`
          x@(VkDeviceGroupPresentInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupPresentInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupPresentInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupPresentInfoKHX where
        unsafeAddr (VkDeviceGroupPresentInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupPresentInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupPresentInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupPresentInfoKHX where
        type StructFields VkDeviceGroupPresentInfoKHX =
             '["sType", "pNext", "swapchainCount", "pDeviceMasks", "mode"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupPresentInfoKHX =
             '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupPresentInfoKHX where
        type FieldType "sType" VkDeviceGroupPresentInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupPresentInfoKHX =
             #{offset VkDeviceGroupPresentInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupPresentInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupPresentInfoKHX where
        type FieldType "pNext" VkDeviceGroupPresentInfoKHX = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupPresentInfoKHX =
             #{offset VkDeviceGroupPresentInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupPresentInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkDeviceGroupPresentInfoKHX where
        type FieldType "swapchainCount" VkDeviceGroupPresentInfoKHX =
             Word32
        type FieldOptional "swapchainCount" VkDeviceGroupPresentInfoKHX =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "swapchainCount" VkDeviceGroupPresentInfoKHX =
             #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}
        type FieldIsArray "swapchainCount" VkDeviceGroupPresentInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}

instance {-# OVERLAPPING #-}
         CanReadField "swapchainCount" VkDeviceGroupPresentInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, swapchainCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchainCount" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "pDeviceMasks" VkDeviceGroupPresentInfoKHX where
        type FieldType "pDeviceMasks" VkDeviceGroupPresentInfoKHX =
             Ptr Word32
        type FieldOptional "pDeviceMasks" VkDeviceGroupPresentInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceMasks" VkDeviceGroupPresentInfoKHX =
             #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}
        type FieldIsArray "pDeviceMasks" VkDeviceGroupPresentInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceMasks" VkDeviceGroupPresentInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceMasks" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}

instance {-# OVERLAPPING #-}
         HasField "mode" VkDeviceGroupPresentInfoKHX where
        type FieldType "mode" VkDeviceGroupPresentInfoKHX =
             VkDeviceGroupPresentModeFlagBitsKHX
        type FieldOptional "mode" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mode" VkDeviceGroupPresentInfoKHX =
             #{offset VkDeviceGroupPresentInfoKHX, mode}
        type FieldIsArray "mode" VkDeviceGroupPresentInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHX, mode}

instance {-# OVERLAPPING #-}
         CanReadField "mode" VkDeviceGroupPresentInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, mode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, mode}

instance {-# OVERLAPPING #-}
         CanWriteField "mode" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, mode}

instance Show VkDeviceGroupPresentInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupPresentInfoKHX {" .
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
