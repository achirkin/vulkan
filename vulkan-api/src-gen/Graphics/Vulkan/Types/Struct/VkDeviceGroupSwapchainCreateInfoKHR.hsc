#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupSwapchainCreateInfoKHR
       (VkDeviceGroupSwapchainCreateInfoKHR(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Base                                                    (Addr##,
                                                                              ByteArray##,
                                                                              byteArrayContents##,
                                                                              plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHR (VkDeviceGroupPresentModeFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR       (VkSwapchainCreateInfoKHR)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupSwapchainCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceGroupPresentModeFlagsKHR                         modes;
--   > } VkDeviceGroupSwapchainCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDeviceGroupSwapchainCreateInfoKHRVkDeviceGroupSwapchainCreateInfoKHR registry at www.khronos.org>
data VkDeviceGroupSwapchainCreateInfoKHR = VkDeviceGroupSwapchainCreateInfoKHR## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupSwapchainCreateInfoKHR where
        (VkDeviceGroupSwapchainCreateInfoKHR## a _) ==
          x@(VkDeviceGroupSwapchainCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupSwapchainCreateInfoKHR where
        (VkDeviceGroupSwapchainCreateInfoKHR## a _) `compare`
          x@(VkDeviceGroupSwapchainCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupSwapchainCreateInfoKHR where
        sizeOf ~_ = #{size VkDeviceGroupSwapchainCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupSwapchainCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupSwapchainCreateInfoKHR
         where
        unsafeAddr (VkDeviceGroupSwapchainCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupSwapchainCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupSwapchainCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupSwapchainCreateInfoKHR where
        type StructFields VkDeviceGroupSwapchainCreateInfoKHR =
             '["sType", "pNext", "modes"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupSwapchainCreateInfoKHR =
             '[VkSwapchainCreateInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupSwapchainCreateInfoKHR where
        type FieldType "sType" VkDeviceGroupSwapchainCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupSwapchainCreateInfoKHR =
             #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType}
        type FieldIsArray "sType" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupSwapchainCreateInfoKHR where
        type FieldType "pNext" VkDeviceGroupSwapchainCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupSwapchainCreateInfoKHR =
             #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "modes" VkDeviceGroupSwapchainCreateInfoKHR where
        type FieldType "modes" VkDeviceGroupSwapchainCreateInfoKHR =
             VkDeviceGroupPresentModeFlagsKHR
        type FieldOptional "modes" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "modes" VkDeviceGroupSwapchainCreateInfoKHR =
             #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes}
        type FieldIsArray "modes" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes}

instance {-# OVERLAPPING #-}
         CanReadField "modes" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes}

instance {-# OVERLAPPING #-}
         CanWriteField "modes" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes}

instance Show VkDeviceGroupSwapchainCreateInfoKHR where
        showsPrec d x
          = showString "VkDeviceGroupSwapchainCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "modes = " .
                            showsPrec d (getField @"modes" x) . showChar '}'
