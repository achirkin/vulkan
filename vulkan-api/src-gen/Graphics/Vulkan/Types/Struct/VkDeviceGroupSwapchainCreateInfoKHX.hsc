#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupSwapchainCreateInfoKHX
       (VkDeviceGroupSwapchainCreateInfoKHX(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDeviceGroupPresentModeFlagsKHX (VkDeviceGroupPresentModeFlagsKHX)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSwapchainCreateInfoKHR       (VkSwapchainCreateInfoKHR)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupSwapchainCreateInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceGroupPresentModeFlagsKHX                         modes;
--   > } VkDeviceGroupSwapchainCreateInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupSwapchainCreateInfoKHX.html VkDeviceGroupSwapchainCreateInfoKHX registry at www.khronos.org>
data VkDeviceGroupSwapchainCreateInfoKHX = VkDeviceGroupSwapchainCreateInfoKHX## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupSwapchainCreateInfoKHX where
        (VkDeviceGroupSwapchainCreateInfoKHX## a _) ==
          x@(VkDeviceGroupSwapchainCreateInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupSwapchainCreateInfoKHX where
        (VkDeviceGroupSwapchainCreateInfoKHX## a _) `compare`
          x@(VkDeviceGroupSwapchainCreateInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupSwapchainCreateInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupSwapchainCreateInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupSwapchainCreateInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupSwapchainCreateInfoKHX
         where
        unsafeAddr (VkDeviceGroupSwapchainCreateInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupSwapchainCreateInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupSwapchainCreateInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupSwapchainCreateInfoKHX where
        type StructFields VkDeviceGroupSwapchainCreateInfoKHX =
             '["sType", "pNext", "modes"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupSwapchainCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupSwapchainCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupSwapchainCreateInfoKHX =
             '[VkSwapchainCreateInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupSwapchainCreateInfoKHX where
        type FieldType "sType" VkDeviceGroupSwapchainCreateInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupSwapchainCreateInfoKHX =
             #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupSwapchainCreateInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupSwapchainCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupSwapchainCreateInfoKHX where
        type FieldType "pNext" VkDeviceGroupSwapchainCreateInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupSwapchainCreateInfoKHX =
             #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupSwapchainCreateInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupSwapchainCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "modes" VkDeviceGroupSwapchainCreateInfoKHX where
        type FieldType "modes" VkDeviceGroupSwapchainCreateInfoKHX =
             VkDeviceGroupPresentModeFlagsKHX
        type FieldOptional "modes" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "modes" VkDeviceGroupSwapchainCreateInfoKHX =
             #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes}
        type FieldIsArray "modes" VkDeviceGroupSwapchainCreateInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes}

instance {-# OVERLAPPING #-}
         CanReadField "modes" VkDeviceGroupSwapchainCreateInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes}

instance {-# OVERLAPPING #-}
         CanWriteField "modes" VkDeviceGroupSwapchainCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHX, modes}

instance Show VkDeviceGroupSwapchainCreateInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupSwapchainCreateInfoKHX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "modes = " .
                            showsPrec d (getField @"modes" x) . showChar '}'
