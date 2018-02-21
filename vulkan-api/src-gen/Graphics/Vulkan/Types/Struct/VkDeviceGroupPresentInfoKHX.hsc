#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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

instance {-# OVERLAPPING #-} HasVkSType VkDeviceGroupPresentInfoKHX
         where
        type VkSTypeMType VkDeviceGroupPresentInfoKHX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGroupPresentInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, sType}

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

instance CanReadField "sType" VkDeviceGroupPresentInfoKHX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDeviceGroupPresentInfoKHX
         where
        type VkPNextMType VkDeviceGroupPresentInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGroupPresentInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, pNext}

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

instance CanReadField "pNext" VkDeviceGroupPresentInfoKHX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSwapchainCount VkDeviceGroupPresentInfoKHX where
        type VkSwapchainCountMType VkDeviceGroupPresentInfoKHX = Word32

        {-# NOINLINE vkSwapchainCount #-}
        vkSwapchainCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, swapchainCount})

        {-# INLINE vkSwapchainCountByteOffset #-}
        vkSwapchainCountByteOffset ~_
          = #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}

        {-# INLINE readVkSwapchainCount #-}
        readVkSwapchainCount p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}

        {-# INLINE writeVkSwapchainCount #-}
        writeVkSwapchainCount p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, swapchainCount}

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

instance CanReadField "swapchainCount" VkDeviceGroupPresentInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSwapchainCount

        {-# INLINE readField #-}
        readField = readVkSwapchainCount

instance CanWriteField "swapchainCount" VkDeviceGroupPresentInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSwapchainCount

instance {-# OVERLAPPING #-}
         HasVkPDeviceMasks VkDeviceGroupPresentInfoKHX where
        type VkPDeviceMasksMType VkDeviceGroupPresentInfoKHX = Ptr Word32

        {-# NOINLINE vkPDeviceMasks #-}
        vkPDeviceMasks x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks})

        {-# INLINE vkPDeviceMasksByteOffset #-}
        vkPDeviceMasksByteOffset ~_
          = #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}

        {-# INLINE readVkPDeviceMasks #-}
        readVkPDeviceMasks p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}

        {-# INLINE writeVkPDeviceMasks #-}
        writeVkPDeviceMasks p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, pDeviceMasks}

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

instance CanReadField "pDeviceMasks" VkDeviceGroupPresentInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPDeviceMasks

        {-# INLINE readField #-}
        readField = readVkPDeviceMasks

instance CanWriteField "pDeviceMasks" VkDeviceGroupPresentInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDeviceMasks

instance {-# OVERLAPPING #-} HasVkMode VkDeviceGroupPresentInfoKHX
         where
        type VkModeMType VkDeviceGroupPresentInfoKHX =
             VkDeviceGroupPresentModeFlagBitsKHX

        {-# NOINLINE vkMode #-}
        vkMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHX, mode})

        {-# INLINE vkModeByteOffset #-}
        vkModeByteOffset ~_
          = #{offset VkDeviceGroupPresentInfoKHX, mode}

        {-# INLINE readVkMode #-}
        readVkMode p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHX, mode}

        {-# INLINE writeVkMode #-}
        writeVkMode p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHX, mode}

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

instance CanReadField "mode" VkDeviceGroupPresentInfoKHX where
        {-# INLINE getField #-}
        getField = vkMode

        {-# INLINE readField #-}
        readField = readVkMode

instance CanWriteField "mode" VkDeviceGroupPresentInfoKHX where
        {-# INLINE writeField #-}
        writeField = writeVkMode

instance Show VkDeviceGroupPresentInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupPresentInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSwapchainCount = " .
                            showsPrec d (vkSwapchainCount x) .
                              showString ", " .
                                showString "vkPDeviceMasks = " .
                                  showsPrec d (vkPDeviceMasks x) .
                                    showString ", " .
                                      showString "vkMode = " . showsPrec d (vkMode x) . showChar '}'
