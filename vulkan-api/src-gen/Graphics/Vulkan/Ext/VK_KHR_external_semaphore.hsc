#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_KHR_external_semaphore
       (-- * Vulkan extension: @VK_KHR_external_semaphore@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @78@
        --
        -- Required extensions: 'VK_KHR_external_semaphore_capabilities'.
        --

        -- ** Required extensions: 'VK_KHR_external_semaphore_capabilities'.
        VkExportSemaphoreCreateInfoKHR(..),
        VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION,
        VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME,
        pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR)
       where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkSemaphoreCreateInfo)
import           Graphics.Vulkan.Common           (VkExternalSemaphoreHandleTypeFlagsKHR,
                                                   VkStructureType,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExportSemaphoreCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalSemaphoreHandleTypeFlagsKHR handleTypes;
--   > } VkExportSemaphoreCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExportSemaphoreCreateInfoKHR.html VkExportSemaphoreCreateInfoKHR registry at www.khronos.org>
data VkExportSemaphoreCreateInfoKHR = VkExportSemaphoreCreateInfoKHR## Addr##
                                                                      ByteArray##

instance Eq VkExportSemaphoreCreateInfoKHR where
        (VkExportSemaphoreCreateInfoKHR## a _) ==
          x@(VkExportSemaphoreCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportSemaphoreCreateInfoKHR where
        (VkExportSemaphoreCreateInfoKHR## a _) `compare`
          x@(VkExportSemaphoreCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportSemaphoreCreateInfoKHR where
        sizeOf ~_ = #{size VkExportSemaphoreCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportSemaphoreCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportSemaphoreCreateInfoKHR where
        unsafeAddr (VkExportSemaphoreCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportSemaphoreCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportSemaphoreCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportSemaphoreCreateInfoKHR where
        type StructFields VkExportSemaphoreCreateInfoKHR =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportSemaphoreCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportSemaphoreCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportSemaphoreCreateInfoKHR =
             '[VkSemaphoreCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkExportSemaphoreCreateInfoKHR where
        type VkSTypeMType VkExportSemaphoreCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportSemaphoreCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportSemaphoreCreateInfoKHR where
        type FieldType "sType" VkExportSemaphoreCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportSemaphoreCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportSemaphoreCreateInfoKHR =
             #{offset VkExportSemaphoreCreateInfoKHR, sType}
        type FieldIsArray "sType" VkExportSemaphoreCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreCreateInfoKHR, sType}

instance CanReadField "sType" VkExportSemaphoreCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExportSemaphoreCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportSemaphoreCreateInfoKHR where
        type VkPNextMType VkExportSemaphoreCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportSemaphoreCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportSemaphoreCreateInfoKHR where
        type FieldType "pNext" VkExportSemaphoreCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportSemaphoreCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportSemaphoreCreateInfoKHR =
             #{offset VkExportSemaphoreCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportSemaphoreCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreCreateInfoKHR, pNext}

instance CanReadField "pNext" VkExportSemaphoreCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExportSemaphoreCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleTypes VkExportSemaphoreCreateInfoKHR where
        type VkHandleTypesMType VkExportSemaphoreCreateInfoKHR =
             VkExternalSemaphoreHandleTypeFlagsKHR

        {-# NOINLINE vkHandleTypes #-}
        vkHandleTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfoKHR, handleTypes})

        {-# INLINE vkHandleTypesByteOffset #-}
        vkHandleTypesByteOffset ~_
          = #{offset VkExportSemaphoreCreateInfoKHR, handleTypes}

        {-# INLINE readVkHandleTypes #-}
        readVkHandleTypes p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfoKHR, handleTypes}

        {-# INLINE writeVkHandleTypes #-}
        writeVkHandleTypes p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfoKHR, handleTypes}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportSemaphoreCreateInfoKHR where
        type FieldType "handleTypes" VkExportSemaphoreCreateInfoKHR =
             VkExternalSemaphoreHandleTypeFlagsKHR
        type FieldOptional "handleTypes" VkExportSemaphoreCreateInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportSemaphoreCreateInfoKHR =
             #{offset VkExportSemaphoreCreateInfoKHR, handleTypes}
        type FieldIsArray "handleTypes" VkExportSemaphoreCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreCreateInfoKHR, handleTypes}

instance CanReadField "handleTypes" VkExportSemaphoreCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkHandleTypes

        {-# INLINE readField #-}
        readField = readVkHandleTypes

instance CanWriteField "handleTypes" VkExportSemaphoreCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleTypes

instance Show VkExportSemaphoreCreateInfoKHR where
        showsPrec d x
          = showString "VkExportSemaphoreCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleTypes = " .
                            showsPrec d (vkHandleTypes x) . showChar '}'

pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1

type VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1

pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: CString

pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME <-
        (is_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME -> True)
  where VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
          = _VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME

{-# INLINE _VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME #-}

_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: CString
_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
  = Ptr "VK_KHR_external_semaphore\NUL"##

{-# INLINE is_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME #-}

is_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
  = eqCStrings _VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME

type VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME =
     "VK_KHR_external_semaphore"

pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR =
        VkStructureType 1000077000
