#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_KHR_16bit_storage
       (-- * Vulkan extension: @VK_KHR_16bit_storage@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jan-Harald Fredriksen @janharald@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @84@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_storage_buffer_storage_class'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_storage_buffer_storage_class'.
        VkPhysicalDevice16BitStorageFeaturesKHR(..),
        VK_KHR_16BIT_STORAGE_SPEC_VERSION,
        pattern VK_KHR_16BIT_STORAGE_SPEC_VERSION,
        VK_KHR_16BIT_STORAGE_EXTENSION_NAME,
        pattern VK_KHR_16BIT_STORAGE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR)
       where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           GHC.Ptr                                                    (Ptr (..))
import           Graphics.Vulkan.Base                                       (VkDeviceCreateInfo)
import           Graphics.Vulkan.Common                                     (VkBool32,
                                                                             VkStructureType (..))
import           Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2 (VkPhysicalDeviceFeatures2KHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDevice16BitStorageFeaturesKHR {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         storageBuffer16BitAccess;
--   >     VkBool32                         uniformAndStorageBuffer16BitAccess;
--   >     VkBool32                         storagePushConstant16;
--   >     VkBool32                         storageInputOutput16;
--   > } VkPhysicalDevice16BitStorageFeaturesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDevice16BitStorageFeaturesKHR.html VkPhysicalDevice16BitStorageFeaturesKHR registry at www.khronos.org>
data VkPhysicalDevice16BitStorageFeaturesKHR = VkPhysicalDevice16BitStorageFeaturesKHR## Addr##
                                                                                        ByteArray##

instance Eq VkPhysicalDevice16BitStorageFeaturesKHR where
        (VkPhysicalDevice16BitStorageFeaturesKHR## a _) ==
          x@(VkPhysicalDevice16BitStorageFeaturesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDevice16BitStorageFeaturesKHR where
        (VkPhysicalDevice16BitStorageFeaturesKHR## a _) `compare`
          x@(VkPhysicalDevice16BitStorageFeaturesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDevice16BitStorageFeaturesKHR where
        sizeOf ~_
          = #{size VkPhysicalDevice16BitStorageFeaturesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDevice16BitStorageFeaturesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDevice16BitStorageFeaturesKHR
         where
        unsafeAddr (VkPhysicalDevice16BitStorageFeaturesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDevice16BitStorageFeaturesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDevice16BitStorageFeaturesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDevice16BitStorageFeaturesKHR
         where
        type StructFields VkPhysicalDevice16BitStorageFeaturesKHR =
             '["sType", "pNext", "storageBuffer16BitAccess", -- ' closing tick for hsc2hs
               "uniformAndStorageBuffer16BitAccess", "storagePushConstant16",
               "storageInputOutput16"]
        type CUnionType VkPhysicalDevice16BitStorageFeaturesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDevice16BitStorageFeaturesKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDevice16BitStorageFeaturesKHR =
             '[VkPhysicalDeviceFeatures2KHR, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDevice16BitStorageFeaturesKHR where
        type VkSTypeMType VkPhysicalDevice16BitStorageFeaturesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDevice16BitStorageFeaturesKHR where
        type FieldType "sType" VkPhysicalDevice16BitStorageFeaturesKHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDevice16BitStorageFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDevice16BitStorageFeaturesKHR =
             #{offset VkPhysicalDevice16BitStorageFeaturesKHR, sType}
        type FieldIsArray "sType" VkPhysicalDevice16BitStorageFeaturesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, sType}

instance CanReadField "sType"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDevice16BitStorageFeaturesKHR where
        type VkPNextMType VkPhysicalDevice16BitStorageFeaturesKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDevice16BitStorageFeaturesKHR where
        type FieldType "pNext" VkPhysicalDevice16BitStorageFeaturesKHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDevice16BitStorageFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDevice16BitStorageFeaturesKHR =
             #{offset VkPhysicalDevice16BitStorageFeaturesKHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDevice16BitStorageFeaturesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, pNext}

instance CanReadField "pNext"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkStorageBuffer16BitAccess
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        type VkStorageBuffer16BitAccessMType
               VkPhysicalDevice16BitStorageFeaturesKHR
             = VkBool32

        {-# NOINLINE vkStorageBuffer16BitAccess #-}
        vkStorageBuffer16BitAccess x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageBuffer16BitAccess})

        {-# INLINE vkStorageBuffer16BitAccessByteOffset #-}
        vkStorageBuffer16BitAccessByteOffset ~_
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageBuffer16BitAccess}

        {-# INLINE readVkStorageBuffer16BitAccess #-}
        readVkStorageBuffer16BitAccess p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageBuffer16BitAccess}

        {-# INLINE writeVkStorageBuffer16BitAccess #-}
        writeVkStorageBuffer16BitAccess p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageBuffer16BitAccess}

instance {-# OVERLAPPING #-}
         HasField "storageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        type FieldType "storageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = VkBool32
        type FieldOptional "storageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "storageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeaturesKHR
             =
             #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageBuffer16BitAccess}
        type FieldIsArray "storageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageBuffer16BitAccess}

instance CanReadField "storageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkStorageBuffer16BitAccess

        {-# INLINE readField #-}
        readField = readVkStorageBuffer16BitAccess

instance CanWriteField "storageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkStorageBuffer16BitAccess

instance {-# OVERLAPPING #-}
         HasVkUniformAndStorageBuffer16BitAccess
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        type VkUniformAndStorageBuffer16BitAccessMType
               VkPhysicalDevice16BitStorageFeaturesKHR
             = VkBool32

        {-# NOINLINE vkUniformAndStorageBuffer16BitAccess #-}
        vkUniformAndStorageBuffer16BitAccess x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, uniformAndStorageBuffer16BitAccess})

        {-# INLINE vkUniformAndStorageBuffer16BitAccessByteOffset #-}
        vkUniformAndStorageBuffer16BitAccessByteOffset ~_
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, uniformAndStorageBuffer16BitAccess}

        {-# INLINE readVkUniformAndStorageBuffer16BitAccess #-}
        readVkUniformAndStorageBuffer16BitAccess p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, uniformAndStorageBuffer16BitAccess}

        {-# INLINE writeVkUniformAndStorageBuffer16BitAccess #-}
        writeVkUniformAndStorageBuffer16BitAccess p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, uniformAndStorageBuffer16BitAccess}

instance {-# OVERLAPPING #-}
         HasField "uniformAndStorageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        type FieldType "uniformAndStorageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = VkBool32
        type FieldOptional "uniformAndStorageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "uniformAndStorageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeaturesKHR
             =
             #{offset VkPhysicalDevice16BitStorageFeaturesKHR, uniformAndStorageBuffer16BitAccess}
        type FieldIsArray "uniformAndStorageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, uniformAndStorageBuffer16BitAccess}

instance CanReadField "uniformAndStorageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkUniformAndStorageBuffer16BitAccess

        {-# INLINE readField #-}
        readField = readVkUniformAndStorageBuffer16BitAccess

instance CanWriteField "uniformAndStorageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkUniformAndStorageBuffer16BitAccess

instance {-# OVERLAPPING #-}
         HasVkStoragePushConstant16 VkPhysicalDevice16BitStorageFeaturesKHR
         where
        type VkStoragePushConstant16MType
               VkPhysicalDevice16BitStorageFeaturesKHR
             = VkBool32

        {-# NOINLINE vkStoragePushConstant16 #-}
        vkStoragePushConstant16 x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storagePushConstant16})

        {-# INLINE vkStoragePushConstant16ByteOffset #-}
        vkStoragePushConstant16ByteOffset ~_
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storagePushConstant16}

        {-# INLINE readVkStoragePushConstant16 #-}
        readVkStoragePushConstant16 p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storagePushConstant16}

        {-# INLINE writeVkStoragePushConstant16 #-}
        writeVkStoragePushConstant16 p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storagePushConstant16}

instance {-# OVERLAPPING #-}
         HasField "storagePushConstant16"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        type FieldType "storagePushConstant16"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = VkBool32
        type FieldOptional "storagePushConstant16"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "storagePushConstant16"
               VkPhysicalDevice16BitStorageFeaturesKHR
             =
             #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storagePushConstant16}
        type FieldIsArray "storagePushConstant16"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storagePushConstant16}

instance CanReadField "storagePushConstant16"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkStoragePushConstant16

        {-# INLINE readField #-}
        readField = readVkStoragePushConstant16

instance CanWriteField "storagePushConstant16"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkStoragePushConstant16

instance {-# OVERLAPPING #-}
         HasVkStorageInputOutput16 VkPhysicalDevice16BitStorageFeaturesKHR
         where
        type VkStorageInputOutput16MType
               VkPhysicalDevice16BitStorageFeaturesKHR
             = VkBool32

        {-# NOINLINE vkStorageInputOutput16 #-}
        vkStorageInputOutput16 x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageInputOutput16})

        {-# INLINE vkStorageInputOutput16ByteOffset #-}
        vkStorageInputOutput16ByteOffset ~_
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageInputOutput16}

        {-# INLINE readVkStorageInputOutput16 #-}
        readVkStorageInputOutput16 p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageInputOutput16}

        {-# INLINE writeVkStorageInputOutput16 #-}
        writeVkStorageInputOutput16 p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageInputOutput16}

instance {-# OVERLAPPING #-}
         HasField "storageInputOutput16"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        type FieldType "storageInputOutput16"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = VkBool32
        type FieldOptional "storageInputOutput16"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "storageInputOutput16"
               VkPhysicalDevice16BitStorageFeaturesKHR
             =
             #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageInputOutput16}
        type FieldIsArray "storageInputOutput16"
               VkPhysicalDevice16BitStorageFeaturesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageInputOutput16}

instance CanReadField "storageInputOutput16"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE getField #-}
        getField = vkStorageInputOutput16

        {-# INLINE readField #-}
        readField = readVkStorageInputOutput16

instance CanWriteField "storageInputOutput16"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkStorageInputOutput16

instance Show VkPhysicalDevice16BitStorageFeaturesKHR where
        showsPrec d x
          = showString "VkPhysicalDevice16BitStorageFeaturesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkStorageBuffer16BitAccess = " .
                            showsPrec d (vkStorageBuffer16BitAccess x) .
                              showString ", " .
                                showString "vkUniformAndStorageBuffer16BitAccess = " .
                                  showsPrec d (vkUniformAndStorageBuffer16BitAccess x) .
                                    showString ", " .
                                      showString "vkStoragePushConstant16 = " .
                                        showsPrec d (vkStoragePushConstant16 x) .
                                          showString ", " .
                                            showString "vkStorageInputOutput16 = " .
                                              showsPrec d (vkStorageInputOutput16 x) . showChar '}'

pattern VK_KHR_16BIT_STORAGE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_16BIT_STORAGE_SPEC_VERSION = 1

type VK_KHR_16BIT_STORAGE_SPEC_VERSION = 1

pattern VK_KHR_16BIT_STORAGE_EXTENSION_NAME :: CString

pattern VK_KHR_16BIT_STORAGE_EXTENSION_NAME <-
        (is_VK_KHR_16BIT_STORAGE_EXTENSION_NAME -> True)
  where VK_KHR_16BIT_STORAGE_EXTENSION_NAME
          = _VK_KHR_16BIT_STORAGE_EXTENSION_NAME

{-# INLINE _VK_KHR_16BIT_STORAGE_EXTENSION_NAME #-}

_VK_KHR_16BIT_STORAGE_EXTENSION_NAME :: CString
_VK_KHR_16BIT_STORAGE_EXTENSION_NAME
  = Ptr "VK_KHR_16bit_storage\NUL"##

{-# INLINE is_VK_KHR_16BIT_STORAGE_EXTENSION_NAME #-}

is_VK_KHR_16BIT_STORAGE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_16BIT_STORAGE_EXTENSION_NAME
  = eqCStrings _VK_KHR_16BIT_STORAGE_EXTENSION_NAME

type VK_KHR_16BIT_STORAGE_EXTENSION_NAME = "VK_KHR_16bit_storage"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR
        = VkStructureType 1000083000
