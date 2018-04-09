#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDevice16BitStorageFeatures
       (VkPhysicalDevice16BitStorageFeatures(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Base                                               (Addr##, ByteArray##,
                                                                         byteArrayContents##,
                                                                         plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                        (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo        (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2 (VkPhysicalDeviceFeatures2)
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDevice16BitStorageFeatures {
--   >     VkStructureType sType;
--   >     void*      pNext;
--   >     VkBool32                         storageBuffer16BitAccess;
--   >     VkBool32                         uniformAndStorageBuffer16BitAccess;
--   >     VkBool32                         storagePushConstant16;
--   >     VkBool32                         storageInputOutput16;
--   > } VkPhysicalDevice16BitStorageFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDevice16BitStorageFeaturesVkPhysicalDevice16BitStorageFeatures registry at www.khronos.org>
data VkPhysicalDevice16BitStorageFeatures = VkPhysicalDevice16BitStorageFeatures## Addr##
                                                                                  ByteArray##

instance Eq VkPhysicalDevice16BitStorageFeatures where
        (VkPhysicalDevice16BitStorageFeatures## a _) ==
          x@(VkPhysicalDevice16BitStorageFeatures## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDevice16BitStorageFeatures where
        (VkPhysicalDevice16BitStorageFeatures## a _) `compare`
          x@(VkPhysicalDevice16BitStorageFeatures## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDevice16BitStorageFeatures where
        sizeOf ~_
          = #{size VkPhysicalDevice16BitStorageFeatures}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDevice16BitStorageFeatures}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDevice16BitStorageFeatures
         where
        unsafeAddr (VkPhysicalDevice16BitStorageFeatures## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDevice16BitStorageFeatures## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDevice16BitStorageFeatures##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDevice16BitStorageFeatures where
        type StructFields VkPhysicalDevice16BitStorageFeatures =
             '["sType", "pNext", "storageBuffer16BitAccess", -- ' closing tick for hsc2hs
               "uniformAndStorageBuffer16BitAccess", "storagePushConstant16",
               "storageInputOutput16"]
        type CUnionType VkPhysicalDevice16BitStorageFeatures = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDevice16BitStorageFeatures = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDevice16BitStorageFeatures =
             '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDevice16BitStorageFeatures where
        type FieldType "sType" VkPhysicalDevice16BitStorageFeatures =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDevice16BitStorageFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDevice16BitStorageFeatures =
             #{offset VkPhysicalDevice16BitStorageFeatures, sType}
        type FieldIsArray "sType" VkPhysicalDevice16BitStorageFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeatures, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDevice16BitStorageFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeatures, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDevice16BitStorageFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDevice16BitStorageFeatures where
        type FieldType "pNext" VkPhysicalDevice16BitStorageFeatures =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDevice16BitStorageFeatures =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDevice16BitStorageFeatures =
             #{offset VkPhysicalDevice16BitStorageFeatures, pNext}
        type FieldIsArray "pNext" VkPhysicalDevice16BitStorageFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDevice16BitStorageFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeatures, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDevice16BitStorageFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, pNext}

instance {-# OVERLAPPING #-}
         HasField "storageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeatures
         where
        type FieldType "storageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeatures
             = VkBool32
        type FieldOptional "storageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "storageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeatures
             =
             #{offset VkPhysicalDevice16BitStorageFeatures, storageBuffer16BitAccess}
        type FieldIsArray "storageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeatures, storageBuffer16BitAccess}

instance {-# OVERLAPPING #-}
         CanReadField "storageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeatures, storageBuffer16BitAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, storageBuffer16BitAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "storageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, storageBuffer16BitAccess}

instance {-# OVERLAPPING #-}
         HasField "uniformAndStorageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeatures
         where
        type FieldType "uniformAndStorageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeatures
             = VkBool32
        type FieldOptional "uniformAndStorageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "uniformAndStorageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeatures
             =
             #{offset VkPhysicalDevice16BitStorageFeatures, uniformAndStorageBuffer16BitAccess}
        type FieldIsArray "uniformAndStorageBuffer16BitAccess"
               VkPhysicalDevice16BitStorageFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeatures, uniformAndStorageBuffer16BitAccess}

instance {-# OVERLAPPING #-}
         CanReadField "uniformAndStorageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeatures, uniformAndStorageBuffer16BitAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, uniformAndStorageBuffer16BitAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "uniformAndStorageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, uniformAndStorageBuffer16BitAccess}

instance {-# OVERLAPPING #-}
         HasField "storagePushConstant16"
           VkPhysicalDevice16BitStorageFeatures
         where
        type FieldType "storagePushConstant16"
               VkPhysicalDevice16BitStorageFeatures
             = VkBool32
        type FieldOptional "storagePushConstant16"
               VkPhysicalDevice16BitStorageFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "storagePushConstant16"
               VkPhysicalDevice16BitStorageFeatures
             =
             #{offset VkPhysicalDevice16BitStorageFeatures, storagePushConstant16}
        type FieldIsArray "storagePushConstant16"
               VkPhysicalDevice16BitStorageFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeatures, storagePushConstant16}

instance {-# OVERLAPPING #-}
         CanReadField "storagePushConstant16"
           VkPhysicalDevice16BitStorageFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeatures, storagePushConstant16})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, storagePushConstant16}

instance {-# OVERLAPPING #-}
         CanWriteField "storagePushConstant16"
           VkPhysicalDevice16BitStorageFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, storagePushConstant16}

instance {-# OVERLAPPING #-}
         HasField "storageInputOutput16"
           VkPhysicalDevice16BitStorageFeatures
         where
        type FieldType "storageInputOutput16"
               VkPhysicalDevice16BitStorageFeatures
             = VkBool32
        type FieldOptional "storageInputOutput16"
               VkPhysicalDevice16BitStorageFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "storageInputOutput16"
               VkPhysicalDevice16BitStorageFeatures
             =
             #{offset VkPhysicalDevice16BitStorageFeatures, storageInputOutput16}
        type FieldIsArray "storageInputOutput16"
               VkPhysicalDevice16BitStorageFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDevice16BitStorageFeatures, storageInputOutput16}

instance {-# OVERLAPPING #-}
         CanReadField "storageInputOutput16"
           VkPhysicalDevice16BitStorageFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeatures, storageInputOutput16})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, storageInputOutput16}

instance {-# OVERLAPPING #-}
         CanWriteField "storageInputOutput16"
           VkPhysicalDevice16BitStorageFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeatures, storageInputOutput16}

instance Show VkPhysicalDevice16BitStorageFeatures where
        showsPrec d x
          = showString "VkPhysicalDevice16BitStorageFeatures {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "storageBuffer16BitAccess = " .
                            showsPrec d (getField @"storageBuffer16BitAccess" x) .
                              showString ", " .
                                showString "uniformAndStorageBuffer16BitAccess = " .
                                  showsPrec d (getField @"uniformAndStorageBuffer16BitAccess" x) .
                                    showString ", " .
                                      showString "storagePushConstant16 = " .
                                        showsPrec d (getField @"storagePushConstant16" x) .
                                          showString ", " .
                                            showString "storageInputOutput16 = " .
                                              showsPrec d (getField @"storageInputOutput16" x) .
                                                showChar '}'
