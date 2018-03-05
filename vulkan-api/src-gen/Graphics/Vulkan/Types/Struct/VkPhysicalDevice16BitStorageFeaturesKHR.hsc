#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDevice16BitStorageFeaturesKHR
       (VkPhysicalDevice16BitStorageFeaturesKHR(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                           (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo           (VkDeviceCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures2KHR (VkPhysicalDeviceFeatures2KHR)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDevice16BitStorageFeaturesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDevice16BitStorageFeaturesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDevice16BitStorageFeaturesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDevice16BitStorageFeaturesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "storageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageBuffer16BitAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageBuffer16BitAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "storageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageBuffer16BitAccess}

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

instance {-# OVERLAPPING #-}
         CanReadField "uniformAndStorageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, uniformAndStorageBuffer16BitAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, uniformAndStorageBuffer16BitAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "uniformAndStorageBuffer16BitAccess"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, uniformAndStorageBuffer16BitAccess}

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

instance {-# OVERLAPPING #-}
         CanReadField "storagePushConstant16"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storagePushConstant16})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storagePushConstant16}

instance {-# OVERLAPPING #-}
         CanWriteField "storagePushConstant16"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storagePushConstant16}

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

instance {-# OVERLAPPING #-}
         CanReadField "storageInputOutput16"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageInputOutput16})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageInputOutput16}

instance {-# OVERLAPPING #-}
         CanWriteField "storageInputOutput16"
           VkPhysicalDevice16BitStorageFeaturesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDevice16BitStorageFeaturesKHR, storageInputOutput16}

instance Show VkPhysicalDevice16BitStorageFeaturesKHR where
        showsPrec d x
          = showString "VkPhysicalDevice16BitStorageFeaturesKHR {" .
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
