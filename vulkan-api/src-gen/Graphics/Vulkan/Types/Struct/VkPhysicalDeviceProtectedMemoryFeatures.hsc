#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProtectedMemoryFeatures
       (VkPhysicalDeviceProtectedMemoryFeatures(..)) where
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

-- | > typedef struct VkPhysicalDeviceProtectedMemoryFeatures {
--   >     VkStructureType sType;
--   >     void*                               pNext;
--   >     VkBool32                            protectedMemory;
--   > } VkPhysicalDeviceProtectedMemoryFeatures;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDeviceProtectedMemoryFeatures VkPhysicalDeviceProtectedMemoryFeatures registry at www.khronos.org>
data VkPhysicalDeviceProtectedMemoryFeatures = VkPhysicalDeviceProtectedMemoryFeatures## Addr##
                                                                                        ByteArray##

instance Eq VkPhysicalDeviceProtectedMemoryFeatures where
        (VkPhysicalDeviceProtectedMemoryFeatures## a _) ==
          x@(VkPhysicalDeviceProtectedMemoryFeatures## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceProtectedMemoryFeatures where
        (VkPhysicalDeviceProtectedMemoryFeatures## a _) `compare`
          x@(VkPhysicalDeviceProtectedMemoryFeatures## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceProtectedMemoryFeatures where
        sizeOf ~_
          = #{size VkPhysicalDeviceProtectedMemoryFeatures}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceProtectedMemoryFeatures}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceProtectedMemoryFeatures
         where
        unsafeAddr (VkPhysicalDeviceProtectedMemoryFeatures## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceProtectedMemoryFeatures## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceProtectedMemoryFeatures##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceProtectedMemoryFeatures
         where
        type StructFields VkPhysicalDeviceProtectedMemoryFeatures =
             '["sType", "pNext", "protectedMemory"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceProtectedMemoryFeatures = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceProtectedMemoryFeatures = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceProtectedMemoryFeatures =
             '[VkPhysicalDeviceFeatures2, VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceProtectedMemoryFeatures where
        type FieldType "sType" VkPhysicalDeviceProtectedMemoryFeatures =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceProtectedMemoryFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceProtectedMemoryFeatures =
             #{offset VkPhysicalDeviceProtectedMemoryFeatures, sType}
        type FieldIsArray "sType" VkPhysicalDeviceProtectedMemoryFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProtectedMemoryFeatures, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceProtectedMemoryFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProtectedMemoryFeatures, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProtectedMemoryFeatures, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceProtectedMemoryFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProtectedMemoryFeatures, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceProtectedMemoryFeatures where
        type FieldType "pNext" VkPhysicalDeviceProtectedMemoryFeatures =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceProtectedMemoryFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceProtectedMemoryFeatures =
             #{offset VkPhysicalDeviceProtectedMemoryFeatures, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceProtectedMemoryFeatures =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProtectedMemoryFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceProtectedMemoryFeatures where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProtectedMemoryFeatures, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProtectedMemoryFeatures, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceProtectedMemoryFeatures where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProtectedMemoryFeatures, pNext}

instance {-# OVERLAPPING #-}
         HasField "protectedMemory" VkPhysicalDeviceProtectedMemoryFeatures
         where
        type FieldType "protectedMemory"
               VkPhysicalDeviceProtectedMemoryFeatures
             = VkBool32
        type FieldOptional "protectedMemory"
               VkPhysicalDeviceProtectedMemoryFeatures
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "protectedMemory"
               VkPhysicalDeviceProtectedMemoryFeatures
             =
             #{offset VkPhysicalDeviceProtectedMemoryFeatures, protectedMemory}
        type FieldIsArray "protectedMemory"
               VkPhysicalDeviceProtectedMemoryFeatures
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProtectedMemoryFeatures, protectedMemory}

instance {-# OVERLAPPING #-}
         CanReadField "protectedMemory"
           VkPhysicalDeviceProtectedMemoryFeatures
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProtectedMemoryFeatures, protectedMemory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProtectedMemoryFeatures, protectedMemory}

instance {-# OVERLAPPING #-}
         CanWriteField "protectedMemory"
           VkPhysicalDeviceProtectedMemoryFeatures
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProtectedMemoryFeatures, protectedMemory}

instance Show VkPhysicalDeviceProtectedMemoryFeatures where
        showsPrec d x
          = showString "VkPhysicalDeviceProtectedMemoryFeatures {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "protectedMemory = " .
                            showsPrec d (getField @"protectedMemory" x) . showChar '}'
