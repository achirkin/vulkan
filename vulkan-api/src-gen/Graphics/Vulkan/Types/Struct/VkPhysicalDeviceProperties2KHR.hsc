#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR
       (VkPhysicalDeviceProperties2KHR(..)) where
import           Foreign.Storable                                        (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType              (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties (VkPhysicalDeviceProperties)
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceProperties       properties;
--   > } VkPhysicalDeviceProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceProperties2KHR.html VkPhysicalDeviceProperties2KHR registry at www.khronos.org>
data VkPhysicalDeviceProperties2KHR = VkPhysicalDeviceProperties2KHR## Addr##
                                                                      ByteArray##

instance Eq VkPhysicalDeviceProperties2KHR where
        (VkPhysicalDeviceProperties2KHR## a _) ==
          x@(VkPhysicalDeviceProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceProperties2KHR where
        (VkPhysicalDeviceProperties2KHR## a _) `compare`
          x@(VkPhysicalDeviceProperties2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceProperties2KHR where
        sizeOf ~_ = #{size VkPhysicalDeviceProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceProperties2KHR where
        unsafeAddr (VkPhysicalDeviceProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceProperties2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceProperties2KHR where
        type StructFields VkPhysicalDeviceProperties2KHR =
             '["sType", "pNext", "properties"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceProperties2KHR where
        type FieldType "sType" VkPhysicalDeviceProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceProperties2KHR =
             #{offset VkPhysicalDeviceProperties2KHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceProperties2KHR where
        type FieldType "pNext" VkPhysicalDeviceProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceProperties2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceProperties2KHR =
             #{offset VkPhysicalDeviceProperties2KHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceProperties2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "properties" VkPhysicalDeviceProperties2KHR where
        type FieldType "properties" VkPhysicalDeviceProperties2KHR =
             VkPhysicalDeviceProperties
        type FieldOptional "properties" VkPhysicalDeviceProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "properties" VkPhysicalDeviceProperties2KHR =
             #{offset VkPhysicalDeviceProperties2KHR, properties}
        type FieldIsArray "properties" VkPhysicalDeviceProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties2KHR, properties}

instance {-# OVERLAPPING #-}
         CanReadField "properties" VkPhysicalDeviceProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties2KHR, properties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties2KHR, properties}

instance {-# OVERLAPPING #-}
         CanWriteField "properties" VkPhysicalDeviceProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties2KHR, properties}

instance Show VkPhysicalDeviceProperties2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceProperties2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "properties = " .
                            showsPrec d (getField @"properties" x) . showChar '}'
