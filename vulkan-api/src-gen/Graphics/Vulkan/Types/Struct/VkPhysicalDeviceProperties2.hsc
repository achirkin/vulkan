#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2
       (VkPhysicalDeviceProperties2(..)) where
import           Foreign.Storable                                        (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType              (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties (VkPhysicalDeviceProperties)
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceProperties       properties;
--   > } VkPhysicalDeviceProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceProperties2.html VkPhysicalDeviceProperties2 registry at www.khronos.org>
data VkPhysicalDeviceProperties2 = VkPhysicalDeviceProperties2## Addr##
                                                                ByteArray##

instance Eq VkPhysicalDeviceProperties2 where
        (VkPhysicalDeviceProperties2## a _) ==
          x@(VkPhysicalDeviceProperties2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceProperties2 where
        (VkPhysicalDeviceProperties2## a _) `compare`
          x@(VkPhysicalDeviceProperties2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceProperties2 where
        sizeOf ~_ = #{size VkPhysicalDeviceProperties2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPhysicalDeviceProperties2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceProperties2 where
        unsafeAddr (VkPhysicalDeviceProperties2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceProperties2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceProperties2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceProperties2 where
        type StructFields VkPhysicalDeviceProperties2 =
             '["sType", "pNext", "properties"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceProperties2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceProperties2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceProperties2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceProperties2 where
        type FieldType "sType" VkPhysicalDeviceProperties2 =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceProperties2 =
             #{offset VkPhysicalDeviceProperties2, sType}
        type FieldIsArray "sType" VkPhysicalDeviceProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceProperties2 where
        type FieldType "pNext" VkPhysicalDeviceProperties2 = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceProperties2 =
             #{offset VkPhysicalDeviceProperties2, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties2, pNext}

instance {-# OVERLAPPING #-}
         HasField "properties" VkPhysicalDeviceProperties2 where
        type FieldType "properties" VkPhysicalDeviceProperties2 =
             VkPhysicalDeviceProperties
        type FieldOptional "properties" VkPhysicalDeviceProperties2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "properties" VkPhysicalDeviceProperties2 =
             #{offset VkPhysicalDeviceProperties2, properties}
        type FieldIsArray "properties" VkPhysicalDeviceProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceProperties2, properties}

instance {-# OVERLAPPING #-}
         CanReadField "properties" VkPhysicalDeviceProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties2, properties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceProperties2, properties}

instance {-# OVERLAPPING #-}
         CanWriteField "properties" VkPhysicalDeviceProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties2, properties}

instance Show VkPhysicalDeviceProperties2 where
        showsPrec d x
          = showString "VkPhysicalDeviceProperties2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "properties = " .
                            showsPrec d (getField @"properties" x) . showChar '}'
