#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageFormatProperties2
       (VkImageFormatProperties2(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Base                                             (Addr##, ByteArray##,
                                                                       byteArrayContents##,
                                                                       plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties (VkImageFormatProperties)
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkImageFormatProperties2 {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkImageFormatProperties          imageFormatProperties;
--   > } VkImageFormatProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkImageFormatProperties2VkImageFormatProperties2 registry at www.khronos.org>
data VkImageFormatProperties2 = VkImageFormatProperties2## Addr##
                                                          ByteArray##

instance Eq VkImageFormatProperties2 where
        (VkImageFormatProperties2## a _) ==
          x@(VkImageFormatProperties2## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageFormatProperties2 where
        (VkImageFormatProperties2## a _) `compare`
          x@(VkImageFormatProperties2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageFormatProperties2 where
        sizeOf ~_ = #{size VkImageFormatProperties2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageFormatProperties2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageFormatProperties2 where
        unsafeAddr (VkImageFormatProperties2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageFormatProperties2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageFormatProperties2## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageFormatProperties2 where
        type StructFields VkImageFormatProperties2 =
             '["sType", "pNext", "imageFormatProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageFormatProperties2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkImageFormatProperties2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageFormatProperties2 where
        type FieldType "sType" VkImageFormatProperties2 = VkStructureType
        type FieldOptional "sType" VkImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageFormatProperties2 =
             #{offset VkImageFormatProperties2, sType}
        type FieldIsArray "sType" VkImageFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageFormatProperties2 where
        type FieldType "pNext" VkImageFormatProperties2 = Ptr Void
        type FieldOptional "pNext" VkImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageFormatProperties2 =
             #{offset VkImageFormatProperties2, pNext}
        type FieldIsArray "pNext" VkImageFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         HasField "imageFormatProperties" VkImageFormatProperties2 where
        type FieldType "imageFormatProperties" VkImageFormatProperties2 =
             VkImageFormatProperties
        type FieldOptional "imageFormatProperties" VkImageFormatProperties2
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "imageFormatProperties" VkImageFormatProperties2 =
             #{offset VkImageFormatProperties2, imageFormatProperties}
        type FieldIsArray "imageFormatProperties" VkImageFormatProperties2
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageFormatProperties2, imageFormatProperties}

instance {-# OVERLAPPING #-}
         CanReadField "imageFormatProperties" VkImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2, imageFormatProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageFormatProperties2, imageFormatProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "imageFormatProperties" VkImageFormatProperties2
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageFormatProperties2, imageFormatProperties}

instance Show VkImageFormatProperties2 where
        showsPrec d x
          = showString "VkImageFormatProperties2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "imageFormatProperties = " .
                            showsPrec d (getField @"imageFormatProperties" x) . showChar '}'
