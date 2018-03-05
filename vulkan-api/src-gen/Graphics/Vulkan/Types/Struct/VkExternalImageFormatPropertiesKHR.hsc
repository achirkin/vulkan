#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalImageFormatPropertiesKHR
       (VkExternalImageFormatPropertiesKHR(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryPropertiesKHR (VkExternalMemoryPropertiesKHR)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR   (VkImageFormatProperties2KHR)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkExternalImageFormatPropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryPropertiesKHR externalMemoryProperties;
--   > } VkExternalImageFormatPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExternalImageFormatPropertiesKHR.html VkExternalImageFormatPropertiesKHR registry at www.khronos.org>
data VkExternalImageFormatPropertiesKHR = VkExternalImageFormatPropertiesKHR## Addr##
                                                                              ByteArray##

instance Eq VkExternalImageFormatPropertiesKHR where
        (VkExternalImageFormatPropertiesKHR## a _) ==
          x@(VkExternalImageFormatPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalImageFormatPropertiesKHR where
        (VkExternalImageFormatPropertiesKHR## a _) `compare`
          x@(VkExternalImageFormatPropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalImageFormatPropertiesKHR where
        sizeOf ~_ = #{size VkExternalImageFormatPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalImageFormatPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalImageFormatPropertiesKHR where
        unsafeAddr (VkExternalImageFormatPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalImageFormatPropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalImageFormatPropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalImageFormatPropertiesKHR where
        type StructFields VkExternalImageFormatPropertiesKHR =
             '["sType", "pNext", "externalMemoryProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalImageFormatPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalImageFormatPropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalImageFormatPropertiesKHR =
             '[VkImageFormatProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalImageFormatPropertiesKHR where
        type FieldType "sType" VkExternalImageFormatPropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkExternalImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalImageFormatPropertiesKHR =
             #{offset VkExternalImageFormatPropertiesKHR, sType}
        type FieldIsArray "sType" VkExternalImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalImageFormatPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalImageFormatPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalImageFormatPropertiesKHR where
        type FieldType "pNext" VkExternalImageFormatPropertiesKHR =
             Ptr Void
        type FieldOptional "pNext" VkExternalImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalImageFormatPropertiesKHR =
             #{offset VkExternalImageFormatPropertiesKHR, pNext}
        type FieldIsArray "pNext" VkExternalImageFormatPropertiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalImageFormatPropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalImageFormatPropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryProperties"
           VkExternalImageFormatPropertiesKHR
         where
        type FieldType "externalMemoryProperties"
               VkExternalImageFormatPropertiesKHR
             = VkExternalMemoryPropertiesKHR
        type FieldOptional "externalMemoryProperties"
               VkExternalImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "externalMemoryProperties"
               VkExternalImageFormatPropertiesKHR
             =
             #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}
        type FieldIsArray "externalMemoryProperties"
               VkExternalImageFormatPropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanReadField "externalMemoryProperties"
           VkExternalImageFormatPropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "externalMemoryProperties"
           VkExternalImageFormatPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalImageFormatPropertiesKHR, externalMemoryProperties}

instance Show VkExternalImageFormatPropertiesKHR where
        showsPrec d x
          = showString "VkExternalImageFormatPropertiesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "externalMemoryProperties = " .
                            showsPrec d (getField @"externalMemoryProperties" x) . showChar '}'
