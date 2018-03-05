#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2KHR
       (VkSparseImageFormatProperties2KHR(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties (VkSparseImageFormatProperties)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageFormatProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkSparseImageFormatProperties    properties;
--   > } VkSparseImageFormatProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSparseImageFormatProperties2KHR.html VkSparseImageFormatProperties2KHR registry at www.khronos.org>
data VkSparseImageFormatProperties2KHR = VkSparseImageFormatProperties2KHR## Addr##
                                                                            ByteArray##

instance Eq VkSparseImageFormatProperties2KHR where
        (VkSparseImageFormatProperties2KHR## a _) ==
          x@(VkSparseImageFormatProperties2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageFormatProperties2KHR where
        (VkSparseImageFormatProperties2KHR## a _) `compare`
          x@(VkSparseImageFormatProperties2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageFormatProperties2KHR where
        sizeOf ~_ = #{size VkSparseImageFormatProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageFormatProperties2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageFormatProperties2KHR where
        unsafeAddr (VkSparseImageFormatProperties2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageFormatProperties2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageFormatProperties2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageFormatProperties2KHR where
        type StructFields VkSparseImageFormatProperties2KHR =
             '["sType", "pNext", "properties"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageFormatProperties2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageFormatProperties2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageFormatProperties2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSparseImageFormatProperties2KHR where
        type FieldType "sType" VkSparseImageFormatProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSparseImageFormatProperties2KHR =
             #{offset VkSparseImageFormatProperties2KHR, sType}
        type FieldIsArray "sType" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSparseImageFormatProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSparseImageFormatProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSparseImageFormatProperties2KHR where
        type FieldType "pNext" VkSparseImageFormatProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSparseImageFormatProperties2KHR =
             #{offset VkSparseImageFormatProperties2KHR, pNext}
        type FieldIsArray "pNext" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSparseImageFormatProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSparseImageFormatProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "properties" VkSparseImageFormatProperties2KHR where
        type FieldType "properties" VkSparseImageFormatProperties2KHR =
             VkSparseImageFormatProperties
        type FieldOptional "properties" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "properties" VkSparseImageFormatProperties2KHR =
             #{offset VkSparseImageFormatProperties2KHR, properties}
        type FieldIsArray "properties" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2KHR, properties}

instance {-# OVERLAPPING #-}
         CanReadField "properties" VkSparseImageFormatProperties2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2KHR, properties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties2KHR, properties}

instance {-# OVERLAPPING #-}
         CanWriteField "properties" VkSparseImageFormatProperties2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2KHR, properties}

instance Show VkSparseImageFormatProperties2KHR where
        showsPrec d x
          = showString "VkSparseImageFormatProperties2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "properties = " .
                            showsPrec d (getField @"properties" x) . showChar '}'
