#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties2
       (VkSparseImageFormatProperties2(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSparseImageFormatProperties (VkSparseImageFormatProperties)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkSparseImageFormatProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkSparseImageFormatProperties    properties;
--   > } VkSparseImageFormatProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSparseImageFormatProperties2.html VkSparseImageFormatProperties2 registry at www.khronos.org>
data VkSparseImageFormatProperties2 = VkSparseImageFormatProperties2## Addr##
                                                                      ByteArray##

instance Eq VkSparseImageFormatProperties2 where
        (VkSparseImageFormatProperties2## a _) ==
          x@(VkSparseImageFormatProperties2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageFormatProperties2 where
        (VkSparseImageFormatProperties2## a _) `compare`
          x@(VkSparseImageFormatProperties2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSparseImageFormatProperties2 where
        sizeOf ~_ = #{size VkSparseImageFormatProperties2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageFormatProperties2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSparseImageFormatProperties2 where
        unsafeAddr (VkSparseImageFormatProperties2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSparseImageFormatProperties2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSparseImageFormatProperties2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSparseImageFormatProperties2 where
        type StructFields VkSparseImageFormatProperties2 =
             '["sType", "pNext", "properties"] -- ' closing tick for hsc2hs
        type CUnionType VkSparseImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSparseImageFormatProperties2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSparseImageFormatProperties2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSparseImageFormatProperties2 where
        type FieldType "sType" VkSparseImageFormatProperties2 =
             VkStructureType
        type FieldOptional "sType" VkSparseImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSparseImageFormatProperties2 =
             #{offset VkSparseImageFormatProperties2, sType}
        type FieldIsArray "sType" VkSparseImageFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSparseImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSparseImageFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSparseImageFormatProperties2 where
        type FieldType "pNext" VkSparseImageFormatProperties2 = Ptr Void
        type FieldOptional "pNext" VkSparseImageFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSparseImageFormatProperties2 =
             #{offset VkSparseImageFormatProperties2, pNext}
        type FieldIsArray "pNext" VkSparseImageFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSparseImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSparseImageFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         HasField "properties" VkSparseImageFormatProperties2 where
        type FieldType "properties" VkSparseImageFormatProperties2 =
             VkSparseImageFormatProperties
        type FieldOptional "properties" VkSparseImageFormatProperties2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "properties" VkSparseImageFormatProperties2 =
             #{offset VkSparseImageFormatProperties2, properties}
        type FieldIsArray "properties" VkSparseImageFormatProperties2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSparseImageFormatProperties2, properties}

instance {-# OVERLAPPING #-}
         CanReadField "properties" VkSparseImageFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2, properties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSparseImageFormatProperties2, properties}

instance {-# OVERLAPPING #-}
         CanWriteField "properties" VkSparseImageFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2, properties}

instance Show VkSparseImageFormatProperties2 where
        showsPrec d x
          = showString "VkSparseImageFormatProperties2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "properties = " .
                            showsPrec d (getField @"properties" x) . showChar '}'
