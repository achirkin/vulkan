#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2
       (VkImageSparseMemoryRequirementsInfo2(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkImage)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkImageSparseMemoryRequirementsInfo2 {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageSparseMemoryRequirementsInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkImageSparseMemoryRequirementsInfo2VkImageSparseMemoryRequirementsInfo2 registry at www.khronos.org>
data VkImageSparseMemoryRequirementsInfo2 = VkImageSparseMemoryRequirementsInfo2## Addr##
                                                                                  ByteArray##

instance Eq VkImageSparseMemoryRequirementsInfo2 where
        (VkImageSparseMemoryRequirementsInfo2## a _) ==
          x@(VkImageSparseMemoryRequirementsInfo2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSparseMemoryRequirementsInfo2 where
        (VkImageSparseMemoryRequirementsInfo2## a _) `compare`
          x@(VkImageSparseMemoryRequirementsInfo2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSparseMemoryRequirementsInfo2 where
        sizeOf ~_
          = #{size VkImageSparseMemoryRequirementsInfo2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageSparseMemoryRequirementsInfo2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSparseMemoryRequirementsInfo2
         where
        unsafeAddr (VkImageSparseMemoryRequirementsInfo2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSparseMemoryRequirementsInfo2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSparseMemoryRequirementsInfo2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSparseMemoryRequirementsInfo2 where
        type StructFields VkImageSparseMemoryRequirementsInfo2 =
             '["sType", "pNext", "image"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSparseMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSparseMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSparseMemoryRequirementsInfo2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageSparseMemoryRequirementsInfo2 where
        type FieldType "sType" VkImageSparseMemoryRequirementsInfo2 =
             VkStructureType
        type FieldOptional "sType" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageSparseMemoryRequirementsInfo2 =
             #{offset VkImageSparseMemoryRequirementsInfo2, sType}
        type FieldIsArray "sType" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageSparseMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageSparseMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageSparseMemoryRequirementsInfo2 where
        type FieldType "pNext" VkImageSparseMemoryRequirementsInfo2 =
             Ptr Void
        type FieldOptional "pNext" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageSparseMemoryRequirementsInfo2 =
             #{offset VkImageSparseMemoryRequirementsInfo2, pNext}
        type FieldIsArray "pNext" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageSparseMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageSparseMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         HasField "image" VkImageSparseMemoryRequirementsInfo2 where
        type FieldType "image" VkImageSparseMemoryRequirementsInfo2 =
             VkImage
        type FieldOptional "image" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageSparseMemoryRequirementsInfo2 =
             #{offset VkImageSparseMemoryRequirementsInfo2, image}
        type FieldIsArray "image" VkImageSparseMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkImageSparseMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkImageSparseMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2, image}

instance Show VkImageSparseMemoryRequirementsInfo2 where
        showsPrec d x
          = showString "VkImageSparseMemoryRequirementsInfo2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "image = " .
                            showsPrec d (getField @"image" x) . showChar '}'
