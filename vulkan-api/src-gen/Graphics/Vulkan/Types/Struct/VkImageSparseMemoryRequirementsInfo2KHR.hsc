#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2KHR
       (VkImageSparseMemoryRequirementsInfo2KHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkImage)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkImageSparseMemoryRequirementsInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageSparseMemoryRequirementsInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageSparseMemoryRequirementsInfo2KHR.html VkImageSparseMemoryRequirementsInfo2KHR registry at www.khronos.org>
data VkImageSparseMemoryRequirementsInfo2KHR = VkImageSparseMemoryRequirementsInfo2KHR## Addr##
                                                                                        ByteArray##

instance Eq VkImageSparseMemoryRequirementsInfo2KHR where
        (VkImageSparseMemoryRequirementsInfo2KHR## a _) ==
          x@(VkImageSparseMemoryRequirementsInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSparseMemoryRequirementsInfo2KHR where
        (VkImageSparseMemoryRequirementsInfo2KHR## a _) `compare`
          x@(VkImageSparseMemoryRequirementsInfo2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSparseMemoryRequirementsInfo2KHR where
        sizeOf ~_
          = #{size VkImageSparseMemoryRequirementsInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageSparseMemoryRequirementsInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSparseMemoryRequirementsInfo2KHR
         where
        unsafeAddr (VkImageSparseMemoryRequirementsInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSparseMemoryRequirementsInfo2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSparseMemoryRequirementsInfo2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSparseMemoryRequirementsInfo2KHR
         where
        type StructFields VkImageSparseMemoryRequirementsInfo2KHR =
             '["sType", "pNext", "image"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSparseMemoryRequirementsInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSparseMemoryRequirementsInfo2KHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSparseMemoryRequirementsInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageSparseMemoryRequirementsInfo2KHR where
        type FieldType "sType" VkImageSparseMemoryRequirementsInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkImageSparseMemoryRequirementsInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageSparseMemoryRequirementsInfo2KHR =
             #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}
        type FieldIsArray "sType" VkImageSparseMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageSparseMemoryRequirementsInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageSparseMemoryRequirementsInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageSparseMemoryRequirementsInfo2KHR where
        type FieldType "pNext" VkImageSparseMemoryRequirementsInfo2KHR =
             Ptr Void
        type FieldOptional "pNext" VkImageSparseMemoryRequirementsInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageSparseMemoryRequirementsInfo2KHR =
             #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}
        type FieldIsArray "pNext" VkImageSparseMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageSparseMemoryRequirementsInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageSparseMemoryRequirementsInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "image" VkImageSparseMemoryRequirementsInfo2KHR where
        type FieldType "image" VkImageSparseMemoryRequirementsInfo2KHR =
             VkImage
        type FieldOptional "image" VkImageSparseMemoryRequirementsInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageSparseMemoryRequirementsInfo2KHR =
             #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}
        type FieldIsArray "image" VkImageSparseMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkImageSparseMemoryRequirementsInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2KHR, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkImageSparseMemoryRequirementsInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

instance Show VkImageSparseMemoryRequirementsInfo2KHR where
        showsPrec d x
          = showString "VkImageSparseMemoryRequirementsInfo2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "image = " .
                            showsPrec d (getField @"image" x) . showChar '}'
