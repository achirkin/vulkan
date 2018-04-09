#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2
       (VkImageMemoryRequirementsInfo2(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkImage)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkImageMemoryRequirementsInfo2 {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageMemoryRequirementsInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkImageMemoryRequirementsInfo2VkImageMemoryRequirementsInfo2 registry at www.khronos.org>
data VkImageMemoryRequirementsInfo2 = VkImageMemoryRequirementsInfo2## Addr##
                                                                      ByteArray##

instance Eq VkImageMemoryRequirementsInfo2 where
        (VkImageMemoryRequirementsInfo2## a _) ==
          x@(VkImageMemoryRequirementsInfo2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageMemoryRequirementsInfo2 where
        (VkImageMemoryRequirementsInfo2## a _) `compare`
          x@(VkImageMemoryRequirementsInfo2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageMemoryRequirementsInfo2 where
        sizeOf ~_ = #{size VkImageMemoryRequirementsInfo2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageMemoryRequirementsInfo2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageMemoryRequirementsInfo2 where
        unsafeAddr (VkImageMemoryRequirementsInfo2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageMemoryRequirementsInfo2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageMemoryRequirementsInfo2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageMemoryRequirementsInfo2 where
        type StructFields VkImageMemoryRequirementsInfo2 =
             '["sType", "pNext", "image"] -- ' closing tick for hsc2hs
        type CUnionType VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageMemoryRequirementsInfo2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageMemoryRequirementsInfo2 where
        type FieldType "sType" VkImageMemoryRequirementsInfo2 =
             VkStructureType
        type FieldOptional "sType" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageMemoryRequirementsInfo2 =
             #{offset VkImageMemoryRequirementsInfo2, sType}
        type FieldIsArray "sType" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageMemoryRequirementsInfo2 where
        type FieldType "pNext" VkImageMemoryRequirementsInfo2 = Ptr Void
        type FieldOptional "pNext" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageMemoryRequirementsInfo2 =
             #{offset VkImageMemoryRequirementsInfo2, pNext}
        type FieldIsArray "pNext" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         HasField "image" VkImageMemoryRequirementsInfo2 where
        type FieldType "image" VkImageMemoryRequirementsInfo2 = VkImage
        type FieldOptional "image" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageMemoryRequirementsInfo2 =
             #{offset VkImageMemoryRequirementsInfo2, image}
        type FieldIsArray "image" VkImageMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkImageMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkImageMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2, image}

instance Show VkImageMemoryRequirementsInfo2 where
        showsPrec d x
          = showString "VkImageMemoryRequirementsInfo2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "image = " .
                            showsPrec d (getField @"image" x) . showChar '}'
