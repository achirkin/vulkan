#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2KHR
       (VkImageMemoryRequirementsInfo2KHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkImage)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkImageMemoryRequirementsInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageMemoryRequirementsInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageMemoryRequirementsInfo2KHR.html VkImageMemoryRequirementsInfo2KHR registry at www.khronos.org>
data VkImageMemoryRequirementsInfo2KHR = VkImageMemoryRequirementsInfo2KHR## Addr##
                                                                            ByteArray##

instance Eq VkImageMemoryRequirementsInfo2KHR where
        (VkImageMemoryRequirementsInfo2KHR## a _) ==
          x@(VkImageMemoryRequirementsInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageMemoryRequirementsInfo2KHR where
        (VkImageMemoryRequirementsInfo2KHR## a _) `compare`
          x@(VkImageMemoryRequirementsInfo2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageMemoryRequirementsInfo2KHR where
        sizeOf ~_ = #{size VkImageMemoryRequirementsInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageMemoryRequirementsInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageMemoryRequirementsInfo2KHR where
        unsafeAddr (VkImageMemoryRequirementsInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageMemoryRequirementsInfo2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageMemoryRequirementsInfo2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageMemoryRequirementsInfo2KHR where
        type StructFields VkImageMemoryRequirementsInfo2KHR =
             '["sType", "pNext", "image"] -- ' closing tick for hsc2hs
        type CUnionType VkImageMemoryRequirementsInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageMemoryRequirementsInfo2KHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageMemoryRequirementsInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageMemoryRequirementsInfo2KHR where
        type FieldType "sType" VkImageMemoryRequirementsInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkImageMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageMemoryRequirementsInfo2KHR =
             #{offset VkImageMemoryRequirementsInfo2KHR, sType}
        type FieldIsArray "sType" VkImageMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageMemoryRequirementsInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageMemoryRequirementsInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageMemoryRequirementsInfo2KHR where
        type FieldType "pNext" VkImageMemoryRequirementsInfo2KHR = Ptr Void
        type FieldOptional "pNext" VkImageMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageMemoryRequirementsInfo2KHR =
             #{offset VkImageMemoryRequirementsInfo2KHR, pNext}
        type FieldIsArray "pNext" VkImageMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageMemoryRequirementsInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageMemoryRequirementsInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "image" VkImageMemoryRequirementsInfo2KHR where
        type FieldType "image" VkImageMemoryRequirementsInfo2KHR = VkImage
        type FieldOptional "image" VkImageMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageMemoryRequirementsInfo2KHR =
             #{offset VkImageMemoryRequirementsInfo2KHR, image}
        type FieldIsArray "image" VkImageMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryRequirementsInfo2KHR, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkImageMemoryRequirementsInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryRequirementsInfo2KHR, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkImageMemoryRequirementsInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryRequirementsInfo2KHR, image}

instance Show VkImageMemoryRequirementsInfo2KHR where
        showsPrec d x
          = showString "VkImageMemoryRequirementsInfo2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "image = " .
                            showsPrec d (getField @"image" x) . showChar '}'
