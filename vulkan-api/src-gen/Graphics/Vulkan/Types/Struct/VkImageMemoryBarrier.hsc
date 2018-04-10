#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageMemoryBarrier
       (VkImageMemoryBarrier(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Base                                             (Addr##, ByteArray##,
                                                                       byteArrayContents##,
                                                                       plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkAccessFlags             (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.VkImageLayout             (VkImageLayout)
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Handles                        (VkImage)
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceRange (VkImageSubresourceRange)
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkImageMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   >     VkImageLayout          oldLayout;
--   >     VkImageLayout          newLayout;
--   >     uint32_t               srcQueueFamilyIndex;
--   >     uint32_t               dstQueueFamilyIndex;
--   >     VkImage                image;
--   >     VkImageSubresourceRange subresourceRange;
--   > } VkImageMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkImageMemoryBarrier VkImageMemoryBarrier registry at www.khronos.org>
data VkImageMemoryBarrier = VkImageMemoryBarrier## Addr## ByteArray##

instance Eq VkImageMemoryBarrier where
        (VkImageMemoryBarrier## a _) == x@(VkImageMemoryBarrier## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageMemoryBarrier where
        (VkImageMemoryBarrier## a _) `compare` x@(VkImageMemoryBarrier## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageMemoryBarrier where
        sizeOf ~_ = #{size VkImageMemoryBarrier}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageMemoryBarrier}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageMemoryBarrier where
        unsafeAddr (VkImageMemoryBarrier## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageMemoryBarrier## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageMemoryBarrier## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageMemoryBarrier where
        type StructFields VkImageMemoryBarrier =
             '["sType", "pNext", "srcAccessMask", "dstAccessMask", "oldLayout", -- ' closing tick for hsc2hs
               "newLayout", "srcQueueFamilyIndex", "dstQueueFamilyIndex", "image",
               "subresourceRange"]
        type CUnionType VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageMemoryBarrier = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkImageMemoryBarrier
         where
        type FieldType "sType" VkImageMemoryBarrier = VkStructureType
        type FieldOptional "sType" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, sType}
        type FieldIsArray "sType" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageMemoryBarrier, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkImageMemoryBarrier
         where
        type FieldType "pNext" VkImageMemoryBarrier = Ptr Void
        type FieldOptional "pNext" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, pNext}
        type FieldIsArray "pNext" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         HasField "srcAccessMask" VkImageMemoryBarrier where
        type FieldType "srcAccessMask" VkImageMemoryBarrier = VkAccessFlags
        type FieldOptional "srcAccessMask" VkImageMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "srcAccessMask" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, srcAccessMask}
        type FieldIsArray "srcAccessMask" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "srcAccessMask" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, srcAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "srcAccessMask" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dstAccessMask" VkImageMemoryBarrier where
        type FieldType "dstAccessMask" VkImageMemoryBarrier = VkAccessFlags
        type FieldOptional "dstAccessMask" VkImageMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dstAccessMask" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, dstAccessMask}
        type FieldIsArray "dstAccessMask" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "dstAccessMask" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, dstAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "dstAccessMask" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         HasField "oldLayout" VkImageMemoryBarrier where
        type FieldType "oldLayout" VkImageMemoryBarrier = VkImageLayout
        type FieldOptional "oldLayout" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "oldLayout" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, oldLayout}
        type FieldIsArray "oldLayout" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageMemoryBarrier, oldLayout}

instance {-# OVERLAPPING #-}
         CanReadField "oldLayout" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, oldLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, oldLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "oldLayout" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, oldLayout}

instance {-# OVERLAPPING #-}
         HasField "newLayout" VkImageMemoryBarrier where
        type FieldType "newLayout" VkImageMemoryBarrier = VkImageLayout
        type FieldOptional "newLayout" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "newLayout" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, newLayout}
        type FieldIsArray "newLayout" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageMemoryBarrier, newLayout}

instance {-# OVERLAPPING #-}
         CanReadField "newLayout" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, newLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, newLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "newLayout" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, newLayout}

instance {-# OVERLAPPING #-}
         HasField "srcQueueFamilyIndex" VkImageMemoryBarrier where
        type FieldType "srcQueueFamilyIndex" VkImageMemoryBarrier = Word32
        type FieldOptional "srcQueueFamilyIndex" VkImageMemoryBarrier =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "srcQueueFamilyIndex" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}
        type FieldIsArray "srcQueueFamilyIndex" VkImageMemoryBarrier =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "srcQueueFamilyIndex" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, srcQueueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "srcQueueFamilyIndex" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "dstQueueFamilyIndex" VkImageMemoryBarrier where
        type FieldType "dstQueueFamilyIndex" VkImageMemoryBarrier = Word32
        type FieldOptional "dstQueueFamilyIndex" VkImageMemoryBarrier =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dstQueueFamilyIndex" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}
        type FieldIsArray "dstQueueFamilyIndex" VkImageMemoryBarrier =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "dstQueueFamilyIndex" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, dstQueueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "dstQueueFamilyIndex" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-} HasField "image" VkImageMemoryBarrier
         where
        type FieldType "image" VkImageMemoryBarrier = VkImage
        type FieldOptional "image" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, image}
        type FieldIsArray "image" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageMemoryBarrier, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, image}

instance {-# OVERLAPPING #-}
         HasField "subresourceRange" VkImageMemoryBarrier where
        type FieldType "subresourceRange" VkImageMemoryBarrier =
             VkImageSubresourceRange
        type FieldOptional "subresourceRange" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subresourceRange" VkImageMemoryBarrier =
             #{offset VkImageMemoryBarrier, subresourceRange}
        type FieldIsArray "subresourceRange" VkImageMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageMemoryBarrier, subresourceRange}

instance {-# OVERLAPPING #-}
         CanReadField "subresourceRange" VkImageMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, subresourceRange})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageMemoryBarrier, subresourceRange}

instance {-# OVERLAPPING #-}
         CanWriteField "subresourceRange" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageMemoryBarrier, subresourceRange}

instance Show VkImageMemoryBarrier where
        showsPrec d x
          = showString "VkImageMemoryBarrier {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "srcAccessMask = " .
                            showsPrec d (getField @"srcAccessMask" x) .
                              showString ", " .
                                showString "dstAccessMask = " .
                                  showsPrec d (getField @"dstAccessMask" x) .
                                    showString ", " .
                                      showString "oldLayout = " .
                                        showsPrec d (getField @"oldLayout" x) .
                                          showString ", " .
                                            showString "newLayout = " .
                                              showsPrec d (getField @"newLayout" x) .
                                                showString ", " .
                                                  showString "srcQueueFamilyIndex = " .
                                                    showsPrec d (getField @"srcQueueFamilyIndex" x)
                                                      .
                                                      showString ", " .
                                                        showString "dstQueueFamilyIndex = " .
                                                          showsPrec d
                                                            (getField @"dstQueueFamilyIndex" x)
                                                            .
                                                            showString ", " .
                                                              showString "image = " .
                                                                showsPrec d (getField @"image" x) .
                                                                  showString ", " .
                                                                    showString "subresourceRange = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"subresourceRange"
                                                                           x)
                                                                        . showChar '}'
