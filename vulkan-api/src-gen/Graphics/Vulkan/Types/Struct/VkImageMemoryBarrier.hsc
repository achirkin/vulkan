#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageMemoryBarrier
       (VkImageMemoryBarrier(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkAccessFlags             (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.VkImageLayout             (VkImageLayout)
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Handles                        (VkImage)
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceRange (VkImageSubresourceRange)
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageMemoryBarrier.html VkImageMemoryBarrier registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkImageMemoryBarrier where
        type VkSTypeMType VkImageMemoryBarrier = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageMemoryBarrier, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageMemoryBarrier, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageMemoryBarrier, sType}

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

instance CanReadField "sType" VkImageMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkImageMemoryBarrier where
        type VkPNextMType VkImageMemoryBarrier = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageMemoryBarrier, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageMemoryBarrier, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageMemoryBarrier, pNext}

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

instance CanReadField "pNext" VkImageMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSrcAccessMask VkImageMemoryBarrier where
        type VkSrcAccessMaskMType VkImageMemoryBarrier = VkAccessFlags

        {-# NOINLINE vkSrcAccessMask #-}
        vkSrcAccessMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, srcAccessMask})

        {-# INLINE vkSrcAccessMaskByteOffset #-}
        vkSrcAccessMaskByteOffset ~_
          = #{offset VkImageMemoryBarrier, srcAccessMask}

        {-# INLINE readVkSrcAccessMask #-}
        readVkSrcAccessMask p
          = peekByteOff p #{offset VkImageMemoryBarrier, srcAccessMask}

        {-# INLINE writeVkSrcAccessMask #-}
        writeVkSrcAccessMask p
          = pokeByteOff p #{offset VkImageMemoryBarrier, srcAccessMask}

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

instance CanReadField "srcAccessMask" VkImageMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkSrcAccessMask

        {-# INLINE readField #-}
        readField = readVkSrcAccessMask

instance CanWriteField "srcAccessMask" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkSrcAccessMask

instance {-# OVERLAPPING #-}
         HasVkDstAccessMask VkImageMemoryBarrier where
        type VkDstAccessMaskMType VkImageMemoryBarrier = VkAccessFlags

        {-# NOINLINE vkDstAccessMask #-}
        vkDstAccessMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, dstAccessMask})

        {-# INLINE vkDstAccessMaskByteOffset #-}
        vkDstAccessMaskByteOffset ~_
          = #{offset VkImageMemoryBarrier, dstAccessMask}

        {-# INLINE readVkDstAccessMask #-}
        readVkDstAccessMask p
          = peekByteOff p #{offset VkImageMemoryBarrier, dstAccessMask}

        {-# INLINE writeVkDstAccessMask #-}
        writeVkDstAccessMask p
          = pokeByteOff p #{offset VkImageMemoryBarrier, dstAccessMask}

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

instance CanReadField "dstAccessMask" VkImageMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkDstAccessMask

        {-# INLINE readField #-}
        readField = readVkDstAccessMask

instance CanWriteField "dstAccessMask" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkDstAccessMask

instance {-# OVERLAPPING #-} HasVkOldLayout VkImageMemoryBarrier
         where
        type VkOldLayoutMType VkImageMemoryBarrier = VkImageLayout

        {-# NOINLINE vkOldLayout #-}
        vkOldLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, oldLayout})

        {-# INLINE vkOldLayoutByteOffset #-}
        vkOldLayoutByteOffset ~_
          = #{offset VkImageMemoryBarrier, oldLayout}

        {-# INLINE readVkOldLayout #-}
        readVkOldLayout p
          = peekByteOff p #{offset VkImageMemoryBarrier, oldLayout}

        {-# INLINE writeVkOldLayout #-}
        writeVkOldLayout p
          = pokeByteOff p #{offset VkImageMemoryBarrier, oldLayout}

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

instance CanReadField "oldLayout" VkImageMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkOldLayout

        {-# INLINE readField #-}
        readField = readVkOldLayout

instance CanWriteField "oldLayout" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkOldLayout

instance {-# OVERLAPPING #-} HasVkNewLayout VkImageMemoryBarrier
         where
        type VkNewLayoutMType VkImageMemoryBarrier = VkImageLayout

        {-# NOINLINE vkNewLayout #-}
        vkNewLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, newLayout})

        {-# INLINE vkNewLayoutByteOffset #-}
        vkNewLayoutByteOffset ~_
          = #{offset VkImageMemoryBarrier, newLayout}

        {-# INLINE readVkNewLayout #-}
        readVkNewLayout p
          = peekByteOff p #{offset VkImageMemoryBarrier, newLayout}

        {-# INLINE writeVkNewLayout #-}
        writeVkNewLayout p
          = pokeByteOff p #{offset VkImageMemoryBarrier, newLayout}

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

instance CanReadField "newLayout" VkImageMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkNewLayout

        {-# INLINE readField #-}
        readField = readVkNewLayout

instance CanWriteField "newLayout" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkNewLayout

instance {-# OVERLAPPING #-}
         HasVkSrcQueueFamilyIndex VkImageMemoryBarrier where
        type VkSrcQueueFamilyIndexMType VkImageMemoryBarrier = Word32

        {-# NOINLINE vkSrcQueueFamilyIndex #-}
        vkSrcQueueFamilyIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, srcQueueFamilyIndex})

        {-# INLINE vkSrcQueueFamilyIndexByteOffset #-}
        vkSrcQueueFamilyIndexByteOffset ~_
          = #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}

        {-# INLINE readVkSrcQueueFamilyIndex #-}
        readVkSrcQueueFamilyIndex p
          = peekByteOff p #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}

        {-# INLINE writeVkSrcQueueFamilyIndex #-}
        writeVkSrcQueueFamilyIndex p
          = pokeByteOff p #{offset VkImageMemoryBarrier, srcQueueFamilyIndex}

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

instance CanReadField "srcQueueFamilyIndex" VkImageMemoryBarrier
         where
        {-# INLINE getField #-}
        getField = vkSrcQueueFamilyIndex

        {-# INLINE readField #-}
        readField = readVkSrcQueueFamilyIndex

instance CanWriteField "srcQueueFamilyIndex" VkImageMemoryBarrier
         where
        {-# INLINE writeField #-}
        writeField = writeVkSrcQueueFamilyIndex

instance {-# OVERLAPPING #-}
         HasVkDstQueueFamilyIndex VkImageMemoryBarrier where
        type VkDstQueueFamilyIndexMType VkImageMemoryBarrier = Word32

        {-# NOINLINE vkDstQueueFamilyIndex #-}
        vkDstQueueFamilyIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, dstQueueFamilyIndex})

        {-# INLINE vkDstQueueFamilyIndexByteOffset #-}
        vkDstQueueFamilyIndexByteOffset ~_
          = #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}

        {-# INLINE readVkDstQueueFamilyIndex #-}
        readVkDstQueueFamilyIndex p
          = peekByteOff p #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}

        {-# INLINE writeVkDstQueueFamilyIndex #-}
        writeVkDstQueueFamilyIndex p
          = pokeByteOff p #{offset VkImageMemoryBarrier, dstQueueFamilyIndex}

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

instance CanReadField "dstQueueFamilyIndex" VkImageMemoryBarrier
         where
        {-# INLINE getField #-}
        getField = vkDstQueueFamilyIndex

        {-# INLINE readField #-}
        readField = readVkDstQueueFamilyIndex

instance CanWriteField "dstQueueFamilyIndex" VkImageMemoryBarrier
         where
        {-# INLINE writeField #-}
        writeField = writeVkDstQueueFamilyIndex

instance {-# OVERLAPPING #-} HasVkImage VkImageMemoryBarrier where
        type VkImageMType VkImageMemoryBarrier = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkImageMemoryBarrier, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkImageMemoryBarrier, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkImageMemoryBarrier, image}

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

instance CanReadField "image" VkImageMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image" VkImageMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance {-# OVERLAPPING #-}
         HasVkSubresourceRange VkImageMemoryBarrier where
        type VkSubresourceRangeMType VkImageMemoryBarrier =
             VkImageSubresourceRange

        {-# NOINLINE vkSubresourceRange #-}
        vkSubresourceRange x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageMemoryBarrier, subresourceRange})

        {-# INLINE vkSubresourceRangeByteOffset #-}
        vkSubresourceRangeByteOffset ~_
          = #{offset VkImageMemoryBarrier, subresourceRange}

        {-# INLINE readVkSubresourceRange #-}
        readVkSubresourceRange p
          = peekByteOff p #{offset VkImageMemoryBarrier, subresourceRange}

        {-# INLINE writeVkSubresourceRange #-}
        writeVkSubresourceRange p
          = pokeByteOff p #{offset VkImageMemoryBarrier, subresourceRange}

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

instance CanReadField "subresourceRange" VkImageMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkSubresourceRange

        {-# INLINE readField #-}
        readField = readVkSubresourceRange

instance CanWriteField "subresourceRange" VkImageMemoryBarrier
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubresourceRange

instance Show VkImageMemoryBarrier where
        showsPrec d x
          = showString "VkImageMemoryBarrier {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSrcAccessMask = " .
                            showsPrec d (vkSrcAccessMask x) .
                              showString ", " .
                                showString "vkDstAccessMask = " .
                                  showsPrec d (vkDstAccessMask x) .
                                    showString ", " .
                                      showString "vkOldLayout = " .
                                        showsPrec d (vkOldLayout x) .
                                          showString ", " .
                                            showString "vkNewLayout = " .
                                              showsPrec d (vkNewLayout x) .
                                                showString ", " .
                                                  showString "vkSrcQueueFamilyIndex = " .
                                                    showsPrec d (vkSrcQueueFamilyIndex x) .
                                                      showString ", " .
                                                        showString "vkDstQueueFamilyIndex = " .
                                                          showsPrec d (vkDstQueueFamilyIndex x) .
                                                            showString ", " .
                                                              showString "vkImage = " .
                                                                showsPrec d (vkImage x) .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkSubresourceRange = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkSubresourceRange x)
                                                                        . showChar '}'
