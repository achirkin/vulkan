#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferMemoryBarrier
       (VkBufferMemoryBarrier(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkAccessFlags   (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkBuffer)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBufferMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   >     uint32_t               srcQueueFamilyIndex;
--   >     uint32_t               dstQueueFamilyIndex;
--   >     VkBuffer               buffer;
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           size;
--   > } VkBufferMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkBufferMemoryBarrier VkBufferMemoryBarrier registry at www.khronos.org>
data VkBufferMemoryBarrier = VkBufferMemoryBarrier## Addr##
                                                    ByteArray##

instance Eq VkBufferMemoryBarrier where
        (VkBufferMemoryBarrier## a _) == x@(VkBufferMemoryBarrier## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferMemoryBarrier where
        (VkBufferMemoryBarrier## a _) `compare`
          x@(VkBufferMemoryBarrier## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferMemoryBarrier where
        sizeOf ~_ = #{size VkBufferMemoryBarrier}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkBufferMemoryBarrier}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferMemoryBarrier where
        unsafeAddr (VkBufferMemoryBarrier## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferMemoryBarrier## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferMemoryBarrier## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferMemoryBarrier where
        type StructFields VkBufferMemoryBarrier =
             '["sType", "pNext", "srcAccessMask", "dstAccessMask", -- ' closing tick for hsc2hs
               "srcQueueFamilyIndex", "dstQueueFamilyIndex", "buffer", "offset",
               "size"]
        type CUnionType VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferMemoryBarrier = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkBufferMemoryBarrier
         where
        type FieldType "sType" VkBufferMemoryBarrier = VkStructureType
        type FieldOptional "sType" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, sType}
        type FieldIsArray "sType" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkBufferMemoryBarrier
         where
        type FieldType "pNext" VkBufferMemoryBarrier = Ptr Void
        type FieldOptional "pNext" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, pNext}
        type FieldIsArray "pNext" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         HasField "srcAccessMask" VkBufferMemoryBarrier where
        type FieldType "srcAccessMask" VkBufferMemoryBarrier =
             VkAccessFlags
        type FieldOptional "srcAccessMask" VkBufferMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "srcAccessMask" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, srcAccessMask}
        type FieldIsArray "srcAccessMask" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "srcAccessMask" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, srcAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "srcAccessMask" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dstAccessMask" VkBufferMemoryBarrier where
        type FieldType "dstAccessMask" VkBufferMemoryBarrier =
             VkAccessFlags
        type FieldOptional "dstAccessMask" VkBufferMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dstAccessMask" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, dstAccessMask}
        type FieldIsArray "dstAccessMask" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "dstAccessMask" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, dstAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "dstAccessMask" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         HasField "srcQueueFamilyIndex" VkBufferMemoryBarrier where
        type FieldType "srcQueueFamilyIndex" VkBufferMemoryBarrier = Word32
        type FieldOptional "srcQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "srcQueueFamilyIndex" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}
        type FieldIsArray "srcQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "srcQueueFamilyIndex" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "srcQueueFamilyIndex" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, srcQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "dstQueueFamilyIndex" VkBufferMemoryBarrier where
        type FieldType "dstQueueFamilyIndex" VkBufferMemoryBarrier = Word32
        type FieldOptional "dstQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dstQueueFamilyIndex" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}
        type FieldIsArray "dstQueueFamilyIndex" VkBufferMemoryBarrier =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "dstQueueFamilyIndex" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "dstQueueFamilyIndex" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, dstQueueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBufferMemoryBarrier where
        type FieldType "buffer" VkBufferMemoryBarrier = VkBuffer
        type FieldOptional "buffer" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, buffer}
        type FieldIsArray "buffer" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, buffer}

instance {-# OVERLAPPING #-}
         HasField "offset" VkBufferMemoryBarrier where
        type FieldType "offset" VkBufferMemoryBarrier = VkDeviceSize
        type FieldOptional "offset" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, offset}
        type FieldIsArray "offset" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, offset}

instance {-# OVERLAPPING #-} HasField "size" VkBufferMemoryBarrier
         where
        type FieldType "size" VkBufferMemoryBarrier = VkDeviceSize
        type FieldOptional "size" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkBufferMemoryBarrier =
             #{offset VkBufferMemoryBarrier, size}
        type FieldIsArray "size" VkBufferMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkBufferMemoryBarrier, size}

instance {-# OVERLAPPING #-}
         CanReadField "size" VkBufferMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryBarrier, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryBarrier, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkBufferMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryBarrier, size}

instance Show VkBufferMemoryBarrier where
        showsPrec d x
          = showString "VkBufferMemoryBarrier {" .
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
                                      showString "srcQueueFamilyIndex = " .
                                        showsPrec d (getField @"srcQueueFamilyIndex" x) .
                                          showString ", " .
                                            showString "dstQueueFamilyIndex = " .
                                              showsPrec d (getField @"dstQueueFamilyIndex" x) .
                                                showString ", " .
                                                  showString "buffer = " .
                                                    showsPrec d (getField @"buffer" x) .
                                                      showString ", " .
                                                        showString "offset = " .
                                                          showsPrec d (getField @"offset" x) .
                                                            showString ", " .
                                                              showString "size = " .
                                                                showsPrec d (getField @"size" x) .
                                                                  showChar '}'
