#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryBarrier
       (VkMemoryBarrier(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkAccessFlags   (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   > } VkMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkMemoryBarrierVkMemoryBarrier registry at www.khronos.org>
data VkMemoryBarrier = VkMemoryBarrier## Addr## ByteArray##

instance Eq VkMemoryBarrier where
        (VkMemoryBarrier## a _) == x@(VkMemoryBarrier## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryBarrier where
        (VkMemoryBarrier## a _) `compare` x@(VkMemoryBarrier## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryBarrier where
        sizeOf ~_ = #{size VkMemoryBarrier}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryBarrier}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryBarrier where
        unsafeAddr (VkMemoryBarrier## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryBarrier## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryBarrier## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryBarrier where
        type StructFields VkMemoryBarrier =
             '["sType", "pNext", "srcAccessMask", "dstAccessMask"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryBarrier = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkMemoryBarrier where
        type FieldType "sType" VkMemoryBarrier = VkStructureType
        type FieldOptional "sType" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryBarrier =
             #{offset VkMemoryBarrier, sType}
        type FieldIsArray "sType" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryBarrier, sType}

instance {-# OVERLAPPING #-} CanReadField "sType" VkMemoryBarrier
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryBarrier, sType}

instance {-# OVERLAPPING #-} CanWriteField "sType" VkMemoryBarrier
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryBarrier, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkMemoryBarrier where
        type FieldType "pNext" VkMemoryBarrier = Ptr Void
        type FieldOptional "pNext" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryBarrier =
             #{offset VkMemoryBarrier, pNext}
        type FieldIsArray "pNext" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryBarrier, pNext}

instance {-# OVERLAPPING #-} CanReadField "pNext" VkMemoryBarrier
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryBarrier, pNext}

instance {-# OVERLAPPING #-} CanWriteField "pNext" VkMemoryBarrier
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryBarrier, pNext}

instance {-# OVERLAPPING #-}
         HasField "srcAccessMask" VkMemoryBarrier where
        type FieldType "srcAccessMask" VkMemoryBarrier = VkAccessFlags
        type FieldOptional "srcAccessMask" VkMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "srcAccessMask" VkMemoryBarrier =
             #{offset VkMemoryBarrier, srcAccessMask}
        type FieldIsArray "srcAccessMask" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "srcAccessMask" VkMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, srcAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "srcAccessMask" VkMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryBarrier, srcAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dstAccessMask" VkMemoryBarrier where
        type FieldType "dstAccessMask" VkMemoryBarrier = VkAccessFlags
        type FieldOptional "dstAccessMask" VkMemoryBarrier = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dstAccessMask" VkMemoryBarrier =
             #{offset VkMemoryBarrier, dstAccessMask}
        type FieldIsArray "dstAccessMask" VkMemoryBarrier = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "dstAccessMask" VkMemoryBarrier where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, dstAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryBarrier, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "dstAccessMask" VkMemoryBarrier where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryBarrier, dstAccessMask}

instance Show VkMemoryBarrier where
        showsPrec d x
          = showString "VkMemoryBarrier {" .
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
                                  showsPrec d (getField @"dstAccessMask" x) . showChar '}'
