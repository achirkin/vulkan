#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryBarrier
       (VkMemoryBarrier(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkAccessFlags   (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryBarrier {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   > } VkMemoryBarrier;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryBarrier.html VkMemoryBarrier registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkMemoryBarrier where
        type VkSTypeMType VkMemoryBarrier = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_ = #{offset VkMemoryBarrier, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryBarrier, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryBarrier, sType}

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

instance CanReadField "sType" VkMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMemoryBarrier where
        type VkPNextMType VkMemoryBarrier = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_ = #{offset VkMemoryBarrier, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryBarrier, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryBarrier, pNext}

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

instance CanReadField "pNext" VkMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkSrcAccessMask VkMemoryBarrier
         where
        type VkSrcAccessMaskMType VkMemoryBarrier = VkAccessFlags

        {-# NOINLINE vkSrcAccessMask #-}
        vkSrcAccessMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, srcAccessMask})

        {-# INLINE vkSrcAccessMaskByteOffset #-}
        vkSrcAccessMaskByteOffset ~_
          = #{offset VkMemoryBarrier, srcAccessMask}

        {-# INLINE readVkSrcAccessMask #-}
        readVkSrcAccessMask p
          = peekByteOff p #{offset VkMemoryBarrier, srcAccessMask}

        {-# INLINE writeVkSrcAccessMask #-}
        writeVkSrcAccessMask p
          = pokeByteOff p #{offset VkMemoryBarrier, srcAccessMask}

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

instance CanReadField "srcAccessMask" VkMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkSrcAccessMask

        {-# INLINE readField #-}
        readField = readVkSrcAccessMask

instance CanWriteField "srcAccessMask" VkMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkSrcAccessMask

instance {-# OVERLAPPING #-} HasVkDstAccessMask VkMemoryBarrier
         where
        type VkDstAccessMaskMType VkMemoryBarrier = VkAccessFlags

        {-# NOINLINE vkDstAccessMask #-}
        vkDstAccessMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryBarrier, dstAccessMask})

        {-# INLINE vkDstAccessMaskByteOffset #-}
        vkDstAccessMaskByteOffset ~_
          = #{offset VkMemoryBarrier, dstAccessMask}

        {-# INLINE readVkDstAccessMask #-}
        readVkDstAccessMask p
          = peekByteOff p #{offset VkMemoryBarrier, dstAccessMask}

        {-# INLINE writeVkDstAccessMask #-}
        writeVkDstAccessMask p
          = pokeByteOff p #{offset VkMemoryBarrier, dstAccessMask}

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

instance CanReadField "dstAccessMask" VkMemoryBarrier where
        {-# INLINE getField #-}
        getField = vkDstAccessMask

        {-# INLINE readField #-}
        readField = readVkDstAccessMask

instance CanWriteField "dstAccessMask" VkMemoryBarrier where
        {-# INLINE writeField #-}
        writeField = writeVkDstAccessMask

instance Show VkMemoryBarrier where
        showsPrec d x
          = showString "VkMemoryBarrier {" .
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
                                  showsPrec d (vkDstAccessMask x) . showChar '}'
