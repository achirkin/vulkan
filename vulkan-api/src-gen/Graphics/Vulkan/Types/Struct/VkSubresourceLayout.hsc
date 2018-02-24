#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSubresourceLayout
       (VkSubresourceLayout(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes     (VkDeviceSize)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkSubresourceLayout {
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           size;
--   >     VkDeviceSize           rowPitch;
--   >     VkDeviceSize           arrayPitch;
--   >     VkDeviceSize           depthPitch;
--   > } VkSubresourceLayout;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSubresourceLayout.html VkSubresourceLayout registry at www.khronos.org>
data VkSubresourceLayout = VkSubresourceLayout## Addr## ByteArray##

instance Eq VkSubresourceLayout where
        (VkSubresourceLayout## a _) == x@(VkSubresourceLayout## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSubresourceLayout where
        (VkSubresourceLayout## a _) `compare` x@(VkSubresourceLayout## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSubresourceLayout where
        sizeOf ~_ = #{size VkSubresourceLayout}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSubresourceLayout}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSubresourceLayout where
        unsafeAddr (VkSubresourceLayout## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSubresourceLayout## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSubresourceLayout## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSubresourceLayout where
        type StructFields VkSubresourceLayout =
             '["offset", "size", "rowPitch", "arrayPitch", "depthPitch"] -- ' closing tick for hsc2hs
        type CUnionType VkSubresourceLayout = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSubresourceLayout = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSubresourceLayout = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkOffset VkSubresourceLayout where
        type VkOffsetMType VkSubresourceLayout = VkDeviceSize

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubresourceLayout, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkSubresourceLayout, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkSubresourceLayout, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkSubresourceLayout, offset}

instance {-# OVERLAPPING #-} HasField "offset" VkSubresourceLayout
         where
        type FieldType "offset" VkSubresourceLayout = VkDeviceSize
        type FieldOptional "offset" VkSubresourceLayout = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkSubresourceLayout =
             #{offset VkSubresourceLayout, offset}
        type FieldIsArray "offset" VkSubresourceLayout = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubresourceLayout, offset}

instance CanReadField "offset" VkSubresourceLayout where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkSubresourceLayout where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-} HasVkSize VkSubresourceLayout where
        type VkSizeMType VkSubresourceLayout = VkDeviceSize

        {-# NOINLINE vkSize #-}
        vkSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubresourceLayout, size})

        {-# INLINE vkSizeByteOffset #-}
        vkSizeByteOffset ~_
          = #{offset VkSubresourceLayout, size}

        {-# INLINE readVkSize #-}
        readVkSize p
          = peekByteOff p #{offset VkSubresourceLayout, size}

        {-# INLINE writeVkSize #-}
        writeVkSize p
          = pokeByteOff p #{offset VkSubresourceLayout, size}

instance {-# OVERLAPPING #-} HasField "size" VkSubresourceLayout
         where
        type FieldType "size" VkSubresourceLayout = VkDeviceSize
        type FieldOptional "size" VkSubresourceLayout = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkSubresourceLayout =
             #{offset VkSubresourceLayout, size}
        type FieldIsArray "size" VkSubresourceLayout = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubresourceLayout, size}

instance CanReadField "size" VkSubresourceLayout where
        {-# INLINE getField #-}
        getField = vkSize

        {-# INLINE readField #-}
        readField = readVkSize

instance CanWriteField "size" VkSubresourceLayout where
        {-# INLINE writeField #-}
        writeField = writeVkSize

instance {-# OVERLAPPING #-} HasVkRowPitch VkSubresourceLayout
         where
        type VkRowPitchMType VkSubresourceLayout = VkDeviceSize

        {-# NOINLINE vkRowPitch #-}
        vkRowPitch x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubresourceLayout, rowPitch})

        {-# INLINE vkRowPitchByteOffset #-}
        vkRowPitchByteOffset ~_
          = #{offset VkSubresourceLayout, rowPitch}

        {-# INLINE readVkRowPitch #-}
        readVkRowPitch p
          = peekByteOff p #{offset VkSubresourceLayout, rowPitch}

        {-# INLINE writeVkRowPitch #-}
        writeVkRowPitch p
          = pokeByteOff p #{offset VkSubresourceLayout, rowPitch}

instance {-# OVERLAPPING #-}
         HasField "rowPitch" VkSubresourceLayout where
        type FieldType "rowPitch" VkSubresourceLayout = VkDeviceSize
        type FieldOptional "rowPitch" VkSubresourceLayout = 'False -- ' closing tick for hsc2hs
        type FieldOffset "rowPitch" VkSubresourceLayout =
             #{offset VkSubresourceLayout, rowPitch}
        type FieldIsArray "rowPitch" VkSubresourceLayout = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubresourceLayout, rowPitch}

instance CanReadField "rowPitch" VkSubresourceLayout where
        {-# INLINE getField #-}
        getField = vkRowPitch

        {-# INLINE readField #-}
        readField = readVkRowPitch

instance CanWriteField "rowPitch" VkSubresourceLayout where
        {-# INLINE writeField #-}
        writeField = writeVkRowPitch

instance {-# OVERLAPPING #-} HasVkArrayPitch VkSubresourceLayout
         where
        type VkArrayPitchMType VkSubresourceLayout = VkDeviceSize

        {-# NOINLINE vkArrayPitch #-}
        vkArrayPitch x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubresourceLayout, arrayPitch})

        {-# INLINE vkArrayPitchByteOffset #-}
        vkArrayPitchByteOffset ~_
          = #{offset VkSubresourceLayout, arrayPitch}

        {-# INLINE readVkArrayPitch #-}
        readVkArrayPitch p
          = peekByteOff p #{offset VkSubresourceLayout, arrayPitch}

        {-# INLINE writeVkArrayPitch #-}
        writeVkArrayPitch p
          = pokeByteOff p #{offset VkSubresourceLayout, arrayPitch}

instance {-# OVERLAPPING #-}
         HasField "arrayPitch" VkSubresourceLayout where
        type FieldType "arrayPitch" VkSubresourceLayout = VkDeviceSize
        type FieldOptional "arrayPitch" VkSubresourceLayout = 'False -- ' closing tick for hsc2hs
        type FieldOffset "arrayPitch" VkSubresourceLayout =
             #{offset VkSubresourceLayout, arrayPitch}
        type FieldIsArray "arrayPitch" VkSubresourceLayout = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubresourceLayout, arrayPitch}

instance CanReadField "arrayPitch" VkSubresourceLayout where
        {-# INLINE getField #-}
        getField = vkArrayPitch

        {-# INLINE readField #-}
        readField = readVkArrayPitch

instance CanWriteField "arrayPitch" VkSubresourceLayout where
        {-# INLINE writeField #-}
        writeField = writeVkArrayPitch

instance {-# OVERLAPPING #-} HasVkDepthPitch VkSubresourceLayout
         where
        type VkDepthPitchMType VkSubresourceLayout = VkDeviceSize

        {-# NOINLINE vkDepthPitch #-}
        vkDepthPitch x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubresourceLayout, depthPitch})

        {-# INLINE vkDepthPitchByteOffset #-}
        vkDepthPitchByteOffset ~_
          = #{offset VkSubresourceLayout, depthPitch}

        {-# INLINE readVkDepthPitch #-}
        readVkDepthPitch p
          = peekByteOff p #{offset VkSubresourceLayout, depthPitch}

        {-# INLINE writeVkDepthPitch #-}
        writeVkDepthPitch p
          = pokeByteOff p #{offset VkSubresourceLayout, depthPitch}

instance {-# OVERLAPPING #-}
         HasField "depthPitch" VkSubresourceLayout where
        type FieldType "depthPitch" VkSubresourceLayout = VkDeviceSize
        type FieldOptional "depthPitch" VkSubresourceLayout = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthPitch" VkSubresourceLayout =
             #{offset VkSubresourceLayout, depthPitch}
        type FieldIsArray "depthPitch" VkSubresourceLayout = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubresourceLayout, depthPitch}

instance CanReadField "depthPitch" VkSubresourceLayout where
        {-# INLINE getField #-}
        getField = vkDepthPitch

        {-# INLINE readField #-}
        readField = readVkDepthPitch

instance CanWriteField "depthPitch" VkSubresourceLayout where
        {-# INLINE writeField #-}
        writeField = writeVkDepthPitch

instance Show VkSubresourceLayout where
        showsPrec d x
          = showString "VkSubresourceLayout {" .
              showString "vkOffset = " .
                showsPrec d (vkOffset x) .
                  showString ", " .
                    showString "vkSize = " .
                      showsPrec d (vkSize x) .
                        showString ", " .
                          showString "vkRowPitch = " .
                            showsPrec d (vkRowPitch x) .
                              showString ", " .
                                showString "vkArrayPitch = " .
                                  showsPrec d (vkArrayPitch x) .
                                    showString ", " .
                                      showString "vkDepthPitch = " .
                                        showsPrec d (vkDepthPitch x) . showChar '}'
