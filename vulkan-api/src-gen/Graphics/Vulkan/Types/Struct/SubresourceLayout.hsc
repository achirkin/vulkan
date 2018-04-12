#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SubresourceLayout
       (VkSubresourceLayout(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes  (VkDeviceSize)
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkSubresourceLayout {
--   >     VkDeviceSize           offset;
--   >     VkDeviceSize           size;
--   >     VkDeviceSize           rowPitch;
--   >     VkDeviceSize           arrayPitch;
--   >     VkDeviceSize           depthPitch;
--   > } VkSubresourceLayout;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubresourceLayout VkSubresourceLayout registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkSubresourceLayout where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubresourceLayout, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubresourceLayout, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkSubresourceLayout where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubresourceLayout, offset}

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

instance {-# OVERLAPPING #-}
         CanReadField "size" VkSubresourceLayout where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubresourceLayout, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubresourceLayout, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkSubresourceLayout where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubresourceLayout, size}

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

instance {-# OVERLAPPING #-}
         CanReadField "rowPitch" VkSubresourceLayout where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubresourceLayout, rowPitch})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubresourceLayout, rowPitch}

instance {-# OVERLAPPING #-}
         CanWriteField "rowPitch" VkSubresourceLayout where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubresourceLayout, rowPitch}

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

instance {-# OVERLAPPING #-}
         CanReadField "arrayPitch" VkSubresourceLayout where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubresourceLayout, arrayPitch})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubresourceLayout, arrayPitch}

instance {-# OVERLAPPING #-}
         CanWriteField "arrayPitch" VkSubresourceLayout where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubresourceLayout, arrayPitch}

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

instance {-# OVERLAPPING #-}
         CanReadField "depthPitch" VkSubresourceLayout where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubresourceLayout, depthPitch})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubresourceLayout, depthPitch}

instance {-# OVERLAPPING #-}
         CanWriteField "depthPitch" VkSubresourceLayout where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubresourceLayout, depthPitch}

instance Show VkSubresourceLayout where
        showsPrec d x
          = showString "VkSubresourceLayout {" .
              showString "offset = " .
                showsPrec d (getField @"offset" x) .
                  showString ", " .
                    showString "size = " .
                      showsPrec d (getField @"size" x) .
                        showString ", " .
                          showString "rowPitch = " .
                            showsPrec d (getField @"rowPitch" x) .
                              showString ", " .
                                showString "arrayPitch = " .
                                  showsPrec d (getField @"arrayPitch" x) .
                                    showString ", " .
                                      showString "depthPitch = " .
                                        showsPrec d (getField @"depthPitch" x) . showChar '}'
