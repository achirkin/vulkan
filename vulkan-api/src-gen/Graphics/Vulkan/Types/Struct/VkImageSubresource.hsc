#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSubresource
       (VkImageSubresource(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags (VkImageAspectFlags)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkImageSubresource {
--   >     VkImageAspectFlags     aspectMask;
--   >     uint32_t               mipLevel;
--   >     uint32_t               arrayLayer;
--   > } VkImageSubresource;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageSubresource VkImageSubresource registry at www.khronos.org>
data VkImageSubresource = VkImageSubresource## Addr## ByteArray##

instance Eq VkImageSubresource where
        (VkImageSubresource## a _) == x@(VkImageSubresource## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSubresource where
        (VkImageSubresource## a _) `compare` x@(VkImageSubresource## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSubresource where
        sizeOf ~_ = #{size VkImageSubresource}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageSubresource}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSubresource where
        unsafeAddr (VkImageSubresource## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSubresource## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSubresource## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSubresource where
        type StructFields VkImageSubresource =
             '["aspectMask", "mipLevel", "arrayLayer"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSubresource = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "aspectMask" VkImageSubresource where
        type FieldType "aspectMask" VkImageSubresource = VkImageAspectFlags
        type FieldOptional "aspectMask" VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectMask" VkImageSubresource =
             #{offset VkImageSubresource, aspectMask}
        type FieldIsArray "aspectMask" VkImageSubresource = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageSubresource, aspectMask}

instance {-# OVERLAPPING #-}
         CanReadField "aspectMask" VkImageSubresource where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresource, aspectMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresource, aspectMask}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectMask" VkImageSubresource where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresource, aspectMask}

instance {-# OVERLAPPING #-} HasField "mipLevel" VkImageSubresource
         where
        type FieldType "mipLevel" VkImageSubresource = Word32
        type FieldOptional "mipLevel" VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mipLevel" VkImageSubresource =
             #{offset VkImageSubresource, mipLevel}
        type FieldIsArray "mipLevel" VkImageSubresource = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageSubresource, mipLevel}

instance {-# OVERLAPPING #-}
         CanReadField "mipLevel" VkImageSubresource where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresource, mipLevel})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresource, mipLevel}

instance {-# OVERLAPPING #-}
         CanWriteField "mipLevel" VkImageSubresource where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresource, mipLevel}

instance {-# OVERLAPPING #-}
         HasField "arrayLayer" VkImageSubresource where
        type FieldType "arrayLayer" VkImageSubresource = Word32
        type FieldOptional "arrayLayer" VkImageSubresource = 'False -- ' closing tick for hsc2hs
        type FieldOffset "arrayLayer" VkImageSubresource =
             #{offset VkImageSubresource, arrayLayer}
        type FieldIsArray "arrayLayer" VkImageSubresource = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageSubresource, arrayLayer}

instance {-# OVERLAPPING #-}
         CanReadField "arrayLayer" VkImageSubresource where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSubresource, arrayLayer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageSubresource, arrayLayer}

instance {-# OVERLAPPING #-}
         CanWriteField "arrayLayer" VkImageSubresource where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageSubresource, arrayLayer}

instance Show VkImageSubresource where
        showsPrec d x
          = showString "VkImageSubresource {" .
              showString "aspectMask = " .
                showsPrec d (getField @"aspectMask" x) .
                  showString ", " .
                    showString "mipLevel = " .
                      showsPrec d (getField @"mipLevel" x) .
                        showString ", " .
                          showString "arrayLayer = " .
                            showsPrec d (getField @"arrayLayer" x) . showChar '}'
