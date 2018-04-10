#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkClearValue (VkClearValue(..))
       where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkClearColorValue        (VkClearColorValue)
import           Graphics.Vulkan.Types.Struct.VkClearDepthStencilValue (VkClearDepthStencilValue)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | // Union allowing specification of color or depth and stencil values. Actual value selected is based on attachment being cleared.
--
--   > typedef union VkClearValue {
--   >     VkClearColorValue      color;
--   >     VkClearDepthStencilValue depthStencil;
--   > } VkClearValue;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkClearValue VkClearValue registry at www.khronos.org>
data VkClearValue = VkClearValue## Addr## ByteArray##

instance Eq VkClearValue where
        (VkClearValue## a _) == x@(VkClearValue## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkClearValue where
        (VkClearValue## a _) `compare` x@(VkClearValue## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkClearValue where
        sizeOf ~_ = #{size VkClearValue}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkClearValue}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkClearValue where
        unsafeAddr (VkClearValue## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkClearValue## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkClearValue## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkClearValue where
        type StructFields VkClearValue = '["color", "depthStencil"] -- ' closing tick for hsc2hs
        type CUnionType VkClearValue = 'True -- ' closing tick for hsc2hs
        type ReturnedOnly VkClearValue = 'False -- ' closing tick for hsc2hs
        type StructExtends VkClearValue = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "color" VkClearValue where
        type FieldType "color" VkClearValue = VkClearColorValue
        type FieldOptional "color" VkClearValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "color" VkClearValue =
             #{offset VkClearValue, color}
        type FieldIsArray "color" VkClearValue = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearValue, color}

instance {-# OVERLAPPING #-} CanReadField "color" VkClearValue
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearValue, color})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearValue, color}

instance {-# OVERLAPPING #-} CanWriteField "color" VkClearValue
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearValue, color}

instance {-# OVERLAPPING #-} HasField "depthStencil" VkClearValue
         where
        type FieldType "depthStencil" VkClearValue =
             VkClearDepthStencilValue
        type FieldOptional "depthStencil" VkClearValue = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthStencil" VkClearValue =
             #{offset VkClearValue, depthStencil}
        type FieldIsArray "depthStencil" VkClearValue = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkClearValue, depthStencil}

instance {-# OVERLAPPING #-}
         CanReadField "depthStencil" VkClearValue where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkClearValue, depthStencil})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkClearValue, depthStencil}

instance {-# OVERLAPPING #-}
         CanWriteField "depthStencil" VkClearValue where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkClearValue, depthStencil}

instance Show VkClearValue where
        showsPrec d x
          = showString "VkClearValue {" .
              showString "color = " .
                showsPrec d (getField @"color" x) .
                  showString ", " .
                    showString "depthStencil = " .
                      showsPrec d (getField @"depthStencil" x) . showChar '}'
