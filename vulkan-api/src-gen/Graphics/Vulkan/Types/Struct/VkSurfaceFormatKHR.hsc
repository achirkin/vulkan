#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSurfaceFormatKHR
       (VkSurfaceFormatKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkColorSpaceKHR (VkColorSpaceKHR)
import           Graphics.Vulkan.Types.Enum.VkFormat        (VkFormat)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkSurfaceFormatKHR {
--   >     VkFormat                         format;
--   >     VkColorSpaceKHR                  colorSpace;
--   > } VkSurfaceFormatKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSurfaceFormatKHR.html VkSurfaceFormatKHR registry at www.khronos.org>
data VkSurfaceFormatKHR = VkSurfaceFormatKHR## Addr## ByteArray##

instance Eq VkSurfaceFormatKHR where
        (VkSurfaceFormatKHR## a _) == x@(VkSurfaceFormatKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceFormatKHR where
        (VkSurfaceFormatKHR## a _) `compare` x@(VkSurfaceFormatKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceFormatKHR where
        sizeOf ~_ = #{size VkSurfaceFormatKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceFormatKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceFormatKHR where
        unsafeAddr (VkSurfaceFormatKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceFormatKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceFormatKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceFormatKHR where
        type StructFields VkSurfaceFormatKHR = '["format", "colorSpace"] -- ' closing tick for hsc2hs
        type CUnionType VkSurfaceFormatKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceFormatKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceFormatKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "format" VkSurfaceFormatKHR
         where
        type FieldType "format" VkSurfaceFormatKHR = VkFormat
        type FieldOptional "format" VkSurfaceFormatKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkSurfaceFormatKHR =
             #{offset VkSurfaceFormatKHR, format}
        type FieldIsArray "format" VkSurfaceFormatKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSurfaceFormatKHR, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkSurfaceFormatKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormatKHR, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceFormatKHR, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkSurfaceFormatKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceFormatKHR, format}

instance {-# OVERLAPPING #-}
         HasField "colorSpace" VkSurfaceFormatKHR where
        type FieldType "colorSpace" VkSurfaceFormatKHR = VkColorSpaceKHR
        type FieldOptional "colorSpace" VkSurfaceFormatKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "colorSpace" VkSurfaceFormatKHR =
             #{offset VkSurfaceFormatKHR, colorSpace}
        type FieldIsArray "colorSpace" VkSurfaceFormatKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSurfaceFormatKHR, colorSpace}

instance {-# OVERLAPPING #-}
         CanReadField "colorSpace" VkSurfaceFormatKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormatKHR, colorSpace})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceFormatKHR, colorSpace}

instance {-# OVERLAPPING #-}
         CanWriteField "colorSpace" VkSurfaceFormatKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceFormatKHR, colorSpace}

instance Show VkSurfaceFormatKHR where
        showsPrec d x
          = showString "VkSurfaceFormatKHR {" .
              showString "format = " .
                showsPrec d (getField @"format" x) .
                  showString ", " .
                    showString "colorSpace = " .
                      showsPrec d (getField @"colorSpace" x) . showChar '}'
