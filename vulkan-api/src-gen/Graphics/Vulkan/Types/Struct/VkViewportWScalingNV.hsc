#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkViewportWScalingNV
       (VkViewportWScalingNV(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkViewportWScalingNV {
--   >     float          xcoeff;
--   >     float          ycoeff;
--   > } VkViewportWScalingNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkViewportWScalingNV.html VkViewportWScalingNV registry at www.khronos.org>
data VkViewportWScalingNV = VkViewportWScalingNV## Addr## ByteArray##

instance Eq VkViewportWScalingNV where
        (VkViewportWScalingNV## a _) == x@(VkViewportWScalingNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkViewportWScalingNV where
        (VkViewportWScalingNV## a _) `compare` x@(VkViewportWScalingNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkViewportWScalingNV where
        sizeOf ~_ = #{size VkViewportWScalingNV}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViewportWScalingNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkViewportWScalingNV where
        unsafeAddr (VkViewportWScalingNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkViewportWScalingNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkViewportWScalingNV## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkViewportWScalingNV where
        type StructFields VkViewportWScalingNV = '["xcoeff", "ycoeff"] -- ' closing tick for hsc2hs
        type CUnionType VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkViewportWScalingNV = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "xcoeff" VkViewportWScalingNV
         where
        type FieldType "xcoeff" VkViewportWScalingNV =
             #{type float}
        type FieldOptional "xcoeff" VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "xcoeff" VkViewportWScalingNV =
             #{offset VkViewportWScalingNV, xcoeff}
        type FieldIsArray "xcoeff" VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportWScalingNV, xcoeff}

instance {-# OVERLAPPING #-}
         CanReadField "xcoeff" VkViewportWScalingNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportWScalingNV, xcoeff})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewportWScalingNV, xcoeff}

instance {-# OVERLAPPING #-}
         CanWriteField "xcoeff" VkViewportWScalingNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewportWScalingNV, xcoeff}

instance {-# OVERLAPPING #-} HasField "ycoeff" VkViewportWScalingNV
         where
        type FieldType "ycoeff" VkViewportWScalingNV =
             #{type float}
        type FieldOptional "ycoeff" VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ycoeff" VkViewportWScalingNV =
             #{offset VkViewportWScalingNV, ycoeff}
        type FieldIsArray "ycoeff" VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportWScalingNV, ycoeff}

instance {-# OVERLAPPING #-}
         CanReadField "ycoeff" VkViewportWScalingNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportWScalingNV, ycoeff})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViewportWScalingNV, ycoeff}

instance {-# OVERLAPPING #-}
         CanWriteField "ycoeff" VkViewportWScalingNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViewportWScalingNV, ycoeff}

instance Show VkViewportWScalingNV where
        showsPrec d x
          = showString "VkViewportWScalingNV {" .
              showString "xcoeff = " .
                showsPrec d (getField @"xcoeff" x) .
                  showString ", " .
                    showString "ycoeff = " .
                      showsPrec d (getField @"ycoeff" x) . showChar '}'
