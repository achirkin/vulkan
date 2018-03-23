#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPresentRegionKHR
       (VkPresentRegionKHR(..)) where
import           Foreign.Storable                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkRectLayerKHR (VkRectLayerKHR)
import           System.IO.Unsafe                            (unsafeDupablePerformIO)

-- | > typedef struct VkPresentRegionKHR {
--   >     uint32_t         rectangleCount;
--   >     const VkRectLayerKHR*   pRectangles;
--   > } VkPresentRegionKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPresentRegionKHR.html VkPresentRegionKHR registry at www.khronos.org>
data VkPresentRegionKHR = VkPresentRegionKHR## Addr## ByteArray##

instance Eq VkPresentRegionKHR where
        (VkPresentRegionKHR## a _) == x@(VkPresentRegionKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPresentRegionKHR where
        (VkPresentRegionKHR## a _) `compare` x@(VkPresentRegionKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPresentRegionKHR where
        sizeOf ~_ = #{size VkPresentRegionKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPresentRegionKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPresentRegionKHR where
        unsafeAddr (VkPresentRegionKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPresentRegionKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPresentRegionKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPresentRegionKHR where
        type StructFields VkPresentRegionKHR =
             '["rectangleCount", "pRectangles"] -- ' closing tick for hsc2hs
        type CUnionType VkPresentRegionKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPresentRegionKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPresentRegionKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "rectangleCount" VkPresentRegionKHR where
        type FieldType "rectangleCount" VkPresentRegionKHR = Word32
        type FieldOptional "rectangleCount" VkPresentRegionKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "rectangleCount" VkPresentRegionKHR =
             #{offset VkPresentRegionKHR, rectangleCount}
        type FieldIsArray "rectangleCount" VkPresentRegionKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPresentRegionKHR, rectangleCount}

instance {-# OVERLAPPING #-}
         CanReadField "rectangleCount" VkPresentRegionKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionKHR, rectangleCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentRegionKHR, rectangleCount}

instance {-# OVERLAPPING #-}
         CanWriteField "rectangleCount" VkPresentRegionKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentRegionKHR, rectangleCount}

instance {-# OVERLAPPING #-}
         HasField "pRectangles" VkPresentRegionKHR where
        type FieldType "pRectangles" VkPresentRegionKHR =
             Ptr VkRectLayerKHR
        type FieldOptional "pRectangles" VkPresentRegionKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pRectangles" VkPresentRegionKHR =
             #{offset VkPresentRegionKHR, pRectangles}
        type FieldIsArray "pRectangles" VkPresentRegionKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPresentRegionKHR, pRectangles}

instance {-# OVERLAPPING #-}
         CanReadField "pRectangles" VkPresentRegionKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPresentRegionKHR, pRectangles})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPresentRegionKHR, pRectangles}

instance {-# OVERLAPPING #-}
         CanWriteField "pRectangles" VkPresentRegionKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPresentRegionKHR, pRectangles}

instance Show VkPresentRegionKHR where
        showsPrec d x
          = showString "VkPresentRegionKHR {" .
              showString "rectangleCount = " .
                showsPrec d (getField @"rectangleCount" x) .
                  showString ", " .
                    showString "pRectangles = " .
                      showsPrec d (getField @"pRectangles" x) . showChar '}'
