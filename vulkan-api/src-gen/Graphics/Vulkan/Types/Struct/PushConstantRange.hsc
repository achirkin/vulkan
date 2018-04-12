#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PushConstantRange
       (VkPushConstantRange(..)) where
import           Foreign.Storable                  (Storable (..))
import           GHC.Base                          (Addr##, ByteArray##,
                                                    byteArrayContents##,
                                                    plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Shader (VkShaderStageFlags)
import           System.IO.Unsafe                  (unsafeDupablePerformIO)

-- | > typedef struct VkPushConstantRange {
--   >     VkShaderStageFlags     stageFlags;
--   >     uint32_t               offset;
--   >     uint32_t               size;
--   > } VkPushConstantRange;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPushConstantRange VkPushConstantRange registry at www.khronos.org>
data VkPushConstantRange = VkPushConstantRange## Addr## ByteArray##

instance Eq VkPushConstantRange where
        (VkPushConstantRange## a _) == x@(VkPushConstantRange## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPushConstantRange where
        (VkPushConstantRange## a _) `compare` x@(VkPushConstantRange## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPushConstantRange where
        sizeOf ~_ = #{size VkPushConstantRange}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPushConstantRange}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPushConstantRange where
        unsafeAddr (VkPushConstantRange## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPushConstantRange## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPushConstantRange## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPushConstantRange where
        type StructFields VkPushConstantRange =
             '["stageFlags", "offset", "size"] -- ' closing tick for hsc2hs
        type CUnionType VkPushConstantRange = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPushConstantRange = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPushConstantRange = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "stageFlags" VkPushConstantRange where
        type FieldType "stageFlags" VkPushConstantRange =
             VkShaderStageFlags
        type FieldOptional "stageFlags" VkPushConstantRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stageFlags" VkPushConstantRange =
             #{offset VkPushConstantRange, stageFlags}
        type FieldIsArray "stageFlags" VkPushConstantRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPushConstantRange, stageFlags}

instance {-# OVERLAPPING #-}
         CanReadField "stageFlags" VkPushConstantRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPushConstantRange, stageFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPushConstantRange, stageFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "stageFlags" VkPushConstantRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPushConstantRange, stageFlags}

instance {-# OVERLAPPING #-} HasField "offset" VkPushConstantRange
         where
        type FieldType "offset" VkPushConstantRange = Word32
        type FieldOptional "offset" VkPushConstantRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkPushConstantRange =
             #{offset VkPushConstantRange, offset}
        type FieldIsArray "offset" VkPushConstantRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPushConstantRange, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkPushConstantRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPushConstantRange, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPushConstantRange, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkPushConstantRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPushConstantRange, offset}

instance {-# OVERLAPPING #-} HasField "size" VkPushConstantRange
         where
        type FieldType "size" VkPushConstantRange = Word32
        type FieldOptional "size" VkPushConstantRange = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkPushConstantRange =
             #{offset VkPushConstantRange, size}
        type FieldIsArray "size" VkPushConstantRange = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkPushConstantRange, size}

instance {-# OVERLAPPING #-}
         CanReadField "size" VkPushConstantRange where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPushConstantRange, size})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPushConstantRange, size}

instance {-# OVERLAPPING #-}
         CanWriteField "size" VkPushConstantRange where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPushConstantRange, size}

instance Show VkPushConstantRange where
        showsPrec d x
          = showString "VkPushConstantRange {" .
              showString "stageFlags = " .
                showsPrec d (getField @"stageFlags" x) .
                  showString ", " .
                    showString "offset = " .
                      showsPrec d (getField @"offset" x) .
                        showString ", " .
                          showString "size = " .
                            showsPrec d (getField @"size" x) . showChar '}'
