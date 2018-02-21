#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPushConstantRange
       (VkPushConstantRange(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags (VkShaderStageFlags)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkPushConstantRange {
--   >     VkShaderStageFlags     stageFlags;
--   >     uint32_t               offset;
--   >     uint32_t               size;
--   > } VkPushConstantRange;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPushConstantRange.html VkPushConstantRange registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkStageFlags VkPushConstantRange
         where
        type VkStageFlagsMType VkPushConstantRange = VkShaderStageFlags

        {-# NOINLINE vkStageFlags #-}
        vkStageFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPushConstantRange, stageFlags})

        {-# INLINE vkStageFlagsByteOffset #-}
        vkStageFlagsByteOffset ~_
          = #{offset VkPushConstantRange, stageFlags}

        {-# INLINE readVkStageFlags #-}
        readVkStageFlags p
          = peekByteOff p #{offset VkPushConstantRange, stageFlags}

        {-# INLINE writeVkStageFlags #-}
        writeVkStageFlags p
          = pokeByteOff p #{offset VkPushConstantRange, stageFlags}

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

instance CanReadField "stageFlags" VkPushConstantRange where
        {-# INLINE getField #-}
        getField = vkStageFlags

        {-# INLINE readField #-}
        readField = readVkStageFlags

instance CanWriteField "stageFlags" VkPushConstantRange where
        {-# INLINE writeField #-}
        writeField = writeVkStageFlags

instance {-# OVERLAPPING #-} HasVkOffset VkPushConstantRange where
        type VkOffsetMType VkPushConstantRange = Word32

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPushConstantRange, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkPushConstantRange, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkPushConstantRange, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkPushConstantRange, offset}

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

instance CanReadField "offset" VkPushConstantRange where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkPushConstantRange where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-} HasVkSize VkPushConstantRange where
        type VkSizeMType VkPushConstantRange = Word32

        {-# NOINLINE vkSize #-}
        vkSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPushConstantRange, size})

        {-# INLINE vkSizeByteOffset #-}
        vkSizeByteOffset ~_
          = #{offset VkPushConstantRange, size}

        {-# INLINE readVkSize #-}
        readVkSize p
          = peekByteOff p #{offset VkPushConstantRange, size}

        {-# INLINE writeVkSize #-}
        writeVkSize p
          = pokeByteOff p #{offset VkPushConstantRange, size}

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

instance CanReadField "size" VkPushConstantRange where
        {-# INLINE getField #-}
        getField = vkSize

        {-# INLINE readField #-}
        readField = readVkSize

instance CanWriteField "size" VkPushConstantRange where
        {-# INLINE writeField #-}
        writeField = writeVkSize

instance Show VkPushConstantRange where
        showsPrec d x
          = showString "VkPushConstantRange {" .
              showString "vkStageFlags = " .
                showsPrec d (vkStageFlags x) .
                  showString ", " .
                    showString "vkOffset = " .
                      showsPrec d (vkOffset x) .
                        showString ", " .
                          showString "vkSize = " . showsPrec d (vkSize x) . showChar '}'
