#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryRequirements
       (VkMemoryRequirements(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes     (VkDeviceSize)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryRequirements {
--   >     VkDeviceSize           size;
--   >     VkDeviceSize           alignment;
--   >     uint32_t               memoryTypeBits;
--   > } VkMemoryRequirements;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryRequirements.html VkMemoryRequirements registry at www.khronos.org>
data VkMemoryRequirements = VkMemoryRequirements## Addr## ByteArray##

instance Eq VkMemoryRequirements where
        (VkMemoryRequirements## a _) == x@(VkMemoryRequirements## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryRequirements where
        (VkMemoryRequirements## a _) `compare` x@(VkMemoryRequirements## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryRequirements where
        sizeOf ~_ = #{size VkMemoryRequirements}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryRequirements}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryRequirements where
        unsafeAddr (VkMemoryRequirements## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryRequirements## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryRequirements## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryRequirements where
        type StructFields VkMemoryRequirements =
             '["size", "alignment", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryRequirements = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryRequirements = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSize VkMemoryRequirements where
        type VkSizeMType VkMemoryRequirements = VkDeviceSize

        {-# NOINLINE vkSize #-}
        vkSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements, size})

        {-# INLINE vkSizeByteOffset #-}
        vkSizeByteOffset ~_
          = #{offset VkMemoryRequirements, size}

        {-# INLINE readVkSize #-}
        readVkSize p
          = peekByteOff p #{offset VkMemoryRequirements, size}

        {-# INLINE writeVkSize #-}
        writeVkSize p
          = pokeByteOff p #{offset VkMemoryRequirements, size}

instance {-# OVERLAPPING #-} HasField "size" VkMemoryRequirements
         where
        type FieldType "size" VkMemoryRequirements = VkDeviceSize
        type FieldOptional "size" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "size" VkMemoryRequirements =
             #{offset VkMemoryRequirements, size}
        type FieldIsArray "size" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements, size}

instance CanReadField "size" VkMemoryRequirements where
        {-# INLINE getField #-}
        getField = vkSize

        {-# INLINE readField #-}
        readField = readVkSize

instance {-# OVERLAPPING #-} HasVkAlignment VkMemoryRequirements
         where
        type VkAlignmentMType VkMemoryRequirements = VkDeviceSize

        {-# NOINLINE vkAlignment #-}
        vkAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements, alignment})

        {-# INLINE vkAlignmentByteOffset #-}
        vkAlignmentByteOffset ~_
          = #{offset VkMemoryRequirements, alignment}

        {-# INLINE readVkAlignment #-}
        readVkAlignment p
          = peekByteOff p #{offset VkMemoryRequirements, alignment}

        {-# INLINE writeVkAlignment #-}
        writeVkAlignment p
          = pokeByteOff p #{offset VkMemoryRequirements, alignment}

instance {-# OVERLAPPING #-}
         HasField "alignment" VkMemoryRequirements where
        type FieldType "alignment" VkMemoryRequirements = VkDeviceSize
        type FieldOptional "alignment" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "alignment" VkMemoryRequirements =
             #{offset VkMemoryRequirements, alignment}
        type FieldIsArray "alignment" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryRequirements, alignment}

instance CanReadField "alignment" VkMemoryRequirements where
        {-# INLINE getField #-}
        getField = vkAlignment

        {-# INLINE readField #-}
        readField = readVkAlignment

instance {-# OVERLAPPING #-}
         HasVkMemoryTypeBits VkMemoryRequirements where
        type VkMemoryTypeBitsMType VkMemoryRequirements = Word32

        {-# NOINLINE vkMemoryTypeBits #-}
        vkMemoryTypeBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryRequirements, memoryTypeBits})

        {-# INLINE vkMemoryTypeBitsByteOffset #-}
        vkMemoryTypeBitsByteOffset ~_
          = #{offset VkMemoryRequirements, memoryTypeBits}

        {-# INLINE readVkMemoryTypeBits #-}
        readVkMemoryTypeBits p
          = peekByteOff p #{offset VkMemoryRequirements, memoryTypeBits}

        {-# INLINE writeVkMemoryTypeBits #-}
        writeVkMemoryTypeBits p
          = pokeByteOff p #{offset VkMemoryRequirements, memoryTypeBits}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryRequirements where
        type FieldType "memoryTypeBits" VkMemoryRequirements = Word32
        type FieldOptional "memoryTypeBits" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits" VkMemoryRequirements =
             #{offset VkMemoryRequirements, memoryTypeBits}
        type FieldIsArray "memoryTypeBits" VkMemoryRequirements = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryRequirements, memoryTypeBits}

instance CanReadField "memoryTypeBits" VkMemoryRequirements where
        {-# INLINE getField #-}
        getField = vkMemoryTypeBits

        {-# INLINE readField #-}
        readField = readVkMemoryTypeBits

instance Show VkMemoryRequirements where
        showsPrec d x
          = showString "VkMemoryRequirements {" .
              showString "vkSize = " .
                showsPrec d (vkSize x) .
                  showString ", " .
                    showString "vkAlignment = " .
                      showsPrec d (vkAlignment x) .
                        showString ", " .
                          showString "vkMemoryTypeBits = " .
                            showsPrec d (vkMemoryTypeBits x) . showChar '}'
