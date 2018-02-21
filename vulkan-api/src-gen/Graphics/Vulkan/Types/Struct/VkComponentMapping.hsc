#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkComponentMapping
       (VkComponentMapping(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkComponentSwizzle (VkComponentSwizzle)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkComponentMapping {
--   >     VkComponentSwizzle r;
--   >     VkComponentSwizzle g;
--   >     VkComponentSwizzle b;
--   >     VkComponentSwizzle a;
--   > } VkComponentMapping;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkComponentMapping.html VkComponentMapping registry at www.khronos.org>
data VkComponentMapping = VkComponentMapping## Addr## ByteArray##

instance Eq VkComponentMapping where
        (VkComponentMapping## a _) == x@(VkComponentMapping## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkComponentMapping where
        (VkComponentMapping## a _) `compare` x@(VkComponentMapping## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkComponentMapping where
        sizeOf ~_ = #{size VkComponentMapping}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkComponentMapping}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkComponentMapping where
        unsafeAddr (VkComponentMapping## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkComponentMapping## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkComponentMapping## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkComponentMapping where
        type StructFields VkComponentMapping = '["r", "g", "b", "a"] -- ' closing tick for hsc2hs
        type CUnionType VkComponentMapping = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkComponentMapping = 'False -- ' closing tick for hsc2hs
        type StructExtends VkComponentMapping = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkR VkComponentMapping where
        type VkRMType VkComponentMapping = VkComponentSwizzle

        {-# NOINLINE vkR #-}
        vkR x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComponentMapping, r})

        {-# INLINE vkRByteOffset #-}
        vkRByteOffset ~_ = #{offset VkComponentMapping, r}

        {-# INLINE readVkR #-}
        readVkR p
          = peekByteOff p #{offset VkComponentMapping, r}

        {-# INLINE writeVkR #-}
        writeVkR p
          = pokeByteOff p #{offset VkComponentMapping, r}

instance {-# OVERLAPPING #-} HasField "r" VkComponentMapping where
        type FieldType "r" VkComponentMapping = VkComponentSwizzle
        type FieldOptional "r" VkComponentMapping = 'False -- ' closing tick for hsc2hs
        type FieldOffset "r" VkComponentMapping =
             #{offset VkComponentMapping, r}
        type FieldIsArray "r" VkComponentMapping = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkComponentMapping, r}

instance CanReadField "r" VkComponentMapping where
        {-# INLINE getField #-}
        getField = vkR

        {-# INLINE readField #-}
        readField = readVkR

instance CanWriteField "r" VkComponentMapping where
        {-# INLINE writeField #-}
        writeField = writeVkR

instance {-# OVERLAPPING #-} HasVkG VkComponentMapping where
        type VkGMType VkComponentMapping = VkComponentSwizzle

        {-# NOINLINE vkG #-}
        vkG x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComponentMapping, g})

        {-# INLINE vkGByteOffset #-}
        vkGByteOffset ~_ = #{offset VkComponentMapping, g}

        {-# INLINE readVkG #-}
        readVkG p
          = peekByteOff p #{offset VkComponentMapping, g}

        {-# INLINE writeVkG #-}
        writeVkG p
          = pokeByteOff p #{offset VkComponentMapping, g}

instance {-# OVERLAPPING #-} HasField "g" VkComponentMapping where
        type FieldType "g" VkComponentMapping = VkComponentSwizzle
        type FieldOptional "g" VkComponentMapping = 'False -- ' closing tick for hsc2hs
        type FieldOffset "g" VkComponentMapping =
             #{offset VkComponentMapping, g}
        type FieldIsArray "g" VkComponentMapping = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkComponentMapping, g}

instance CanReadField "g" VkComponentMapping where
        {-# INLINE getField #-}
        getField = vkG

        {-# INLINE readField #-}
        readField = readVkG

instance CanWriteField "g" VkComponentMapping where
        {-# INLINE writeField #-}
        writeField = writeVkG

instance {-# OVERLAPPING #-} HasVkB VkComponentMapping where
        type VkBMType VkComponentMapping = VkComponentSwizzle

        {-# NOINLINE vkB #-}
        vkB x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComponentMapping, b})

        {-# INLINE vkBByteOffset #-}
        vkBByteOffset ~_ = #{offset VkComponentMapping, b}

        {-# INLINE readVkB #-}
        readVkB p
          = peekByteOff p #{offset VkComponentMapping, b}

        {-# INLINE writeVkB #-}
        writeVkB p
          = pokeByteOff p #{offset VkComponentMapping, b}

instance {-# OVERLAPPING #-} HasField "b" VkComponentMapping where
        type FieldType "b" VkComponentMapping = VkComponentSwizzle
        type FieldOptional "b" VkComponentMapping = 'False -- ' closing tick for hsc2hs
        type FieldOffset "b" VkComponentMapping =
             #{offset VkComponentMapping, b}
        type FieldIsArray "b" VkComponentMapping = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkComponentMapping, b}

instance CanReadField "b" VkComponentMapping where
        {-# INLINE getField #-}
        getField = vkB

        {-# INLINE readField #-}
        readField = readVkB

instance CanWriteField "b" VkComponentMapping where
        {-# INLINE writeField #-}
        writeField = writeVkB

instance {-# OVERLAPPING #-} HasVkA VkComponentMapping where
        type VkAMType VkComponentMapping = VkComponentSwizzle

        {-# NOINLINE vkA #-}
        vkA x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComponentMapping, a})

        {-# INLINE vkAByteOffset #-}
        vkAByteOffset ~_ = #{offset VkComponentMapping, a}

        {-# INLINE readVkA #-}
        readVkA p
          = peekByteOff p #{offset VkComponentMapping, a}

        {-# INLINE writeVkA #-}
        writeVkA p
          = pokeByteOff p #{offset VkComponentMapping, a}

instance {-# OVERLAPPING #-} HasField "a" VkComponentMapping where
        type FieldType "a" VkComponentMapping = VkComponentSwizzle
        type FieldOptional "a" VkComponentMapping = 'False -- ' closing tick for hsc2hs
        type FieldOffset "a" VkComponentMapping =
             #{offset VkComponentMapping, a}
        type FieldIsArray "a" VkComponentMapping = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkComponentMapping, a}

instance CanReadField "a" VkComponentMapping where
        {-# INLINE getField #-}
        getField = vkA

        {-# INLINE readField #-}
        readField = readVkA

instance CanWriteField "a" VkComponentMapping where
        {-# INLINE writeField #-}
        writeField = writeVkA

instance Show VkComponentMapping where
        showsPrec d x
          = showString "VkComponentMapping {" .
              showString "vkR = " .
                showsPrec d (vkR x) .
                  showString ", " .
                    showString "vkG = " .
                      showsPrec d (vkG x) .
                        showString ", " .
                          showString "vkB = " .
                            showsPrec d (vkB x) .
                              showString ", " .
                                showString "vkA = " . showsPrec d (vkA x) . showChar '}'
