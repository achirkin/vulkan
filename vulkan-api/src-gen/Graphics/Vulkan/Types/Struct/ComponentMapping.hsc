#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ComponentMapping
       (VkComponentMapping(..)) where
import           Foreign.Storable                            (Storable (..))
import           GHC.Base                                    (Addr##, ByteArray##,
                                                              byteArrayContents##,
                                                              plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.ComponentSwizzle (VkComponentSwizzle)
import           System.IO.Unsafe                            (unsafeDupablePerformIO)

-- | > typedef struct VkComponentMapping {
--   >     VkComponentSwizzle r;
--   >     VkComponentSwizzle g;
--   >     VkComponentSwizzle b;
--   >     VkComponentSwizzle a;
--   > } VkComponentMapping;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkComponentMapping VkComponentMapping registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} CanReadField "r" VkComponentMapping
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComponentMapping, r})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComponentMapping, r}

instance {-# OVERLAPPING #-} CanWriteField "r" VkComponentMapping
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComponentMapping, r}

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

instance {-# OVERLAPPING #-} CanReadField "g" VkComponentMapping
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComponentMapping, g})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComponentMapping, g}

instance {-# OVERLAPPING #-} CanWriteField "g" VkComponentMapping
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComponentMapping, g}

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

instance {-# OVERLAPPING #-} CanReadField "b" VkComponentMapping
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComponentMapping, b})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComponentMapping, b}

instance {-# OVERLAPPING #-} CanWriteField "b" VkComponentMapping
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComponentMapping, b}

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

instance {-# OVERLAPPING #-} CanReadField "a" VkComponentMapping
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComponentMapping, a})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComponentMapping, a}

instance {-# OVERLAPPING #-} CanWriteField "a" VkComponentMapping
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComponentMapping, a}

instance Show VkComponentMapping where
        showsPrec d x
          = showString "VkComponentMapping {" .
              showString "r = " .
                showsPrec d (getField @"r" x) .
                  showString ", " .
                    showString "g = " .
                      showsPrec d (getField @"g" x) .
                        showString ", " .
                          showString "b = " .
                            showsPrec d (getField @"b" x) .
                              showString ", " .
                                showString "a = " . showsPrec d (getField @"a" x) . showChar '}'
