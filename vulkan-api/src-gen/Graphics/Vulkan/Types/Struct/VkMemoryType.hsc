#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryType (VkMemoryType(..))
       where
import           Foreign.Storable                                 (Storable (..))
import           GHC.Base                                         (Addr##,
                                                                   ByteArray##,
                                                                   byteArrayContents##,
                                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkMemoryPropertyFlags (VkMemoryPropertyFlags)
import           System.IO.Unsafe                                 (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryType {
--   >     VkMemoryPropertyFlags  propertyFlags;
--   >     uint32_t               heapIndex;
--   > } VkMemoryType;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkMemoryType VkMemoryType registry at www.khronos.org>
data VkMemoryType = VkMemoryType## Addr## ByteArray##

instance Eq VkMemoryType where
        (VkMemoryType## a _) == x@(VkMemoryType## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryType where
        (VkMemoryType## a _) `compare` x@(VkMemoryType## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryType where
        sizeOf ~_ = #{size VkMemoryType}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryType}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryType where
        unsafeAddr (VkMemoryType## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryType## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryType## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryType where
        type StructFields VkMemoryType = '["propertyFlags", "heapIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryType = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryType = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryType = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "propertyFlags" VkMemoryType
         where
        type FieldType "propertyFlags" VkMemoryType = VkMemoryPropertyFlags
        type FieldOptional "propertyFlags" VkMemoryType = 'True -- ' closing tick for hsc2hs
        type FieldOffset "propertyFlags" VkMemoryType =
             #{offset VkMemoryType, propertyFlags}
        type FieldIsArray "propertyFlags" VkMemoryType = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryType, propertyFlags}

instance {-# OVERLAPPING #-}
         CanReadField "propertyFlags" VkMemoryType where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryType, propertyFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryType, propertyFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "propertyFlags" VkMemoryType where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryType, propertyFlags}

instance {-# OVERLAPPING #-} HasField "heapIndex" VkMemoryType
         where
        type FieldType "heapIndex" VkMemoryType = Word32
        type FieldOptional "heapIndex" VkMemoryType = 'False -- ' closing tick for hsc2hs
        type FieldOffset "heapIndex" VkMemoryType =
             #{offset VkMemoryType, heapIndex}
        type FieldIsArray "heapIndex" VkMemoryType = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryType, heapIndex}

instance {-# OVERLAPPING #-} CanReadField "heapIndex" VkMemoryType
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryType, heapIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryType, heapIndex}

instance {-# OVERLAPPING #-} CanWriteField "heapIndex" VkMemoryType
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryType, heapIndex}

instance Show VkMemoryType where
        showsPrec d x
          = showString "VkMemoryType {" .
              showString "propertyFlags = " .
                showsPrec d (getField @"propertyFlags" x) .
                  showString ", " .
                    showString "heapIndex = " .
                      showsPrec d (getField @"heapIndex" x) . showChar '}'
