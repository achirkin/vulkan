#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryHostPointerPropertiesEXT
       (VkMemoryHostPointerPropertiesEXT(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryHostPointerPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     uint32_t memoryTypeBits;
--   > } VkMemoryHostPointerPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryHostPointerPropertiesEXT VkMemoryHostPointerPropertiesEXT registry at www.khronos.org>
data VkMemoryHostPointerPropertiesEXT = VkMemoryHostPointerPropertiesEXT## Addr##
                                                                          ByteArray##

instance Eq VkMemoryHostPointerPropertiesEXT where
        (VkMemoryHostPointerPropertiesEXT## a _) ==
          x@(VkMemoryHostPointerPropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryHostPointerPropertiesEXT where
        (VkMemoryHostPointerPropertiesEXT## a _) `compare`
          x@(VkMemoryHostPointerPropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryHostPointerPropertiesEXT where
        sizeOf ~_ = #{size VkMemoryHostPointerPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryHostPointerPropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryHostPointerPropertiesEXT where
        unsafeAddr (VkMemoryHostPointerPropertiesEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryHostPointerPropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryHostPointerPropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryHostPointerPropertiesEXT where
        type StructFields VkMemoryHostPointerPropertiesEXT =
             '["sType", "pNext", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryHostPointerPropertiesEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryHostPointerPropertiesEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryHostPointerPropertiesEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryHostPointerPropertiesEXT where
        type FieldType "sType" VkMemoryHostPointerPropertiesEXT =
             VkStructureType
        type FieldOptional "sType" VkMemoryHostPointerPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryHostPointerPropertiesEXT =
             #{offset VkMemoryHostPointerPropertiesEXT, sType}
        type FieldIsArray "sType" VkMemoryHostPointerPropertiesEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryHostPointerPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryHostPointerPropertiesEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryHostPointerPropertiesEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryHostPointerPropertiesEXT where
        type FieldType "pNext" VkMemoryHostPointerPropertiesEXT = Ptr Void
        type FieldOptional "pNext" VkMemoryHostPointerPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryHostPointerPropertiesEXT =
             #{offset VkMemoryHostPointerPropertiesEXT, pNext}
        type FieldIsArray "pNext" VkMemoryHostPointerPropertiesEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryHostPointerPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryHostPointerPropertiesEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryHostPointerPropertiesEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryHostPointerPropertiesEXT where
        type FieldType "memoryTypeBits" VkMemoryHostPointerPropertiesEXT =
             Word32
        type FieldOptional "memoryTypeBits"
               VkMemoryHostPointerPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits" VkMemoryHostPointerPropertiesEXT
             =
             #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}
        type FieldIsArray "memoryTypeBits" VkMemoryHostPointerPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeBits" VkMemoryHostPointerPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeBits" VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

instance Show VkMemoryHostPointerPropertiesEXT where
        showsPrec d x
          = showString "VkMemoryHostPointerPropertiesEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryTypeBits = " .
                            showsPrec d (getField @"memoryTypeBits" x) . showChar '}'
