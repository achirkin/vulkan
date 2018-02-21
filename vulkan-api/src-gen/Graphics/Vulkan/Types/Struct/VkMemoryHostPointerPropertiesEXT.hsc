#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryHostPointerPropertiesEXT
       (VkMemoryHostPointerPropertiesEXT(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryHostPointerPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     uint32_t memoryTypeBits;
--   > } VkMemoryHostPointerPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryHostPointerPropertiesEXT.html VkMemoryHostPointerPropertiesEXT registry at www.khronos.org>
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
         HasVkSType VkMemoryHostPointerPropertiesEXT where
        type VkSTypeMType VkMemoryHostPointerPropertiesEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMemoryHostPointerPropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, sType}

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

instance CanReadField "sType" VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkMemoryHostPointerPropertiesEXT where
        type VkPNextMType VkMemoryHostPointerPropertiesEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMemoryHostPointerPropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, pNext}

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

instance CanReadField "pNext" VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMemoryTypeBits VkMemoryHostPointerPropertiesEXT where
        type VkMemoryTypeBitsMType VkMemoryHostPointerPropertiesEXT =
             Word32

        {-# NOINLINE vkMemoryTypeBits #-}
        vkMemoryTypeBits x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits})

        {-# INLINE vkMemoryTypeBitsByteOffset #-}
        vkMemoryTypeBitsByteOffset ~_
          = #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

        {-# INLINE readVkMemoryTypeBits #-}
        readVkMemoryTypeBits p
          = peekByteOff p #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

        {-# INLINE writeVkMemoryTypeBits #-}
        writeVkMemoryTypeBits p
          = pokeByteOff p #{offset VkMemoryHostPointerPropertiesEXT, memoryTypeBits}

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

instance CanReadField "memoryTypeBits"
           VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkMemoryTypeBits

        {-# INLINE readField #-}
        readField = readVkMemoryTypeBits

instance CanWriteField "memoryTypeBits"
           VkMemoryHostPointerPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMemoryTypeBits

instance Show VkMemoryHostPointerPropertiesEXT where
        showsPrec d x
          = showString "VkMemoryHostPointerPropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemoryTypeBits = " .
                            showsPrec d (vkMemoryTypeBits x) . showChar '}'
