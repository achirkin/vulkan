#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferMemoryRequirementsInfo2KHR
       (VkBufferMemoryRequirementsInfo2KHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkBuffer)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBufferMemoryRequirementsInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkBuffer                                                             buffer;
--   > } VkBufferMemoryRequirementsInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBufferMemoryRequirementsInfo2KHR.html VkBufferMemoryRequirementsInfo2KHR registry at www.khronos.org>
data VkBufferMemoryRequirementsInfo2KHR = VkBufferMemoryRequirementsInfo2KHR## Addr##
                                                                              ByteArray##

instance Eq VkBufferMemoryRequirementsInfo2KHR where
        (VkBufferMemoryRequirementsInfo2KHR## a _) ==
          x@(VkBufferMemoryRequirementsInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferMemoryRequirementsInfo2KHR where
        (VkBufferMemoryRequirementsInfo2KHR## a _) `compare`
          x@(VkBufferMemoryRequirementsInfo2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferMemoryRequirementsInfo2KHR where
        sizeOf ~_ = #{size VkBufferMemoryRequirementsInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBufferMemoryRequirementsInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferMemoryRequirementsInfo2KHR where
        unsafeAddr (VkBufferMemoryRequirementsInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferMemoryRequirementsInfo2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferMemoryRequirementsInfo2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferMemoryRequirementsInfo2KHR where
        type StructFields VkBufferMemoryRequirementsInfo2KHR =
             '["sType", "pNext", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkBufferMemoryRequirementsInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferMemoryRequirementsInfo2KHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferMemoryRequirementsInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkBufferMemoryRequirementsInfo2KHR where
        type VkSTypeMType VkBufferMemoryRequirementsInfo2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkBufferMemoryRequirementsInfo2KHR where
        type FieldType "sType" VkBufferMemoryRequirementsInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferMemoryRequirementsInfo2KHR =
             #{offset VkBufferMemoryRequirementsInfo2KHR, sType}
        type FieldIsArray "sType" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

instance CanReadField "sType" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkBufferMemoryRequirementsInfo2KHR where
        type VkPNextMType VkBufferMemoryRequirementsInfo2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBufferMemoryRequirementsInfo2KHR where
        type FieldType "pNext" VkBufferMemoryRequirementsInfo2KHR =
             Ptr Void
        type FieldOptional "pNext" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferMemoryRequirementsInfo2KHR =
             #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}
        type FieldIsArray "pNext" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

instance CanReadField "pNext" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkBuffer VkBufferMemoryRequirementsInfo2KHR where
        type VkBufferMType VkBufferMemoryRequirementsInfo2KHR = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2KHR, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBufferMemoryRequirementsInfo2KHR where
        type FieldType "buffer" VkBufferMemoryRequirementsInfo2KHR =
             VkBuffer
        type FieldOptional "buffer" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBufferMemoryRequirementsInfo2KHR =
             #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}
        type FieldIsArray "buffer" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

instance CanReadField "buffer" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkBuffer

        {-# INLINE readField #-}
        readField = readVkBuffer

instance CanWriteField "buffer" VkBufferMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkBuffer

instance Show VkBufferMemoryRequirementsInfo2KHR where
        showsPrec d x
          = showString "VkBufferMemoryRequirementsInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkBuffer = " . showsPrec d (vkBuffer x) . showChar '}'
