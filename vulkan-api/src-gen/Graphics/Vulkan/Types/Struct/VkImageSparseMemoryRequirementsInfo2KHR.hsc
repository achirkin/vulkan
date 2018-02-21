#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageSparseMemoryRequirementsInfo2KHR
       (VkImageSparseMemoryRequirementsInfo2KHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkImage)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkImageSparseMemoryRequirementsInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkImage                                                              image;
--   > } VkImageSparseMemoryRequirementsInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageSparseMemoryRequirementsInfo2KHR.html VkImageSparseMemoryRequirementsInfo2KHR registry at www.khronos.org>
data VkImageSparseMemoryRequirementsInfo2KHR = VkImageSparseMemoryRequirementsInfo2KHR## Addr##
                                                                                        ByteArray##

instance Eq VkImageSparseMemoryRequirementsInfo2KHR where
        (VkImageSparseMemoryRequirementsInfo2KHR## a _) ==
          x@(VkImageSparseMemoryRequirementsInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageSparseMemoryRequirementsInfo2KHR where
        (VkImageSparseMemoryRequirementsInfo2KHR## a _) `compare`
          x@(VkImageSparseMemoryRequirementsInfo2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageSparseMemoryRequirementsInfo2KHR where
        sizeOf ~_
          = #{size VkImageSparseMemoryRequirementsInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageSparseMemoryRequirementsInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageSparseMemoryRequirementsInfo2KHR
         where
        unsafeAddr (VkImageSparseMemoryRequirementsInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageSparseMemoryRequirementsInfo2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageSparseMemoryRequirementsInfo2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageSparseMemoryRequirementsInfo2KHR
         where
        type StructFields VkImageSparseMemoryRequirementsInfo2KHR =
             '["sType", "pNext", "image"] -- ' closing tick for hsc2hs
        type CUnionType VkImageSparseMemoryRequirementsInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageSparseMemoryRequirementsInfo2KHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageSparseMemoryRequirementsInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkImageSparseMemoryRequirementsInfo2KHR where
        type VkSTypeMType VkImageSparseMemoryRequirementsInfo2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageSparseMemoryRequirementsInfo2KHR where
        type FieldType "sType" VkImageSparseMemoryRequirementsInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkImageSparseMemoryRequirementsInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageSparseMemoryRequirementsInfo2KHR =
             #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}
        type FieldIsArray "sType" VkImageSparseMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, sType}

instance CanReadField "sType"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkImageSparseMemoryRequirementsInfo2KHR where
        type VkPNextMType VkImageSparseMemoryRequirementsInfo2KHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageSparseMemoryRequirementsInfo2KHR where
        type FieldType "pNext" VkImageSparseMemoryRequirementsInfo2KHR =
             Ptr Void
        type FieldOptional "pNext" VkImageSparseMemoryRequirementsInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageSparseMemoryRequirementsInfo2KHR =
             #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}
        type FieldIsArray "pNext" VkImageSparseMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, pNext}

instance CanReadField "pNext"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkImage VkImageSparseMemoryRequirementsInfo2KHR where
        type VkImageMType VkImageSparseMemoryRequirementsInfo2KHR = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageSparseMemoryRequirementsInfo2KHR, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

instance {-# OVERLAPPING #-}
         HasField "image" VkImageSparseMemoryRequirementsInfo2KHR where
        type FieldType "image" VkImageSparseMemoryRequirementsInfo2KHR =
             VkImage
        type FieldOptional "image" VkImageSparseMemoryRequirementsInfo2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageSparseMemoryRequirementsInfo2KHR =
             #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}
        type FieldIsArray "image" VkImageSparseMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageSparseMemoryRequirementsInfo2KHR, image}

instance CanReadField "image"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image"
           VkImageSparseMemoryRequirementsInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance Show VkImageSparseMemoryRequirementsInfo2KHR where
        showsPrec d x
          = showString "VkImageSparseMemoryRequirementsInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkImage = " . showsPrec d (vkImage x) . showChar '}'
