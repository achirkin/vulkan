#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRenderPassInputAttachmentAspectCreateInfoKHR
       (VkRenderPassInputAttachmentAspectCreateInfoKHR(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkInputAttachmentAspectReferenceKHR
                                                                                   (VkInputAttachmentAspectReferenceKHR)
import           Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo
                                                                                   (VkRenderPassCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkRenderPassInputAttachmentAspectCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                     pNext;
--   >     uint32_t                        aspectReferenceCount;
--   >     const VkInputAttachmentAspectReferenceKHR* pAspectReferences;
--   > } VkRenderPassInputAttachmentAspectCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkRenderPassInputAttachmentAspectCreateInfoKHR.html VkRenderPassInputAttachmentAspectCreateInfoKHR registry at www.khronos.org>
data VkRenderPassInputAttachmentAspectCreateInfoKHR = VkRenderPassInputAttachmentAspectCreateInfoKHR## Addr##
                                                                                                      ByteArray##

instance Eq VkRenderPassInputAttachmentAspectCreateInfoKHR where
        (VkRenderPassInputAttachmentAspectCreateInfoKHR## a _) ==
          x@(VkRenderPassInputAttachmentAspectCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassInputAttachmentAspectCreateInfoKHR where
        (VkRenderPassInputAttachmentAspectCreateInfoKHR## a _) `compare`
          x@(VkRenderPassInputAttachmentAspectCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        sizeOf ~_
          = #{size VkRenderPassInputAttachmentAspectCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassInputAttachmentAspectCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        unsafeAddr (VkRenderPassInputAttachmentAspectCreateInfoKHR## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkRenderPassInputAttachmentAspectCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassInputAttachmentAspectCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type StructFields VkRenderPassInputAttachmentAspectCreateInfoKHR =
             '["sType", "pNext", "aspectReferenceCount", "pAspectReferences"] -- ' closing tick for hsc2hs
        type CUnionType VkRenderPassInputAttachmentAspectCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassInputAttachmentAspectCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassInputAttachmentAspectCreateInfoKHR =
             '[VkRenderPassCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkRenderPassInputAttachmentAspectCreateInfoKHR where
        type VkSTypeMType VkRenderPassInputAttachmentAspectCreateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type FieldType "sType"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = VkStructureType
        type FieldOptional "sType"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}
        type FieldIsArray "sType"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}

instance CanReadField "sType"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkRenderPassInputAttachmentAspectCreateInfoKHR where
        type VkPNextMType VkRenderPassInputAttachmentAspectCreateInfoKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type FieldType "pNext"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = Ptr Void
        type FieldOptional "pNext"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}
        type FieldIsArray "pNext"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}

instance CanReadField "pNext"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkAspectReferenceCount
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type VkAspectReferenceCountMType
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = Word32

        {-# NOINLINE vkAspectReferenceCount #-}
        vkAspectReferenceCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount})

        {-# INLINE vkAspectReferenceCountByteOffset #-}
        vkAspectReferenceCountByteOffset ~_
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}

        {-# INLINE readVkAspectReferenceCount #-}
        readVkAspectReferenceCount p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}

        {-# INLINE writeVkAspectReferenceCount #-}
        writeVkAspectReferenceCount p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}

instance {-# OVERLAPPING #-}
         HasField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type FieldType "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = Word32
        type FieldOptional "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}
        type FieldIsArray "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}

instance CanReadField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkAspectReferenceCount

        {-# INLINE readField #-}
        readField = readVkAspectReferenceCount

instance CanWriteField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkAspectReferenceCount

instance {-# OVERLAPPING #-}
         HasVkPAspectReferences
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type VkPAspectReferencesMType
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = Ptr VkInputAttachmentAspectReferenceKHR

        {-# NOINLINE vkPAspectReferences #-}
        vkPAspectReferences x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences})

        {-# INLINE vkPAspectReferencesByteOffset #-}
        vkPAspectReferencesByteOffset ~_
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}

        {-# INLINE readVkPAspectReferences #-}
        readVkPAspectReferences p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}

        {-# INLINE writeVkPAspectReferences #-}
        writeVkPAspectReferences p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}

instance {-# OVERLAPPING #-}
         HasField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        type FieldType "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = Ptr VkInputAttachmentAspectReferenceKHR
        type FieldOptional "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}
        type FieldIsArray "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}

instance CanReadField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPAspectReferences

        {-# INLINE readField #-}
        readField = readVkPAspectReferences

instance CanWriteField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPAspectReferences

instance Show VkRenderPassInputAttachmentAspectCreateInfoKHR where
        showsPrec d x
          = showString "VkRenderPassInputAttachmentAspectCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAspectReferenceCount = " .
                            showsPrec d (vkAspectReferenceCount x) .
                              showString ", " .
                                showString "vkPAspectReferences = " .
                                  showsPrec d (vkPAspectReferences x) . showChar '}'
