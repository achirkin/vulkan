#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, aspectReferenceCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}

instance {-# OVERLAPPING #-}
         CanWriteField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfoKHR, pAspectReferences}

instance Show VkRenderPassInputAttachmentAspectCreateInfoKHR where
        showsPrec d x
          = showString "VkRenderPassInputAttachmentAspectCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "aspectReferenceCount = " .
                            showsPrec d (getField @"aspectReferenceCount" x) .
                              showString ", " .
                                showString "pAspectReferences = " .
                                  showsPrec d (getField @"pAspectReferences" x) . showChar '}'
