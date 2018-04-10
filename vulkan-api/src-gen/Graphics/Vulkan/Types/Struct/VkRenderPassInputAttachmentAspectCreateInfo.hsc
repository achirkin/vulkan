#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRenderPassInputAttachmentAspectCreateInfo
       (VkRenderPassInputAttachmentAspectCreateInfo(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Base                                                      (Addr##,
                                                                                ByteArray##,
                                                                                byteArrayContents##,
                                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkInputAttachmentAspectReference (VkInputAttachmentAspectReference)
import           Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo           (VkRenderPassCreateInfo)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkRenderPassInputAttachmentAspectCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                     pNext;
--   >     uint32_t                        aspectReferenceCount;
--   >     const VkInputAttachmentAspectReference* pAspectReferences;
--   > } VkRenderPassInputAttachmentAspectCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkRenderPassInputAttachmentAspectCreateInfo VkRenderPassInputAttachmentAspectCreateInfo registry at www.khronos.org>
data VkRenderPassInputAttachmentAspectCreateInfo = VkRenderPassInputAttachmentAspectCreateInfo## Addr##
                                                                                                ByteArray##

instance Eq VkRenderPassInputAttachmentAspectCreateInfo where
        (VkRenderPassInputAttachmentAspectCreateInfo## a _) ==
          x@(VkRenderPassInputAttachmentAspectCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassInputAttachmentAspectCreateInfo where
        (VkRenderPassInputAttachmentAspectCreateInfo## a _) `compare`
          x@(VkRenderPassInputAttachmentAspectCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassInputAttachmentAspectCreateInfo where
        sizeOf ~_
          = #{size VkRenderPassInputAttachmentAspectCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassInputAttachmentAspectCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkRenderPassInputAttachmentAspectCreateInfo
         where
        unsafeAddr (VkRenderPassInputAttachmentAspectCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassInputAttachmentAspectCreateInfo## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassInputAttachmentAspectCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassInputAttachmentAspectCreateInfo
         where
        type StructFields VkRenderPassInputAttachmentAspectCreateInfo =
             '["sType", "pNext", "aspectReferenceCount", "pAspectReferences"] -- ' closing tick for hsc2hs
        type CUnionType VkRenderPassInputAttachmentAspectCreateInfo =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassInputAttachmentAspectCreateInfo =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassInputAttachmentAspectCreateInfo =
             '[VkRenderPassCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassInputAttachmentAspectCreateInfo where
        type FieldType "sType" VkRenderPassInputAttachmentAspectCreateInfo
             = VkStructureType
        type FieldOptional "sType"
               VkRenderPassInputAttachmentAspectCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkRenderPassInputAttachmentAspectCreateInfo
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfo, sType}
        type FieldIsArray "sType"
               VkRenderPassInputAttachmentAspectCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassInputAttachmentAspectCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkRenderPassInputAttachmentAspectCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassInputAttachmentAspectCreateInfo where
        type FieldType "pNext" VkRenderPassInputAttachmentAspectCreateInfo
             = Ptr Void
        type FieldOptional "pNext"
               VkRenderPassInputAttachmentAspectCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkRenderPassInputAttachmentAspectCreateInfo
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfo, pNext}
        type FieldIsArray "pNext"
               VkRenderPassInputAttachmentAspectCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassInputAttachmentAspectCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkRenderPassInputAttachmentAspectCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfo
         where
        type FieldType "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfo
             = Word32
        type FieldOptional "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfo
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfo, aspectReferenceCount}
        type FieldIsArray "aspectReferenceCount"
               VkRenderPassInputAttachmentAspectCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfo, aspectReferenceCount}

instance {-# OVERLAPPING #-}
         CanReadField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfo, aspectReferenceCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfo, aspectReferenceCount}

instance {-# OVERLAPPING #-}
         CanWriteField "aspectReferenceCount"
           VkRenderPassInputAttachmentAspectCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfo, aspectReferenceCount}

instance {-# OVERLAPPING #-}
         HasField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfo
         where
        type FieldType "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfo
             = Ptr VkInputAttachmentAspectReference
        type FieldOptional "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfo
             =
             #{offset VkRenderPassInputAttachmentAspectCreateInfo, pAspectReferences}
        type FieldIsArray "pAspectReferences"
               VkRenderPassInputAttachmentAspectCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassInputAttachmentAspectCreateInfo, pAspectReferences}

instance {-# OVERLAPPING #-}
         CanReadField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassInputAttachmentAspectCreateInfo, pAspectReferences})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfo, pAspectReferences}

instance {-# OVERLAPPING #-}
         CanWriteField "pAspectReferences"
           VkRenderPassInputAttachmentAspectCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassInputAttachmentAspectCreateInfo, pAspectReferences}

instance Show VkRenderPassInputAttachmentAspectCreateInfo where
        showsPrec d x
          = showString "VkRenderPassInputAttachmentAspectCreateInfo {" .
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
