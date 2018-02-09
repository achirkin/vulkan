#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_descriptor_update_template
       (-- * Vulkan extension: @VK_KHR_descriptor_update_template@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Markus Tavenrath @mtavenrath@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @86@
        VkDescriptorUpdateTemplateEntryKHR(..),
        VkDescriptorUpdateTemplateCreateInfoKHR(..),
        vkCreateDescriptorUpdateTemplateKHR,
        vkDestroyDescriptorUpdateTemplateKHR,
        vkUpdateDescriptorSetWithTemplateKHR,
        vkCmdPushDescriptorSetWithTemplateKHR,
        VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION,
        pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION,
        VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME,
        pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT,
        pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorUpdateTemplateEntryKHR {
--   >     uint32_t                         dstBinding;
--   >     uint32_t                         dstArrayElement;
--   >     uint32_t                         descriptorCount;
--   >     VkDescriptorType                 descriptorType;
--   >     size_t                           offset;
--   >     size_t                           stride;
--   > } VkDescriptorUpdateTemplateEntryKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorUpdateTemplateEntryKHR.html VkDescriptorUpdateTemplateEntryKHR registry at www.khronos.org>
data VkDescriptorUpdateTemplateEntryKHR = VkDescriptorUpdateTemplateEntryKHR## ByteArray##

instance Eq VkDescriptorUpdateTemplateEntryKHR where
        (VkDescriptorUpdateTemplateEntryKHR## a) ==
          (VkDescriptorUpdateTemplateEntryKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorUpdateTemplateEntryKHR where
        (VkDescriptorUpdateTemplateEntryKHR## a) `compare`
          (VkDescriptorUpdateTemplateEntryKHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDescriptorUpdateTemplateEntryKHR where
        sizeOf ~_ = #{size VkDescriptorUpdateTemplateEntryKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorUpdateTemplateEntryKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDescriptorUpdateTemplateEntryKHR),
            I## a <- alignment (undefined :: VkDescriptorUpdateTemplateEntryKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDescriptorUpdateTemplateEntryKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDescriptorUpdateTemplateEntryKHR## ba)
          | I## n <- sizeOf (undefined :: VkDescriptorUpdateTemplateEntryKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDescriptorUpdateTemplateEntryKHR where
        type StructFields VkDescriptorUpdateTemplateEntryKHR =
             '["dstBinding", "dstArrayElement", "descriptorCount", -- ' closing tick for hsc2hs
               "descriptorType", "offset", "stride"]

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDescriptorUpdateTemplateEntryKHR),
            I## a <- alignment (undefined :: VkDescriptorUpdateTemplateEntryKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDescriptorUpdateTemplateEntryKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDescriptorUpdateTemplateEntryKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDescriptorUpdateTemplateEntryKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDescriptorUpdateTemplateEntryKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDescriptorUpdateTemplateEntryKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDescriptorUpdateTemplateEntryKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkDstBinding VkDescriptorUpdateTemplateEntryKHR where
        type VkDstBindingMType VkDescriptorUpdateTemplateEntryKHR = Word32

        {-# NOINLINE vkDstBinding #-}
        vkDstBinding x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding})

        {-# INLINE vkDstBindingByteOffset #-}
        vkDstBindingByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

        {-# INLINE readVkDstBinding #-}
        readVkDstBinding p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

        {-# INLINE writeVkDstBinding #-}
        writeVkDstBinding p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

instance {-# OVERLAPPING #-}
         HasField "dstBinding" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "dstBinding" VkDescriptorUpdateTemplateEntryKHR =
             Word32
        type FieldOptional "dstBinding" VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstBinding" VkDescriptorUpdateTemplateEntryKHR =
             #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, dstBinding}

instance CanReadField "dstBinding"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkDstBinding

        {-# INLINE readField #-}
        readField = readVkDstBinding

instance CanWriteField "dstBinding"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDstBinding

instance {-# OVERLAPPING #-}
         HasVkDstArrayElement VkDescriptorUpdateTemplateEntryKHR where
        type VkDstArrayElementMType VkDescriptorUpdateTemplateEntryKHR =
             Word32

        {-# NOINLINE vkDstArrayElement #-}
        vkDstArrayElement x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement})

        {-# INLINE vkDstArrayElementByteOffset #-}
        vkDstArrayElementByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

        {-# INLINE readVkDstArrayElement #-}
        readVkDstArrayElement p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

        {-# INLINE writeVkDstArrayElement #-}
        writeVkDstArrayElement p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

instance {-# OVERLAPPING #-}
         HasField "dstArrayElement" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "dstArrayElement" VkDescriptorUpdateTemplateEntryKHR
             = Word32
        type FieldOptional "dstArrayElement"
               VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstArrayElement"
               VkDescriptorUpdateTemplateEntryKHR
             =
             #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, dstArrayElement}

instance CanReadField "dstArrayElement"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkDstArrayElement

        {-# INLINE readField #-}
        readField = readVkDstArrayElement

instance CanWriteField "dstArrayElement"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDstArrayElement

instance {-# OVERLAPPING #-}
         HasVkDescriptorCount VkDescriptorUpdateTemplateEntryKHR where
        type VkDescriptorCountMType VkDescriptorUpdateTemplateEntryKHR =
             Word32

        {-# NOINLINE vkDescriptorCount #-}
        vkDescriptorCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount})

        {-# INLINE vkDescriptorCountByteOffset #-}
        vkDescriptorCountByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

        {-# INLINE readVkDescriptorCount #-}
        readVkDescriptorCount p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

        {-# INLINE writeVkDescriptorCount #-}
        writeVkDescriptorCount p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

instance {-# OVERLAPPING #-}
         HasField "descriptorCount" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "descriptorCount" VkDescriptorUpdateTemplateEntryKHR
             = Word32
        type FieldOptional "descriptorCount"
               VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorCount"
               VkDescriptorUpdateTemplateEntryKHR
             =
             #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorCount}

instance CanReadField "descriptorCount"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkDescriptorCount

        {-# INLINE readField #-}
        readField = readVkDescriptorCount

instance CanWriteField "descriptorCount"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorCount

instance {-# OVERLAPPING #-}
         HasVkDescriptorType VkDescriptorUpdateTemplateEntryKHR where
        type VkDescriptorTypeMType VkDescriptorUpdateTemplateEntryKHR =
             VkDescriptorType

        {-# NOINLINE vkDescriptorType #-}
        vkDescriptorType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType})

        {-# INLINE vkDescriptorTypeByteOffset #-}
        vkDescriptorTypeByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

        {-# INLINE readVkDescriptorType #-}
        readVkDescriptorType p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

        {-# INLINE writeVkDescriptorType #-}
        writeVkDescriptorType p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

instance {-# OVERLAPPING #-}
         HasField "descriptorType" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "descriptorType" VkDescriptorUpdateTemplateEntryKHR
             = VkDescriptorType
        type FieldOptional "descriptorType"
               VkDescriptorUpdateTemplateEntryKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorType"
               VkDescriptorUpdateTemplateEntryKHR
             =
             #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, descriptorType}

instance CanReadField "descriptorType"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkDescriptorType

        {-# INLINE readField #-}
        readField = readVkDescriptorType

instance CanWriteField "descriptorType"
           VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorType

instance {-# OVERLAPPING #-}
         HasVkOffset VkDescriptorUpdateTemplateEntryKHR where
        type VkOffsetMType VkDescriptorUpdateTemplateEntryKHR =
             #{type size_t}

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

instance {-# OVERLAPPING #-}
         HasField "offset" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "offset" VkDescriptorUpdateTemplateEntryKHR =
             #{type size_t}
        type FieldOptional "offset" VkDescriptorUpdateTemplateEntryKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkDescriptorUpdateTemplateEntryKHR =
             #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, offset}

instance CanReadField "offset" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkOffset

        {-# INLINE readField #-}
        readField = readVkOffset

instance CanWriteField "offset" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkOffset

instance {-# OVERLAPPING #-}
         HasVkStride VkDescriptorUpdateTemplateEntryKHR where
        type VkStrideMType VkDescriptorUpdateTemplateEntryKHR =
             #{type size_t}

        {-# NOINLINE vkStride #-}
        vkStride x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateEntryKHR, stride})

        {-# INLINE vkStrideByteOffset #-}
        vkStrideByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

        {-# INLINE readVkStride #-}
        readVkStride p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

        {-# INLINE writeVkStride #-}
        writeVkStride p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

instance {-# OVERLAPPING #-}
         HasField "stride" VkDescriptorUpdateTemplateEntryKHR where
        type FieldType "stride" VkDescriptorUpdateTemplateEntryKHR =
             #{type size_t}
        type FieldOptional "stride" VkDescriptorUpdateTemplateEntryKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stride" VkDescriptorUpdateTemplateEntryKHR =
             #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateEntryKHR, stride}

instance CanReadField "stride" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE getField #-}
        getField = vkStride

        {-# INLINE readField #-}
        readField = readVkStride

instance CanWriteField "stride" VkDescriptorUpdateTemplateEntryKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkStride

instance Show VkDescriptorUpdateTemplateEntryKHR where
        showsPrec d x
          = showString "VkDescriptorUpdateTemplateEntryKHR {" .
              showString "vkDstBinding = " .
                showsPrec d (vkDstBinding x) .
                  showString ", " .
                    showString "vkDstArrayElement = " .
                      showsPrec d (vkDstArrayElement x) .
                        showString ", " .
                          showString "vkDescriptorCount = " .
                            showsPrec d (vkDescriptorCount x) .
                              showString ", " .
                                showString "vkDescriptorType = " .
                                  showsPrec d (vkDescriptorType x) .
                                    showString ", " .
                                      showString "vkOffset = " .
                                        showsPrec d (vkOffset x) .
                                          showString ", " .
                                            showString "vkStride = " .
                                              showsPrec d (vkStride x) . showChar '}'

-- | > typedef struct VkDescriptorUpdateTemplateCreateInfoKHR {
--   >     VkStructureType sType;
--   >     void*                                   pNext;
--   >     VkDescriptorUpdateTemplateCreateFlagsKHR    flags;
--   >     uint32_t                 descriptorUpdateEntryCount;
--   >     const VkDescriptorUpdateTemplateEntryKHR* pDescriptorUpdateEntries;
--   >     VkDescriptorUpdateTemplateTypeKHR templateType;
--   >     VkDescriptorSetLayout descriptorSetLayout;
--   >     VkPipelineBindPoint pipelineBindPoint;
--   >     VkPipelineLayoutpipelineLayout;
--   >     uint32_t set;
--   > } VkDescriptorUpdateTemplateCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDescriptorUpdateTemplateCreateInfoKHR.html VkDescriptorUpdateTemplateCreateInfoKHR registry at www.khronos.org>
data VkDescriptorUpdateTemplateCreateInfoKHR = VkDescriptorUpdateTemplateCreateInfoKHR## ByteArray##

instance Eq VkDescriptorUpdateTemplateCreateInfoKHR where
        (VkDescriptorUpdateTemplateCreateInfoKHR## a) ==
          (VkDescriptorUpdateTemplateCreateInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorUpdateTemplateCreateInfoKHR where
        (VkDescriptorUpdateTemplateCreateInfoKHR## a) `compare`
          (VkDescriptorUpdateTemplateCreateInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDescriptorUpdateTemplateCreateInfoKHR where
        sizeOf ~_
          = #{size VkDescriptorUpdateTemplateCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorUpdateTemplateCreateInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkDescriptorUpdateTemplateCreateInfoKHR),
            I## a <- alignment
                      (undefined :: VkDescriptorUpdateTemplateCreateInfoKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDescriptorUpdateTemplateCreateInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDescriptorUpdateTemplateCreateInfoKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkDescriptorUpdateTemplateCreateInfoKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type StructFields VkDescriptorUpdateTemplateCreateInfoKHR =
             '["sType", "pNext", "flags", "descriptorUpdateEntryCount", -- ' closing tick for hsc2hs
               "pDescriptorUpdateEntries", "templateType", "descriptorSetLayout",
               "pipelineBindPoint", "pipelineLayout", "set"]

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkDescriptorUpdateTemplateCreateInfoKHR),
            I## a <- alignment
                      (undefined :: VkDescriptorUpdateTemplateCreateInfoKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDescriptorUpdateTemplateCreateInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDescriptorUpdateTemplateCreateInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDescriptorUpdateTemplateCreateInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDescriptorUpdateTemplateCreateInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDescriptorUpdateTemplateCreateInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDescriptorUpdateTemplateCreateInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkSTypeMType VkDescriptorUpdateTemplateCreateInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "sType" VkDescriptorUpdateTemplateCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

instance CanReadField "sType"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkPNextMType VkDescriptorUpdateTemplateCreateInfoKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "pNext" VkDescriptorUpdateTemplateCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

instance CanReadField "pNext"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkFlagsMType VkDescriptorUpdateTemplateCreateInfoKHR =
             VkDescriptorUpdateTemplateCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "flags" VkDescriptorUpdateTemplateCreateInfoKHR =
             VkDescriptorUpdateTemplateCreateFlagsKHR
        type FieldOptional "flags" VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

instance CanReadField "flags"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkDescriptorUpdateEntryCount
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type VkDescriptorUpdateEntryCountMType
               VkDescriptorUpdateTemplateCreateInfoKHR
             = Word32

        {-# NOINLINE vkDescriptorUpdateEntryCount #-}
        vkDescriptorUpdateEntryCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount})

        {-# INLINE vkDescriptorUpdateEntryCountByteOffset #-}
        vkDescriptorUpdateEntryCountByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

        {-# INLINE readVkDescriptorUpdateEntryCount #-}
        readVkDescriptorUpdateEntryCount p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

        {-# INLINE writeVkDescriptorUpdateEntryCount #-}
        writeVkDescriptorUpdateEntryCount p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         HasField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = Word32
        type FieldOptional "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

instance CanReadField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkDescriptorUpdateEntryCount

        {-# INLINE readField #-}
        readField = readVkDescriptorUpdateEntryCount

instance CanWriteField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorUpdateEntryCount

instance {-# OVERLAPPING #-}
         HasVkPDescriptorUpdateEntries
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type VkPDescriptorUpdateEntriesMType
               VkDescriptorUpdateTemplateCreateInfoKHR
             = Ptr VkDescriptorUpdateTemplateEntryKHR

        {-# NOINLINE vkPDescriptorUpdateEntries #-}
        vkPDescriptorUpdateEntries x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries})

        {-# INLINE vkPDescriptorUpdateEntriesByteOffset #-}
        vkPDescriptorUpdateEntriesByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

        {-# INLINE readVkPDescriptorUpdateEntries #-}
        readVkPDescriptorUpdateEntries p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

        {-# INLINE writeVkPDescriptorUpdateEntries #-}
        writeVkPDescriptorUpdateEntries p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         HasField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = Ptr VkDescriptorUpdateTemplateEntryKHR
        type FieldOptional "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

instance CanReadField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPDescriptorUpdateEntries

        {-# INLINE readField #-}
        readField = readVkPDescriptorUpdateEntries

instance CanWriteField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDescriptorUpdateEntries

instance {-# OVERLAPPING #-}
         HasVkTemplateType VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkTemplateTypeMType VkDescriptorUpdateTemplateCreateInfoKHR =
             VkDescriptorUpdateTemplateTypeKHR

        {-# NOINLINE vkTemplateType #-}
        vkTemplateType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType})

        {-# INLINE vkTemplateTypeByteOffset #-}
        vkTemplateTypeByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

        {-# INLINE readVkTemplateType #-}
        readVkTemplateType p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

        {-# INLINE writeVkTemplateType #-}
        writeVkTemplateType p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

instance {-# OVERLAPPING #-}
         HasField "templateType" VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkDescriptorUpdateTemplateTypeKHR
        type FieldOptional "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

instance CanReadField "templateType"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkTemplateType

        {-# INLINE readField #-}
        readField = readVkTemplateType

instance CanWriteField "templateType"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkTemplateType

instance {-# OVERLAPPING #-}
         HasVkDescriptorSetLayout VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type VkDescriptorSetLayoutMType
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkDescriptorSetLayout

        {-# NOINLINE vkDescriptorSetLayout #-}
        vkDescriptorSetLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout})

        {-# INLINE vkDescriptorSetLayoutByteOffset #-}
        vkDescriptorSetLayoutByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

        {-# INLINE readVkDescriptorSetLayout #-}
        readVkDescriptorSetLayout p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

        {-# INLINE writeVkDescriptorSetLayout #-}
        writeVkDescriptorSetLayout p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         HasField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkDescriptorSetLayout
        type FieldOptional "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

instance CanReadField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkDescriptorSetLayout

        {-# INLINE readField #-}
        readField = readVkDescriptorSetLayout

instance CanWriteField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDescriptorSetLayout

instance {-# OVERLAPPING #-}
         HasVkPipelineBindPoint VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type VkPipelineBindPointMType
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkPipelineBindPoint

        {-# NOINLINE vkPipelineBindPoint #-}
        vkPipelineBindPoint x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint})

        {-# INLINE vkPipelineBindPointByteOffset #-}
        vkPipelineBindPointByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

        {-# INLINE readVkPipelineBindPoint #-}
        readVkPipelineBindPoint p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

        {-# INLINE writeVkPipelineBindPoint #-}
        writeVkPipelineBindPoint p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         HasField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkPipelineBindPoint
        type FieldOptional "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

instance CanReadField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPipelineBindPoint

        {-# INLINE readField #-}
        readField = readVkPipelineBindPoint

instance CanWriteField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPipelineBindPoint

instance {-# OVERLAPPING #-}
         HasVkPipelineLayout VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkPipelineLayoutMType VkDescriptorUpdateTemplateCreateInfoKHR
             = VkPipelineLayout

        {-# NOINLINE vkPipelineLayout #-}
        vkPipelineLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout})

        {-# INLINE vkPipelineLayoutByteOffset #-}
        vkPipelineLayoutByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

        {-# INLINE readVkPipelineLayout #-}
        readVkPipelineLayout p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

        {-# INLINE writeVkPipelineLayout #-}
        writeVkPipelineLayout p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

instance {-# OVERLAPPING #-}
         HasField "pipelineLayout" VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkPipelineLayout
        type FieldOptional "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

instance CanReadField "pipelineLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPipelineLayout

        {-# INLINE readField #-}
        readField = readVkPipelineLayout

instance CanWriteField "pipelineLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPipelineLayout

instance {-# OVERLAPPING #-}
         HasVkSet VkDescriptorUpdateTemplateCreateInfoKHR where
        type VkSetMType VkDescriptorUpdateTemplateCreateInfoKHR = Word32

        {-# NOINLINE vkSet #-}
        vkSet x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set})

        {-# INLINE vkSetByteOffset #-}
        vkSetByteOffset ~_
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

        {-# INLINE readVkSet #-}
        readVkSet p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

        {-# INLINE writeVkSet #-}
        writeVkSet p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

instance {-# OVERLAPPING #-}
         HasField "set" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             Word32
        type FieldOptional "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

instance CanReadField "set" VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSet

        {-# INLINE readField #-}
        readField = readVkSet

instance CanWriteField "set"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSet

instance Show VkDescriptorUpdateTemplateCreateInfoKHR where
        showsPrec d x
          = showString "VkDescriptorUpdateTemplateCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDescriptorUpdateEntryCount = " .
                                  showsPrec d (vkDescriptorUpdateEntryCount x) .
                                    showString ", " .
                                      showString "vkPDescriptorUpdateEntries = " .
                                        showsPrec d (vkPDescriptorUpdateEntries x) .
                                          showString ", " .
                                            showString "vkTemplateType = " .
                                              showsPrec d (vkTemplateType x) .
                                                showString ", " .
                                                  showString "vkDescriptorSetLayout = " .
                                                    showsPrec d (vkDescriptorSetLayout x) .
                                                      showString ", " .
                                                        showString "vkPipelineBindPoint = " .
                                                          showsPrec d (vkPipelineBindPoint x) .
                                                            showString ", " .
                                                              showString "vkPipelineLayout = " .
                                                                showsPrec d (vkPipelineLayout x) .
                                                                  showString ", " .
                                                                    showString "vkSet = " .
                                                                      showsPrec d (vkSet x) .
                                                                        showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , const VkDescriptorUpdateTemplateCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDescriptorUpdateTemplateKHR* pDescriptorUpdateTemplate
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDescriptorUpdateTemplateKHR.html vkCreateDescriptorUpdateTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDescriptorUpdateTemplateKHR"
               vkCreateDescriptorUpdateTemplateKHR ::
               VkDevice -- ^ device
                        ->
                 Ptr VkDescriptorUpdateTemplateCreateInfoKHR -- ^ pCreateInfo
                                                             ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDescriptorUpdateTemplateKHR -- ^ pDescriptorUpdateTemplate
                                                       -> IO VkResult

-- | > void vkDestroyDescriptorUpdateTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorUpdateTemplateKHR descriptorUpdateTemplate
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyDescriptorUpdateTemplateKHR.html vkDestroyDescriptorUpdateTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDescriptorUpdateTemplateKHR"
               vkDestroyDescriptorUpdateTemplateKHR ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorUpdateTemplateKHR -- ^ descriptorUpdateTemplate
                                               -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                            -> IO ()

-- | > void vkUpdateDescriptorSetWithTemplateKHR
--   >     ( VkDevice device
--   >     , VkDescriptorSet descriptorSet
--   >     , VkDescriptorUpdateTemplateKHR descriptorUpdateTemplate
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkUpdateDescriptorSetWithTemplateKHR.html vkUpdateDescriptorSetWithTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkUpdateDescriptorSetWithTemplateKHR"
               vkUpdateDescriptorSetWithTemplateKHR ::
               VkDevice -- ^ device
                        ->
                 VkDescriptorSet -- ^ descriptorSet
                                 ->
                   VkDescriptorUpdateTemplateKHR -- ^ descriptorUpdateTemplate
                                                 -> Ptr Void -- ^ pData
                                                             -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @both@
--
--   > void vkCmdPushDescriptorSetWithTemplateKHR
--   >     ( VkCommandBuffer commandBuffer
--   >     , VkDescriptorUpdateTemplateKHR descriptorUpdateTemplate
--   >     , VkPipelineLayout layout
--   >     , uint32_t set
--   >     , const void* pData
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdPushDescriptorSetWithTemplateKHR.html vkCmdPushDescriptorSetWithTemplateKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCmdPushDescriptorSetWithTemplateKHR"
               vkCmdPushDescriptorSetWithTemplateKHR ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 VkDescriptorUpdateTemplateKHR -- ^ descriptorUpdateTemplate
                                               ->
                   VkPipelineLayout -- ^ layout
                                    -> Word32 -- ^ set
                                              -> Ptr Void -- ^ pData
                                                          -> IO ()

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1

type VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: CString

pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME <-
        (is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME -> True)
  where VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
          = _VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME

_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME #-}
_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  = Ptr "VK_KHR_descriptor_update_template\NUL"##

is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME ::
                                                    CString -> Bool

{-# INLINE is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME #-}
is_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  = (_VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME ==)

type VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME =
     "VK_KHR_descriptor_update_template"

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
        = VkStructureType 1000085000

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
        :: VkDebugReportObjectTypeEXT

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
        = VkDebugReportObjectTypeEXT 1000085000

-- | VkDescriptorUpdateTemplateKHR
pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR ::
        VkObjectType

pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR =
        VkObjectType 1000085000
