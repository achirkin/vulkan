#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAttachmentSampleLocationsEXT
       (VkAttachmentSampleLocationsEXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkSampleLocationsInfoEXT (VkSampleLocationsInfoEXT)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkAttachmentSampleLocationsEXT {
--   >     uint32_t                         attachmentIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkAttachmentSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkAttachmentSampleLocationsEXT.html VkAttachmentSampleLocationsEXT registry at www.khronos.org>
data VkAttachmentSampleLocationsEXT = VkAttachmentSampleLocationsEXT## Addr##
                                                                      ByteArray##

instance Eq VkAttachmentSampleLocationsEXT where
        (VkAttachmentSampleLocationsEXT## a _) ==
          x@(VkAttachmentSampleLocationsEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAttachmentSampleLocationsEXT where
        (VkAttachmentSampleLocationsEXT## a _) `compare`
          x@(VkAttachmentSampleLocationsEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAttachmentSampleLocationsEXT where
        sizeOf ~_ = #{size VkAttachmentSampleLocationsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAttachmentSampleLocationsEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAttachmentSampleLocationsEXT where
        unsafeAddr (VkAttachmentSampleLocationsEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAttachmentSampleLocationsEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAttachmentSampleLocationsEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAttachmentSampleLocationsEXT where
        type StructFields VkAttachmentSampleLocationsEXT =
             '["attachmentIndex", "sampleLocationsInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkAttachmentSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAttachmentSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAttachmentSampleLocationsEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkAttachmentIndex VkAttachmentSampleLocationsEXT where
        type VkAttachmentIndexMType VkAttachmentSampleLocationsEXT = Word32

        {-# NOINLINE vkAttachmentIndex #-}
        vkAttachmentIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentSampleLocationsEXT, attachmentIndex})

        {-# INLINE vkAttachmentIndexByteOffset #-}
        vkAttachmentIndexByteOffset ~_
          = #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

        {-# INLINE readVkAttachmentIndex #-}
        readVkAttachmentIndex p
          = peekByteOff p #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

        {-# INLINE writeVkAttachmentIndex #-}
        writeVkAttachmentIndex p
          = pokeByteOff p #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

instance {-# OVERLAPPING #-}
         HasField "attachmentIndex" VkAttachmentSampleLocationsEXT where
        type FieldType "attachmentIndex" VkAttachmentSampleLocationsEXT =
             Word32
        type FieldOptional "attachmentIndex" VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "attachmentIndex" VkAttachmentSampleLocationsEXT =
             #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}
        type FieldIsArray "attachmentIndex" VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

instance CanReadField "attachmentIndex"
           VkAttachmentSampleLocationsEXT
         where
        {-# INLINE getField #-}
        getField = vkAttachmentIndex

        {-# INLINE readField #-}
        readField = readVkAttachmentIndex

instance CanWriteField "attachmentIndex"
           VkAttachmentSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAttachmentIndex

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsInfo VkAttachmentSampleLocationsEXT where
        type VkSampleLocationsInfoMType VkAttachmentSampleLocationsEXT =
             VkSampleLocationsInfoEXT

        {-# NOINLINE vkSampleLocationsInfo #-}
        vkSampleLocationsInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo})

        {-# INLINE vkSampleLocationsInfoByteOffset #-}
        vkSampleLocationsInfoByteOffset ~_
          = #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE readVkSampleLocationsInfo #-}
        readVkSampleLocationsInfo p
          = peekByteOff p #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE writeVkSampleLocationsInfo #-}
        writeVkSampleLocationsInfo p
          = pokeByteOff p #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsInfo" VkAttachmentSampleLocationsEXT where
        type FieldType "sampleLocationsInfo" VkAttachmentSampleLocationsEXT
             = VkSampleLocationsInfoEXT
        type FieldOptional "sampleLocationsInfo"
               VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsInfo"
               VkAttachmentSampleLocationsEXT
             =
             #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}
        type FieldIsArray "sampleLocationsInfo"
               VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

instance CanReadField "sampleLocationsInfo"
           VkAttachmentSampleLocationsEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationsInfo

        {-# INLINE readField #-}
        readField = readVkSampleLocationsInfo

instance CanWriteField "sampleLocationsInfo"
           VkAttachmentSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationsInfo

instance Show VkAttachmentSampleLocationsEXT where
        showsPrec d x
          = showString "VkAttachmentSampleLocationsEXT {" .
              showString "vkAttachmentIndex = " .
                showsPrec d (vkAttachmentIndex x) .
                  showString ", " .
                    showString "vkSampleLocationsInfo = " .
                      showsPrec d (vkSampleLocationsInfo x) . showChar '}'
