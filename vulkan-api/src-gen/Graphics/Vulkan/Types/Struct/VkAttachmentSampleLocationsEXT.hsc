#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAttachmentSampleLocationsEXT
       (VkAttachmentSampleLocationsEXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkSampleLocationsInfoEXT (VkSampleLocationsInfoEXT)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkAttachmentSampleLocationsEXT {
--   >     uint32_t                         attachmentIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkAttachmentSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAttachmentSampleLocationsEXT VkAttachmentSampleLocationsEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "attachmentIndex" VkAttachmentSampleLocationsEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentSampleLocationsEXT, attachmentIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "attachmentIndex" VkAttachmentSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

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

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationsInfo" VkAttachmentSampleLocationsEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationsInfo" VkAttachmentSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

instance Show VkAttachmentSampleLocationsEXT where
        showsPrec d x
          = showString "VkAttachmentSampleLocationsEXT {" .
              showString "attachmentIndex = " .
                showsPrec d (getField @"attachmentIndex" x) .
                  showString ", " .
                    showString "sampleLocationsInfo = " .
                      showsPrec d (getField @"sampleLocationsInfo" x) . showChar '}'
