#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.FramebufferCreateInfo
       (VkFramebufferCreateInfo(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkFramebufferCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkImageView,
                                                           VkRenderPass)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkFramebufferCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkFramebufferCreateFlags    flags;
--   >     VkRenderPass           renderPass;
--   >     uint32_t               attachmentCount;
--   >     const VkImageView*     pAttachments;
--   >     uint32_t               width;
--   >     uint32_t               height;
--   >     uint32_t               layers;
--   > } VkFramebufferCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFramebufferCreateInfo VkFramebufferCreateInfo registry at www.khronos.org>
data VkFramebufferCreateInfo = VkFramebufferCreateInfo## Addr##
                                                        ByteArray##

instance Eq VkFramebufferCreateInfo where
        (VkFramebufferCreateInfo## a _) == x@(VkFramebufferCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFramebufferCreateInfo where
        (VkFramebufferCreateInfo## a _) `compare`
          x@(VkFramebufferCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFramebufferCreateInfo where
        sizeOf ~_ = #{size VkFramebufferCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFramebufferCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFramebufferCreateInfo where
        unsafeAddr (VkFramebufferCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFramebufferCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFramebufferCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFramebufferCreateInfo where
        type StructFields VkFramebufferCreateInfo =
             '["sType", "pNext", "flags", "renderPass", "attachmentCount", -- ' closing tick for hsc2hs
               "pAttachments", "width", "height", "layers"]
        type CUnionType VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkFramebufferCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkFramebufferCreateInfo where
        type FieldType "sType" VkFramebufferCreateInfo = VkStructureType
        type FieldOptional "sType" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, sType}
        type FieldIsArray "sType" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkFramebufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFramebufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkFramebufferCreateInfo where
        type FieldType "pNext" VkFramebufferCreateInfo = Ptr Void
        type FieldOptional "pNext" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, pNext}
        type FieldIsArray "pNext" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkFramebufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFramebufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkFramebufferCreateInfo where
        type FieldType "flags" VkFramebufferCreateInfo =
             VkFramebufferCreateFlags
        type FieldOptional "flags" VkFramebufferCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, flags}
        type FieldIsArray "flags" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkFramebufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFramebufferCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "renderPass" VkFramebufferCreateInfo where
        type FieldType "renderPass" VkFramebufferCreateInfo = VkRenderPass
        type FieldOptional "renderPass" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "renderPass" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, renderPass}
        type FieldIsArray "renderPass" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFramebufferCreateInfo, renderPass}

instance {-# OVERLAPPING #-}
         CanReadField "renderPass" VkFramebufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, renderPass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFramebufferCreateInfo, renderPass}

instance {-# OVERLAPPING #-}
         CanWriteField "renderPass" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, renderPass}

instance {-# OVERLAPPING #-}
         HasField "attachmentCount" VkFramebufferCreateInfo where
        type FieldType "attachmentCount" VkFramebufferCreateInfo = Word32
        type FieldOptional "attachmentCount" VkFramebufferCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "attachmentCount" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, attachmentCount}
        type FieldIsArray "attachmentCount" VkFramebufferCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFramebufferCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         CanReadField "attachmentCount" VkFramebufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, attachmentCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFramebufferCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         CanWriteField "attachmentCount" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         HasField "pAttachments" VkFramebufferCreateInfo where
        type FieldType "pAttachments" VkFramebufferCreateInfo =
             Ptr VkImageView
        type FieldOptional "pAttachments" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAttachments" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, pAttachments}
        type FieldIsArray "pAttachments" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFramebufferCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         CanReadField "pAttachments" VkFramebufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, pAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFramebufferCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttachments" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         HasField "width" VkFramebufferCreateInfo where
        type FieldType "width" VkFramebufferCreateInfo = Word32
        type FieldOptional "width" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "width" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, width}
        type FieldIsArray "width" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, width}

instance {-# OVERLAPPING #-}
         CanReadField "width" VkFramebufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, width})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFramebufferCreateInfo, width}

instance {-# OVERLAPPING #-}
         CanWriteField "width" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, width}

instance {-# OVERLAPPING #-}
         HasField "height" VkFramebufferCreateInfo where
        type FieldType "height" VkFramebufferCreateInfo = Word32
        type FieldOptional "height" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "height" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, height}
        type FieldIsArray "height" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, height}

instance {-# OVERLAPPING #-}
         CanReadField "height" VkFramebufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, height})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFramebufferCreateInfo, height}

instance {-# OVERLAPPING #-}
         CanWriteField "height" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, height}

instance {-# OVERLAPPING #-}
         HasField "layers" VkFramebufferCreateInfo where
        type FieldType "layers" VkFramebufferCreateInfo = Word32
        type FieldOptional "layers" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layers" VkFramebufferCreateInfo =
             #{offset VkFramebufferCreateInfo, layers}
        type FieldIsArray "layers" VkFramebufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFramebufferCreateInfo, layers}

instance {-# OVERLAPPING #-}
         CanReadField "layers" VkFramebufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFramebufferCreateInfo, layers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFramebufferCreateInfo, layers}

instance {-# OVERLAPPING #-}
         CanWriteField "layers" VkFramebufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFramebufferCreateInfo, layers}

instance Show VkFramebufferCreateInfo where
        showsPrec d x
          = showString "VkFramebufferCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "renderPass = " .
                                  showsPrec d (getField @"renderPass" x) .
                                    showString ", " .
                                      showString "attachmentCount = " .
                                        showsPrec d (getField @"attachmentCount" x) .
                                          showString ", " .
                                            showString "pAttachments = " .
                                              showsPrec d (getField @"pAttachments" x) .
                                                showString ", " .
                                                  showString "width = " .
                                                    showsPrec d (getField @"width" x) .
                                                      showString ", " .
                                                        showString "height = " .
                                                          showsPrec d (getField @"height" x) .
                                                            showString ", " .
                                                              showString "layers = " .
                                                                showsPrec d (getField @"layers" x) .
                                                                  showChar '}'
