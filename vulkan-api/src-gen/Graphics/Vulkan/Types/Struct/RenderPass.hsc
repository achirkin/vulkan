#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.RenderPass
       (VkRenderPassBeginInfo(..), VkRenderPassCreateInfo(..),
        VkRenderPassInputAttachmentAspectCreateInfo(..),
        VkRenderPassInputAttachmentAspectCreateInfoKHR,
        VkRenderPassMultiviewCreateInfo(..),
        VkRenderPassMultiviewCreateInfoKHR,
        VkRenderPassSampleLocationsBeginInfoEXT(..))
       where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Base                                                    (Addr##,
                                                                              ByteArray##,
                                                                              byteArrayContents##,
                                                                              plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                              (VkRenderPassCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Handles                               (VkFramebuffer,
                                                                              VkRenderPass)
import           Graphics.Vulkan.Types.Struct.Attachment                     (VkAttachmentDescription,
                                                                              VkAttachmentSampleLocationsEXT)
import           Graphics.Vulkan.Types.Struct.Clear                          (VkClearValue)
import           Graphics.Vulkan.Types.Struct.InputAttachmentAspectReference (VkInputAttachmentAspectReference)
import           Graphics.Vulkan.Types.Struct.Rect                           (VkRect2D)
import           Graphics.Vulkan.Types.Struct.Subpass                        (VkSubpassDependency,
                                                                              VkSubpassDescription,
                                                                              VkSubpassSampleLocationsEXT)
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkRenderPassBeginInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRenderPass           renderPass;
--   >     VkFramebuffer          framebuffer;
--   >     VkRect2D               renderArea;
--   >     uint32_t               clearValueCount;
--   >     const VkClearValue*    pClearValues;
--   > } VkRenderPassBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRenderPassBeginInfo VkRenderPassBeginInfo registry at www.khronos.org>
data VkRenderPassBeginInfo = VkRenderPassBeginInfo## Addr##
                                                    ByteArray##

instance Eq VkRenderPassBeginInfo where
        (VkRenderPassBeginInfo## a _) == x@(VkRenderPassBeginInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassBeginInfo where
        (VkRenderPassBeginInfo## a _) `compare`
          x@(VkRenderPassBeginInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassBeginInfo where
        sizeOf ~_ = #{size VkRenderPassBeginInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkRenderPassBeginInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassBeginInfo where
        unsafeAddr (VkRenderPassBeginInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassBeginInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassBeginInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassBeginInfo where
        type StructFields VkRenderPassBeginInfo =
             '["sType", "pNext", "renderPass", "framebuffer", "renderArea", -- ' closing tick for hsc2hs
               "clearValueCount", "pClearValues"]
        type CUnionType VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassBeginInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkRenderPassBeginInfo
         where
        type FieldType "sType" VkRenderPassBeginInfo = VkStructureType
        type FieldOptional "sType" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, sType}
        type FieldIsArray "sType" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkRenderPassBeginInfo
         where
        type FieldType "pNext" VkRenderPassBeginInfo = Ptr Void
        type FieldOptional "pNext" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, pNext}
        type FieldIsArray "pNext" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "renderPass" VkRenderPassBeginInfo where
        type FieldType "renderPass" VkRenderPassBeginInfo = VkRenderPass
        type FieldOptional "renderPass" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "renderPass" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, renderPass}
        type FieldIsArray "renderPass" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassBeginInfo, renderPass}

instance {-# OVERLAPPING #-}
         CanReadField "renderPass" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, renderPass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, renderPass}

instance {-# OVERLAPPING #-}
         CanWriteField "renderPass" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, renderPass}

instance {-# OVERLAPPING #-}
         HasField "framebuffer" VkRenderPassBeginInfo where
        type FieldType "framebuffer" VkRenderPassBeginInfo = VkFramebuffer
        type FieldOptional "framebuffer" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "framebuffer" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, framebuffer}
        type FieldIsArray "framebuffer" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassBeginInfo, framebuffer}

instance {-# OVERLAPPING #-}
         CanReadField "framebuffer" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, framebuffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, framebuffer}

instance {-# OVERLAPPING #-}
         CanWriteField "framebuffer" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, framebuffer}

instance {-# OVERLAPPING #-}
         HasField "renderArea" VkRenderPassBeginInfo where
        type FieldType "renderArea" VkRenderPassBeginInfo = VkRect2D
        type FieldOptional "renderArea" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "renderArea" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, renderArea}
        type FieldIsArray "renderArea" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassBeginInfo, renderArea}

instance {-# OVERLAPPING #-}
         CanReadField "renderArea" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, renderArea})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, renderArea}

instance {-# OVERLAPPING #-}
         CanWriteField "renderArea" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, renderArea}

instance {-# OVERLAPPING #-}
         HasField "clearValueCount" VkRenderPassBeginInfo where
        type FieldType "clearValueCount" VkRenderPassBeginInfo = Word32
        type FieldOptional "clearValueCount" VkRenderPassBeginInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "clearValueCount" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, clearValueCount}
        type FieldIsArray "clearValueCount" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassBeginInfo, clearValueCount}

instance {-# OVERLAPPING #-}
         CanReadField "clearValueCount" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, clearValueCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, clearValueCount}

instance {-# OVERLAPPING #-}
         CanWriteField "clearValueCount" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, clearValueCount}

instance {-# OVERLAPPING #-}
         HasField "pClearValues" VkRenderPassBeginInfo where
        type FieldType "pClearValues" VkRenderPassBeginInfo =
             Ptr VkClearValue
        type FieldOptional "pClearValues" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pClearValues" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, pClearValues}
        type FieldIsArray "pClearValues" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassBeginInfo, pClearValues}

instance {-# OVERLAPPING #-}
         CanReadField "pClearValues" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, pClearValues})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, pClearValues}

instance {-# OVERLAPPING #-}
         CanWriteField "pClearValues" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, pClearValues}

instance Show VkRenderPassBeginInfo where
        showsPrec d x
          = showString "VkRenderPassBeginInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "renderPass = " .
                            showsPrec d (getField @"renderPass" x) .
                              showString ", " .
                                showString "framebuffer = " .
                                  showsPrec d (getField @"framebuffer" x) .
                                    showString ", " .
                                      showString "renderArea = " .
                                        showsPrec d (getField @"renderArea" x) .
                                          showString ", " .
                                            showString "clearValueCount = " .
                                              showsPrec d (getField @"clearValueCount" x) .
                                                showString ", " .
                                                  showString "pClearValues = " .
                                                    showsPrec d (getField @"pClearValues" x) .
                                                      showChar '}'

-- | > typedef struct VkRenderPassCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRenderPassCreateFlags    flags;
--   >     uint32_t   attachmentCount;
--   >     const VkAttachmentDescription* pAttachments;
--   >     uint32_t               subpassCount;
--   >     const VkSubpassDescription* pSubpasses;
--   >     uint32_t       dependencyCount;
--   >     const VkSubpassDependency* pDependencies;
--   > } VkRenderPassCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRenderPassCreateInfo VkRenderPassCreateInfo registry at www.khronos.org>
data VkRenderPassCreateInfo = VkRenderPassCreateInfo## Addr##
                                                      ByteArray##

instance Eq VkRenderPassCreateInfo where
        (VkRenderPassCreateInfo## a _) == x@(VkRenderPassCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassCreateInfo where
        (VkRenderPassCreateInfo## a _) `compare`
          x@(VkRenderPassCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassCreateInfo where
        sizeOf ~_ = #{size VkRenderPassCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkRenderPassCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassCreateInfo where
        unsafeAddr (VkRenderPassCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassCreateInfo where
        type StructFields VkRenderPassCreateInfo =
             '["sType", "pNext", "flags", "attachmentCount", "pAttachments", -- ' closing tick for hsc2hs
               "subpassCount", "pSubpasses", "dependencyCount", "pDependencies"]
        type CUnionType VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassCreateInfo where
        type FieldType "sType" VkRenderPassCreateInfo = VkStructureType
        type FieldOptional "sType" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, sType}
        type FieldIsArray "sType" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRenderPassCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassCreateInfo where
        type FieldType "pNext" VkRenderPassCreateInfo = Ptr Void
        type FieldOptional "pNext" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, pNext}
        type FieldIsArray "pNext" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRenderPassCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkRenderPassCreateInfo where
        type FieldType "flags" VkRenderPassCreateInfo =
             VkRenderPassCreateFlags
        type FieldOptional "flags" VkRenderPassCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, flags}
        type FieldIsArray "flags" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRenderPassCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "attachmentCount" VkRenderPassCreateInfo where
        type FieldType "attachmentCount" VkRenderPassCreateInfo = Word32
        type FieldOptional "attachmentCount" VkRenderPassCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "attachmentCount" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, attachmentCount}
        type FieldIsArray "attachmentCount" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         CanReadField "attachmentCount" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, attachmentCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         CanWriteField "attachmentCount" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         HasField "pAttachments" VkRenderPassCreateInfo where
        type FieldType "pAttachments" VkRenderPassCreateInfo =
             Ptr VkAttachmentDescription
        type FieldOptional "pAttachments" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAttachments" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, pAttachments}
        type FieldIsArray "pAttachments" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         CanReadField "pAttachments" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttachments" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         HasField "subpassCount" VkRenderPassCreateInfo where
        type FieldType "subpassCount" VkRenderPassCreateInfo = Word32
        type FieldOptional "subpassCount" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subpassCount" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, subpassCount}
        type FieldIsArray "subpassCount" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         CanReadField "subpassCount" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, subpassCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         CanWriteField "subpassCount" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         HasField "pSubpasses" VkRenderPassCreateInfo where
        type FieldType "pSubpasses" VkRenderPassCreateInfo =
             Ptr VkSubpassDescription
        type FieldOptional "pSubpasses" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSubpasses" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, pSubpasses}
        type FieldIsArray "pSubpasses" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, pSubpasses}

instance {-# OVERLAPPING #-}
         CanReadField "pSubpasses" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pSubpasses})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pSubpasses}

instance {-# OVERLAPPING #-}
         CanWriteField "pSubpasses" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pSubpasses}

instance {-# OVERLAPPING #-}
         HasField "dependencyCount" VkRenderPassCreateInfo where
        type FieldType "dependencyCount" VkRenderPassCreateInfo = Word32
        type FieldOptional "dependencyCount" VkRenderPassCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dependencyCount" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, dependencyCount}
        type FieldIsArray "dependencyCount" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         CanReadField "dependencyCount" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, dependencyCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         CanWriteField "dependencyCount" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         HasField "pDependencies" VkRenderPassCreateInfo where
        type FieldType "pDependencies" VkRenderPassCreateInfo =
             Ptr VkSubpassDependency
        type FieldOptional "pDependencies" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDependencies" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, pDependencies}
        type FieldIsArray "pDependencies" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, pDependencies}

instance {-# OVERLAPPING #-}
         CanReadField "pDependencies" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pDependencies})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pDependencies}

instance {-# OVERLAPPING #-}
         CanWriteField "pDependencies" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pDependencies}

instance Show VkRenderPassCreateInfo where
        showsPrec d x
          = showString "VkRenderPassCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "attachmentCount = " .
                                  showsPrec d (getField @"attachmentCount" x) .
                                    showString ", " .
                                      showString "pAttachments = " .
                                        showsPrec d (getField @"pAttachments" x) .
                                          showString ", " .
                                            showString "subpassCount = " .
                                              showsPrec d (getField @"subpassCount" x) .
                                                showString ", " .
                                                  showString "pSubpasses = " .
                                                    showsPrec d (getField @"pSubpasses" x) .
                                                      showString ", " .
                                                        showString "dependencyCount = " .
                                                          showsPrec d
                                                            (getField @"dependencyCount" x)
                                                            .
                                                            showString ", " .
                                                              showString "pDependencies = " .
                                                                showsPrec d
                                                                  (getField @"pDependencies" x)
                                                                  . showChar '}'

-- | > typedef struct VkRenderPassInputAttachmentAspectCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                     pNext;
--   >     uint32_t                        aspectReferenceCount;
--   >     const VkInputAttachmentAspectReference* pAspectReferences;
--   > } VkRenderPassInputAttachmentAspectCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRenderPassInputAttachmentAspectCreateInfo VkRenderPassInputAttachmentAspectCreateInfo registry at www.khronos.org>
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

-- | Alias for `VkRenderPassInputAttachmentAspectCreateInfo`
type VkRenderPassInputAttachmentAspectCreateInfoKHR =
     VkRenderPassInputAttachmentAspectCreateInfo

-- | > typedef struct VkRenderPassMultiviewCreateInfo {
--   >     VkStructureType        sType;
--   >     const void*            pNext;
--   >     uint32_t               subpassCount;
--   >     const uint32_t*     pViewMasks;
--   >     uint32_t               dependencyCount;
--   >     const int32_t*   pViewOffsets;
--   >     uint32_t               correlationMaskCount;
--   >     const uint32_t* pCorrelationMasks;
--   > } VkRenderPassMultiviewCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRenderPassMultiviewCreateInfo VkRenderPassMultiviewCreateInfo registry at www.khronos.org>
data VkRenderPassMultiviewCreateInfo = VkRenderPassMultiviewCreateInfo## Addr##
                                                                        ByteArray##

instance Eq VkRenderPassMultiviewCreateInfo where
        (VkRenderPassMultiviewCreateInfo## a _) ==
          x@(VkRenderPassMultiviewCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassMultiviewCreateInfo where
        (VkRenderPassMultiviewCreateInfo## a _) `compare`
          x@(VkRenderPassMultiviewCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassMultiviewCreateInfo where
        sizeOf ~_ = #{size VkRenderPassMultiviewCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassMultiviewCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassMultiviewCreateInfo where
        unsafeAddr (VkRenderPassMultiviewCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassMultiviewCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassMultiviewCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassMultiviewCreateInfo where
        type StructFields VkRenderPassMultiviewCreateInfo =
             '["sType", "pNext", "subpassCount", "pViewMasks", -- ' closing tick for hsc2hs
               "dependencyCount", "pViewOffsets", "correlationMaskCount",
               "pCorrelationMasks"]
        type CUnionType VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassMultiviewCreateInfo =
             '[VkRenderPassCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassMultiviewCreateInfo where
        type FieldType "sType" VkRenderPassMultiviewCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassMultiviewCreateInfo =
             #{offset VkRenderPassMultiviewCreateInfo, sType}
        type FieldIsArray "sType" VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassMultiviewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkRenderPassMultiviewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassMultiviewCreateInfo where
        type FieldType "pNext" VkRenderPassMultiviewCreateInfo = Ptr Void
        type FieldOptional "pNext" VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassMultiviewCreateInfo =
             #{offset VkRenderPassMultiviewCreateInfo, pNext}
        type FieldIsArray "pNext" VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassMultiviewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkRenderPassMultiviewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "subpassCount" VkRenderPassMultiviewCreateInfo where
        type FieldType "subpassCount" VkRenderPassMultiviewCreateInfo =
             Word32
        type FieldOptional "subpassCount" VkRenderPassMultiviewCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "subpassCount" VkRenderPassMultiviewCreateInfo =
             #{offset VkRenderPassMultiviewCreateInfo, subpassCount}
        type FieldIsArray "subpassCount" VkRenderPassMultiviewCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         CanReadField "subpassCount" VkRenderPassMultiviewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, subpassCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         CanWriteField "subpassCount" VkRenderPassMultiviewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         HasField "pViewMasks" VkRenderPassMultiviewCreateInfo where
        type FieldType "pViewMasks" VkRenderPassMultiviewCreateInfo =
             Ptr Word32
        type FieldOptional "pViewMasks" VkRenderPassMultiviewCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewMasks" VkRenderPassMultiviewCreateInfo =
             #{offset VkRenderPassMultiviewCreateInfo, pViewMasks}
        type FieldIsArray "pViewMasks" VkRenderPassMultiviewCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, pViewMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pViewMasks" VkRenderPassMultiviewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, pViewMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, pViewMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewMasks" VkRenderPassMultiviewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, pViewMasks}

instance {-# OVERLAPPING #-}
         HasField "dependencyCount" VkRenderPassMultiviewCreateInfo where
        type FieldType "dependencyCount" VkRenderPassMultiviewCreateInfo =
             Word32
        type FieldOptional "dependencyCount"
               VkRenderPassMultiviewCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dependencyCount" VkRenderPassMultiviewCreateInfo
             =
             #{offset VkRenderPassMultiviewCreateInfo, dependencyCount}
        type FieldIsArray "dependencyCount" VkRenderPassMultiviewCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         CanReadField "dependencyCount" VkRenderPassMultiviewCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, dependencyCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         CanWriteField "dependencyCount" VkRenderPassMultiviewCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         HasField "pViewOffsets" VkRenderPassMultiviewCreateInfo where
        type FieldType "pViewOffsets" VkRenderPassMultiviewCreateInfo =
             Ptr Int32
        type FieldOptional "pViewOffsets" VkRenderPassMultiviewCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewOffsets" VkRenderPassMultiviewCreateInfo =
             #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets}
        type FieldIsArray "pViewOffsets" VkRenderPassMultiviewCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets}

instance {-# OVERLAPPING #-}
         CanReadField "pViewOffsets" VkRenderPassMultiviewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewOffsets" VkRenderPassMultiviewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets}

instance {-# OVERLAPPING #-}
         HasField "correlationMaskCount" VkRenderPassMultiviewCreateInfo
         where
        type FieldType "correlationMaskCount"
               VkRenderPassMultiviewCreateInfo
             = Word32
        type FieldOptional "correlationMaskCount"
               VkRenderPassMultiviewCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "correlationMaskCount"
               VkRenderPassMultiviewCreateInfo
             =
             #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount}
        type FieldIsArray "correlationMaskCount"
               VkRenderPassMultiviewCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount}

instance {-# OVERLAPPING #-}
         CanReadField "correlationMaskCount" VkRenderPassMultiviewCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount}

instance {-# OVERLAPPING #-}
         CanWriteField "correlationMaskCount"
           VkRenderPassMultiviewCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount}

instance {-# OVERLAPPING #-}
         HasField "pCorrelationMasks" VkRenderPassMultiviewCreateInfo where
        type FieldType "pCorrelationMasks" VkRenderPassMultiviewCreateInfo
             = Ptr Word32
        type FieldOptional "pCorrelationMasks"
               VkRenderPassMultiviewCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pCorrelationMasks"
               VkRenderPassMultiviewCreateInfo
             =
             #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks}
        type FieldIsArray "pCorrelationMasks"
               VkRenderPassMultiviewCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pCorrelationMasks" VkRenderPassMultiviewCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pCorrelationMasks" VkRenderPassMultiviewCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks}

instance Show VkRenderPassMultiviewCreateInfo where
        showsPrec d x
          = showString "VkRenderPassMultiviewCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "subpassCount = " .
                            showsPrec d (getField @"subpassCount" x) .
                              showString ", " .
                                showString "pViewMasks = " .
                                  showsPrec d (getField @"pViewMasks" x) .
                                    showString ", " .
                                      showString "dependencyCount = " .
                                        showsPrec d (getField @"dependencyCount" x) .
                                          showString ", " .
                                            showString "pViewOffsets = " .
                                              showsPrec d (getField @"pViewOffsets" x) .
                                                showString ", " .
                                                  showString "correlationMaskCount = " .
                                                    showsPrec d (getField @"correlationMaskCount" x)
                                                      .
                                                      showString ", " .
                                                        showString "pCorrelationMasks = " .
                                                          showsPrec d
                                                            (getField @"pCorrelationMasks" x)
                                                            . showChar '}'

-- | Alias for `VkRenderPassMultiviewCreateInfo`
type VkRenderPassMultiviewCreateInfoKHR =
     VkRenderPassMultiviewCreateInfo

-- | > typedef struct VkRenderPassSampleLocationsBeginInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         attachmentInitialSampleLocationsCount;
--   >     const VkAttachmentSampleLocationsEXT* pAttachmentInitialSampleLocations;
--   >     uint32_t         postSubpassSampleLocationsCount;
--   >     const VkSubpassSampleLocationsEXT* pPostSubpassSampleLocations;
--   > } VkRenderPassSampleLocationsBeginInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkRenderPassSampleLocationsBeginInfoEXT VkRenderPassSampleLocationsBeginInfoEXT registry at www.khronos.org>
data VkRenderPassSampleLocationsBeginInfoEXT = VkRenderPassSampleLocationsBeginInfoEXT## Addr##
                                                                                        ByteArray##

instance Eq VkRenderPassSampleLocationsBeginInfoEXT where
        (VkRenderPassSampleLocationsBeginInfoEXT## a _) ==
          x@(VkRenderPassSampleLocationsBeginInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassSampleLocationsBeginInfoEXT where
        (VkRenderPassSampleLocationsBeginInfoEXT## a _) `compare`
          x@(VkRenderPassSampleLocationsBeginInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassSampleLocationsBeginInfoEXT where
        sizeOf ~_
          = #{size VkRenderPassSampleLocationsBeginInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassSampleLocationsBeginInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassSampleLocationsBeginInfoEXT
         where
        unsafeAddr (VkRenderPassSampleLocationsBeginInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassSampleLocationsBeginInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassSampleLocationsBeginInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassSampleLocationsBeginInfoEXT
         where
        type StructFields VkRenderPassSampleLocationsBeginInfoEXT =
             '["sType", "pNext", "attachmentInitialSampleLocationsCount", -- ' closing tick for hsc2hs
               "pAttachmentInitialSampleLocations",
               "postSubpassSampleLocationsCount", "pPostSubpassSampleLocations"]
        type CUnionType VkRenderPassSampleLocationsBeginInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassSampleLocationsBeginInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassSampleLocationsBeginInfoEXT =
             '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassSampleLocationsBeginInfoEXT where
        type FieldType "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}
        type FieldIsArray "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassSampleLocationsBeginInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkRenderPassSampleLocationsBeginInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassSampleLocationsBeginInfoEXT where
        type FieldType "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}
        type FieldIsArray "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassSampleLocationsBeginInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkRenderPassSampleLocationsBeginInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32
        type FieldOptional "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}
        type FieldIsArray "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance {-# OVERLAPPING #-}
         CanReadField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance {-# OVERLAPPING #-}
         CanWriteField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkAttachmentSampleLocationsEXT
        type FieldOptional "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}
        type FieldIsArray "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance {-# OVERLAPPING #-}
         CanReadField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance {-# OVERLAPPING #-}
         HasField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32
        type FieldOptional "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}
        type FieldIsArray "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance {-# OVERLAPPING #-}
         CanReadField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance {-# OVERLAPPING #-}
         CanWriteField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkSubpassSampleLocationsEXT
        type FieldOptional "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}
        type FieldIsArray "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance {-# OVERLAPPING #-}
         CanReadField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance {-# OVERLAPPING #-}
         CanWriteField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance Show VkRenderPassSampleLocationsBeginInfoEXT where
        showsPrec d x
          = showString "VkRenderPassSampleLocationsBeginInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "attachmentInitialSampleLocationsCount = " .
                            showsPrec d (getField @"attachmentInitialSampleLocationsCount" x) .
                              showString ", " .
                                showString "pAttachmentInitialSampleLocations = " .
                                  showsPrec d (getField @"pAttachmentInitialSampleLocations" x) .
                                    showString ", " .
                                      showString "postSubpassSampleLocationsCount = " .
                                        showsPrec d (getField @"postSubpassSampleLocationsCount" x)
                                          .
                                          showString ", " .
                                            showString "pPostSubpassSampleLocations = " .
                                              showsPrec d
                                                (getField @"pPostSubpassSampleLocations" x)
                                                . showChar '}'
