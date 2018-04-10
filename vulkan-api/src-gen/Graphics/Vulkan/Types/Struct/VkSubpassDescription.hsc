#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSubpassDescription
       (VkSubpassDescription(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Base                                             (Addr##, ByteArray##,
                                                                       byteArrayContents##,
                                                                       plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint       (VkPipelineBindPoint)
import           Graphics.Vulkan.Types.Enum.VkSubpassDescriptionFlags (VkSubpassDescriptionFlags)
import           Graphics.Vulkan.Types.Struct.VkAttachmentReference   (VkAttachmentReference)
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkSubpassDescription {
--   >     VkSubpassDescriptionFlags flags;
--   >     VkPipelineBindPoint    pipelineBindPoint;
--   >     uint32_t               inputAttachmentCount;
--   >     const VkAttachmentReference* pInputAttachments;
--   >     uint32_t               colorAttachmentCount;
--   >     const VkAttachmentReference* pColorAttachments;
--   >     const VkAttachmentReference* pResolveAttachments;
--   >     const VkAttachmentReference* pDepthStencilAttachment;
--   >     uint32_t               preserveAttachmentCount;
--   >     const uint32_t* pPreserveAttachments;
--   > } VkSubpassDescription;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkSubpassDescription VkSubpassDescription registry at www.khronos.org>
data VkSubpassDescription = VkSubpassDescription## Addr## ByteArray##

instance Eq VkSubpassDescription where
        (VkSubpassDescription## a _) == x@(VkSubpassDescription## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSubpassDescription where
        (VkSubpassDescription## a _) `compare` x@(VkSubpassDescription## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSubpassDescription where
        sizeOf ~_ = #{size VkSubpassDescription}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSubpassDescription}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSubpassDescription where
        unsafeAddr (VkSubpassDescription## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSubpassDescription## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSubpassDescription## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSubpassDescription where
        type StructFields VkSubpassDescription =
             '["flags", "pipelineBindPoint", "inputAttachmentCount", -- ' closing tick for hsc2hs
               "pInputAttachments", "colorAttachmentCount", "pColorAttachments",
               "pResolveAttachments", "pDepthStencilAttachment",
               "preserveAttachmentCount", "pPreserveAttachments"]
        type CUnionType VkSubpassDescription = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSubpassDescription = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSubpassDescription = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "flags" VkSubpassDescription
         where
        type FieldType "flags" VkSubpassDescription =
             VkSubpassDescriptionFlags
        type FieldOptional "flags" VkSubpassDescription = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSubpassDescription =
             #{offset VkSubpassDescription, flags}
        type FieldIsArray "flags" VkSubpassDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubpassDescription, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkSubpassDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDescription, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDescription, flags}

instance {-# OVERLAPPING #-}
         HasField "pipelineBindPoint" VkSubpassDescription where
        type FieldType "pipelineBindPoint" VkSubpassDescription =
             VkPipelineBindPoint
        type FieldOptional "pipelineBindPoint" VkSubpassDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineBindPoint" VkSubpassDescription =
             #{offset VkSubpassDescription, pipelineBindPoint}
        type FieldIsArray "pipelineBindPoint" VkSubpassDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDescription, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineBindPoint" VkSubpassDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pipelineBindPoint})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDescription, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineBindPoint" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDescription, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         HasField "inputAttachmentCount" VkSubpassDescription where
        type FieldType "inputAttachmentCount" VkSubpassDescription = Word32
        type FieldOptional "inputAttachmentCount" VkSubpassDescription =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "inputAttachmentCount" VkSubpassDescription =
             #{offset VkSubpassDescription, inputAttachmentCount}
        type FieldIsArray "inputAttachmentCount" VkSubpassDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDescription, inputAttachmentCount}

instance {-# OVERLAPPING #-}
         CanReadField "inputAttachmentCount" VkSubpassDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, inputAttachmentCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDescription, inputAttachmentCount}

instance {-# OVERLAPPING #-}
         CanWriteField "inputAttachmentCount" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDescription, inputAttachmentCount}

instance {-# OVERLAPPING #-}
         HasField "pInputAttachments" VkSubpassDescription where
        type FieldType "pInputAttachments" VkSubpassDescription =
             Ptr VkAttachmentReference
        type FieldOptional "pInputAttachments" VkSubpassDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pInputAttachments" VkSubpassDescription =
             #{offset VkSubpassDescription, pInputAttachments}
        type FieldIsArray "pInputAttachments" VkSubpassDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDescription, pInputAttachments}

instance {-# OVERLAPPING #-}
         CanReadField "pInputAttachments" VkSubpassDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pInputAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDescription, pInputAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "pInputAttachments" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDescription, pInputAttachments}

instance {-# OVERLAPPING #-}
         HasField "colorAttachmentCount" VkSubpassDescription where
        type FieldType "colorAttachmentCount" VkSubpassDescription = Word32
        type FieldOptional "colorAttachmentCount" VkSubpassDescription =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "colorAttachmentCount" VkSubpassDescription =
             #{offset VkSubpassDescription, colorAttachmentCount}
        type FieldIsArray "colorAttachmentCount" VkSubpassDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDescription, colorAttachmentCount}

instance {-# OVERLAPPING #-}
         CanReadField "colorAttachmentCount" VkSubpassDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, colorAttachmentCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDescription, colorAttachmentCount}

instance {-# OVERLAPPING #-}
         CanWriteField "colorAttachmentCount" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDescription, colorAttachmentCount}

instance {-# OVERLAPPING #-}
         HasField "pColorAttachments" VkSubpassDescription where
        type FieldType "pColorAttachments" VkSubpassDescription =
             Ptr VkAttachmentReference
        type FieldOptional "pColorAttachments" VkSubpassDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pColorAttachments" VkSubpassDescription =
             #{offset VkSubpassDescription, pColorAttachments}
        type FieldIsArray "pColorAttachments" VkSubpassDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDescription, pColorAttachments}

instance {-# OVERLAPPING #-}
         CanReadField "pColorAttachments" VkSubpassDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pColorAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDescription, pColorAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "pColorAttachments" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDescription, pColorAttachments}

instance {-# OVERLAPPING #-}
         HasField "pResolveAttachments" VkSubpassDescription where
        type FieldType "pResolveAttachments" VkSubpassDescription =
             Ptr VkAttachmentReference
        type FieldOptional "pResolveAttachments" VkSubpassDescription =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pResolveAttachments" VkSubpassDescription =
             #{offset VkSubpassDescription, pResolveAttachments}
        type FieldIsArray "pResolveAttachments" VkSubpassDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDescription, pResolveAttachments}

instance {-# OVERLAPPING #-}
         CanReadField "pResolveAttachments" VkSubpassDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pResolveAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDescription, pResolveAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "pResolveAttachments" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDescription, pResolveAttachments}

instance {-# OVERLAPPING #-}
         HasField "pDepthStencilAttachment" VkSubpassDescription where
        type FieldType "pDepthStencilAttachment" VkSubpassDescription =
             Ptr VkAttachmentReference
        type FieldOptional "pDepthStencilAttachment" VkSubpassDescription =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pDepthStencilAttachment" VkSubpassDescription =
             #{offset VkSubpassDescription, pDepthStencilAttachment}
        type FieldIsArray "pDepthStencilAttachment" VkSubpassDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDescription, pDepthStencilAttachment}

instance {-# OVERLAPPING #-}
         CanReadField "pDepthStencilAttachment" VkSubpassDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pDepthStencilAttachment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDescription, pDepthStencilAttachment}

instance {-# OVERLAPPING #-}
         CanWriteField "pDepthStencilAttachment" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDescription, pDepthStencilAttachment}

instance {-# OVERLAPPING #-}
         HasField "preserveAttachmentCount" VkSubpassDescription where
        type FieldType "preserveAttachmentCount" VkSubpassDescription =
             Word32
        type FieldOptional "preserveAttachmentCount" VkSubpassDescription =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "preserveAttachmentCount" VkSubpassDescription =
             #{offset VkSubpassDescription, preserveAttachmentCount}
        type FieldIsArray "preserveAttachmentCount" VkSubpassDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDescription, preserveAttachmentCount}

instance {-# OVERLAPPING #-}
         CanReadField "preserveAttachmentCount" VkSubpassDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, preserveAttachmentCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDescription, preserveAttachmentCount}

instance {-# OVERLAPPING #-}
         CanWriteField "preserveAttachmentCount" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDescription, preserveAttachmentCount}

instance {-# OVERLAPPING #-}
         HasField "pPreserveAttachments" VkSubpassDescription where
        type FieldType "pPreserveAttachments" VkSubpassDescription =
             Ptr Word32
        type FieldOptional "pPreserveAttachments" VkSubpassDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pPreserveAttachments" VkSubpassDescription =
             #{offset VkSubpassDescription, pPreserveAttachments}
        type FieldIsArray "pPreserveAttachments" VkSubpassDescription =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDescription, pPreserveAttachments}

instance {-# OVERLAPPING #-}
         CanReadField "pPreserveAttachments" VkSubpassDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pPreserveAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDescription, pPreserveAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "pPreserveAttachments" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDescription, pPreserveAttachments}

instance Show VkSubpassDescription where
        showsPrec d x
          = showString "VkSubpassDescription {" .
              showString "flags = " .
                showsPrec d (getField @"flags" x) .
                  showString ", " .
                    showString "pipelineBindPoint = " .
                      showsPrec d (getField @"pipelineBindPoint" x) .
                        showString ", " .
                          showString "inputAttachmentCount = " .
                            showsPrec d (getField @"inputAttachmentCount" x) .
                              showString ", " .
                                showString "pInputAttachments = " .
                                  showsPrec d (getField @"pInputAttachments" x) .
                                    showString ", " .
                                      showString "colorAttachmentCount = " .
                                        showsPrec d (getField @"colorAttachmentCount" x) .
                                          showString ", " .
                                            showString "pColorAttachments = " .
                                              showsPrec d (getField @"pColorAttachments" x) .
                                                showString ", " .
                                                  showString "pResolveAttachments = " .
                                                    showsPrec d (getField @"pResolveAttachments" x)
                                                      .
                                                      showString ", " .
                                                        showString "pDepthStencilAttachment = " .
                                                          showsPrec d
                                                            (getField @"pDepthStencilAttachment" x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "preserveAttachmentCount = "
                                                                .
                                                                showsPrec d
                                                                  (getField
                                                                     @"preserveAttachmentCount"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "pPreserveAttachments = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"pPreserveAttachments"
                                                                           x)
                                                                        . showChar '}'
