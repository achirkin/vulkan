#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSubpassDescription
       (VkSubpassDescription(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint       (VkPipelineBindPoint)
import           Graphics.Vulkan.Types.Enum.VkSubpassDescriptionFlags (VkSubpassDescriptionFlags)
import           Graphics.Vulkan.Types.Struct.VkAttachmentReference   (VkAttachmentReference)
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSubpassDescription.html VkSubpassDescription registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkFlags VkSubpassDescription where
        type VkFlagsMType VkSubpassDescription = VkSubpassDescriptionFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkSubpassDescription, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkSubpassDescription, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkSubpassDescription, flags}

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

instance CanReadField "flags" VkSubpassDescription where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkSubpassDescription where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkPipelineBindPoint VkSubpassDescription where
        type VkPipelineBindPointMType VkSubpassDescription =
             VkPipelineBindPoint

        {-# NOINLINE vkPipelineBindPoint #-}
        vkPipelineBindPoint x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pipelineBindPoint})

        {-# INLINE vkPipelineBindPointByteOffset #-}
        vkPipelineBindPointByteOffset ~_
          = #{offset VkSubpassDescription, pipelineBindPoint}

        {-# INLINE readVkPipelineBindPoint #-}
        readVkPipelineBindPoint p
          = peekByteOff p #{offset VkSubpassDescription, pipelineBindPoint}

        {-# INLINE writeVkPipelineBindPoint #-}
        writeVkPipelineBindPoint p
          = pokeByteOff p #{offset VkSubpassDescription, pipelineBindPoint}

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

instance CanReadField "pipelineBindPoint" VkSubpassDescription
         where
        {-# INLINE getField #-}
        getField = vkPipelineBindPoint

        {-# INLINE readField #-}
        readField = readVkPipelineBindPoint

instance CanWriteField "pipelineBindPoint" VkSubpassDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkPipelineBindPoint

instance {-# OVERLAPPING #-}
         HasVkInputAttachmentCount VkSubpassDescription where
        type VkInputAttachmentCountMType VkSubpassDescription = Word32

        {-# NOINLINE vkInputAttachmentCount #-}
        vkInputAttachmentCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, inputAttachmentCount})

        {-# INLINE vkInputAttachmentCountByteOffset #-}
        vkInputAttachmentCountByteOffset ~_
          = #{offset VkSubpassDescription, inputAttachmentCount}

        {-# INLINE readVkInputAttachmentCount #-}
        readVkInputAttachmentCount p
          = peekByteOff p #{offset VkSubpassDescription, inputAttachmentCount}

        {-# INLINE writeVkInputAttachmentCount #-}
        writeVkInputAttachmentCount p
          = pokeByteOff p #{offset VkSubpassDescription, inputAttachmentCount}

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

instance CanReadField "inputAttachmentCount" VkSubpassDescription
         where
        {-# INLINE getField #-}
        getField = vkInputAttachmentCount

        {-# INLINE readField #-}
        readField = readVkInputAttachmentCount

instance CanWriteField "inputAttachmentCount" VkSubpassDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkInputAttachmentCount

instance {-# OVERLAPPING #-}
         HasVkPInputAttachments VkSubpassDescription where
        type VkPInputAttachmentsMType VkSubpassDescription =
             Ptr VkAttachmentReference

        {-# NOINLINE vkPInputAttachments #-}
        vkPInputAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pInputAttachments})

        {-# INLINE vkPInputAttachmentsByteOffset #-}
        vkPInputAttachmentsByteOffset ~_
          = #{offset VkSubpassDescription, pInputAttachments}

        {-# INLINE readVkPInputAttachments #-}
        readVkPInputAttachments p
          = peekByteOff p #{offset VkSubpassDescription, pInputAttachments}

        {-# INLINE writeVkPInputAttachments #-}
        writeVkPInputAttachments p
          = pokeByteOff p #{offset VkSubpassDescription, pInputAttachments}

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

instance CanReadField "pInputAttachments" VkSubpassDescription
         where
        {-# INLINE getField #-}
        getField = vkPInputAttachments

        {-# INLINE readField #-}
        readField = readVkPInputAttachments

instance CanWriteField "pInputAttachments" VkSubpassDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkPInputAttachments

instance {-# OVERLAPPING #-}
         HasVkColorAttachmentCount VkSubpassDescription where
        type VkColorAttachmentCountMType VkSubpassDescription = Word32

        {-# NOINLINE vkColorAttachmentCount #-}
        vkColorAttachmentCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, colorAttachmentCount})

        {-# INLINE vkColorAttachmentCountByteOffset #-}
        vkColorAttachmentCountByteOffset ~_
          = #{offset VkSubpassDescription, colorAttachmentCount}

        {-# INLINE readVkColorAttachmentCount #-}
        readVkColorAttachmentCount p
          = peekByteOff p #{offset VkSubpassDescription, colorAttachmentCount}

        {-# INLINE writeVkColorAttachmentCount #-}
        writeVkColorAttachmentCount p
          = pokeByteOff p #{offset VkSubpassDescription, colorAttachmentCount}

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

instance CanReadField "colorAttachmentCount" VkSubpassDescription
         where
        {-# INLINE getField #-}
        getField = vkColorAttachmentCount

        {-# INLINE readField #-}
        readField = readVkColorAttachmentCount

instance CanWriteField "colorAttachmentCount" VkSubpassDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkColorAttachmentCount

instance {-# OVERLAPPING #-}
         HasVkPColorAttachments VkSubpassDescription where
        type VkPColorAttachmentsMType VkSubpassDescription =
             Ptr VkAttachmentReference

        {-# NOINLINE vkPColorAttachments #-}
        vkPColorAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pColorAttachments})

        {-# INLINE vkPColorAttachmentsByteOffset #-}
        vkPColorAttachmentsByteOffset ~_
          = #{offset VkSubpassDescription, pColorAttachments}

        {-# INLINE readVkPColorAttachments #-}
        readVkPColorAttachments p
          = peekByteOff p #{offset VkSubpassDescription, pColorAttachments}

        {-# INLINE writeVkPColorAttachments #-}
        writeVkPColorAttachments p
          = pokeByteOff p #{offset VkSubpassDescription, pColorAttachments}

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

instance CanReadField "pColorAttachments" VkSubpassDescription
         where
        {-# INLINE getField #-}
        getField = vkPColorAttachments

        {-# INLINE readField #-}
        readField = readVkPColorAttachments

instance CanWriteField "pColorAttachments" VkSubpassDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkPColorAttachments

instance {-# OVERLAPPING #-}
         HasVkPResolveAttachments VkSubpassDescription where
        type VkPResolveAttachmentsMType VkSubpassDescription =
             Ptr VkAttachmentReference

        {-# NOINLINE vkPResolveAttachments #-}
        vkPResolveAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pResolveAttachments})

        {-# INLINE vkPResolveAttachmentsByteOffset #-}
        vkPResolveAttachmentsByteOffset ~_
          = #{offset VkSubpassDescription, pResolveAttachments}

        {-# INLINE readVkPResolveAttachments #-}
        readVkPResolveAttachments p
          = peekByteOff p #{offset VkSubpassDescription, pResolveAttachments}

        {-# INLINE writeVkPResolveAttachments #-}
        writeVkPResolveAttachments p
          = pokeByteOff p #{offset VkSubpassDescription, pResolveAttachments}

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

instance CanReadField "pResolveAttachments" VkSubpassDescription
         where
        {-# INLINE getField #-}
        getField = vkPResolveAttachments

        {-# INLINE readField #-}
        readField = readVkPResolveAttachments

instance CanWriteField "pResolveAttachments" VkSubpassDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkPResolveAttachments

instance {-# OVERLAPPING #-}
         HasVkPDepthStencilAttachment VkSubpassDescription where
        type VkPDepthStencilAttachmentMType VkSubpassDescription =
             Ptr VkAttachmentReference

        {-# NOINLINE vkPDepthStencilAttachment #-}
        vkPDepthStencilAttachment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pDepthStencilAttachment})

        {-# INLINE vkPDepthStencilAttachmentByteOffset #-}
        vkPDepthStencilAttachmentByteOffset ~_
          = #{offset VkSubpassDescription, pDepthStencilAttachment}

        {-# INLINE readVkPDepthStencilAttachment #-}
        readVkPDepthStencilAttachment p
          = peekByteOff p #{offset VkSubpassDescription, pDepthStencilAttachment}

        {-# INLINE writeVkPDepthStencilAttachment #-}
        writeVkPDepthStencilAttachment p
          = pokeByteOff p #{offset VkSubpassDescription, pDepthStencilAttachment}

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

instance CanReadField "pDepthStencilAttachment"
           VkSubpassDescription
         where
        {-# INLINE getField #-}
        getField = vkPDepthStencilAttachment

        {-# INLINE readField #-}
        readField = readVkPDepthStencilAttachment

instance CanWriteField "pDepthStencilAttachment"
           VkSubpassDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDepthStencilAttachment

instance {-# OVERLAPPING #-}
         HasVkPreserveAttachmentCount VkSubpassDescription where
        type VkPreserveAttachmentCountMType VkSubpassDescription = Word32

        {-# NOINLINE vkPreserveAttachmentCount #-}
        vkPreserveAttachmentCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, preserveAttachmentCount})

        {-# INLINE vkPreserveAttachmentCountByteOffset #-}
        vkPreserveAttachmentCountByteOffset ~_
          = #{offset VkSubpassDescription, preserveAttachmentCount}

        {-# INLINE readVkPreserveAttachmentCount #-}
        readVkPreserveAttachmentCount p
          = peekByteOff p #{offset VkSubpassDescription, preserveAttachmentCount}

        {-# INLINE writeVkPreserveAttachmentCount #-}
        writeVkPreserveAttachmentCount p
          = pokeByteOff p #{offset VkSubpassDescription, preserveAttachmentCount}

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

instance CanReadField "preserveAttachmentCount"
           VkSubpassDescription
         where
        {-# INLINE getField #-}
        getField = vkPreserveAttachmentCount

        {-# INLINE readField #-}
        readField = readVkPreserveAttachmentCount

instance CanWriteField "preserveAttachmentCount"
           VkSubpassDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkPreserveAttachmentCount

instance {-# OVERLAPPING #-}
         HasVkPPreserveAttachments VkSubpassDescription where
        type VkPPreserveAttachmentsMType VkSubpassDescription = Ptr Word32

        {-# NOINLINE vkPPreserveAttachments #-}
        vkPPreserveAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDescription, pPreserveAttachments})

        {-# INLINE vkPPreserveAttachmentsByteOffset #-}
        vkPPreserveAttachmentsByteOffset ~_
          = #{offset VkSubpassDescription, pPreserveAttachments}

        {-# INLINE readVkPPreserveAttachments #-}
        readVkPPreserveAttachments p
          = peekByteOff p #{offset VkSubpassDescription, pPreserveAttachments}

        {-# INLINE writeVkPPreserveAttachments #-}
        writeVkPPreserveAttachments p
          = pokeByteOff p #{offset VkSubpassDescription, pPreserveAttachments}

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

instance CanReadField "pPreserveAttachments" VkSubpassDescription
         where
        {-# INLINE getField #-}
        getField = vkPPreserveAttachments

        {-# INLINE readField #-}
        readField = readVkPPreserveAttachments

instance CanWriteField "pPreserveAttachments" VkSubpassDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkPPreserveAttachments

instance Show VkSubpassDescription where
        showsPrec d x
          = showString "VkSubpassDescription {" .
              showString "vkFlags = " .
                showsPrec d (vkFlags x) .
                  showString ", " .
                    showString "vkPipelineBindPoint = " .
                      showsPrec d (vkPipelineBindPoint x) .
                        showString ", " .
                          showString "vkInputAttachmentCount = " .
                            showsPrec d (vkInputAttachmentCount x) .
                              showString ", " .
                                showString "vkPInputAttachments = " .
                                  showsPrec d (vkPInputAttachments x) .
                                    showString ", " .
                                      showString "vkColorAttachmentCount = " .
                                        showsPrec d (vkColorAttachmentCount x) .
                                          showString ", " .
                                            showString "vkPColorAttachments = " .
                                              showsPrec d (vkPColorAttachments x) .
                                                showString ", " .
                                                  showString "vkPResolveAttachments = " .
                                                    showsPrec d (vkPResolveAttachments x) .
                                                      showString ", " .
                                                        showString "vkPDepthStencilAttachment = " .
                                                          showsPrec d (vkPDepthStencilAttachment x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "vkPreserveAttachmentCount = "
                                                                .
                                                                showsPrec d
                                                                  (vkPreserveAttachmentCount x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkPPreserveAttachments = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkPPreserveAttachments x)
                                                                        . showChar '}'
