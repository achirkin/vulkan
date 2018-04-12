#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Subpass
       (VkSubpassDependency(..), VkSubpassDescription(..),
        VkSubpassSampleLocationsEXT(..))
       where
import           Foreign.Storable                            (Storable (..))
import           GHC.Base                                    (Addr##, ByteArray##,
                                                              byteArrayContents##,
                                                              plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.AccessFlags      (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.DependencyFlags  (VkDependencyFlags)
import           Graphics.Vulkan.Types.Enum.Pipeline         (VkPipelineBindPoint,
                                                              VkPipelineStageFlags)
import           Graphics.Vulkan.Types.Enum.Subpass          (VkSubpassDescriptionFlags)
import           Graphics.Vulkan.Types.Struct.Attachment     (VkAttachmentReference)
import           Graphics.Vulkan.Types.Struct.SampleLocation (VkSampleLocationsInfoEXT)
import           System.IO.Unsafe                            (unsafeDupablePerformIO)

-- | > typedef struct VkSubpassDependency {
--   >     uint32_t               srcSubpass;
--   >     uint32_t               dstSubpass;
--   >     VkPipelineStageFlags   srcStageMask;
--   >     VkPipelineStageFlags   dstStageMask;
--   >     VkAccessFlags          srcAccessMask;
--   >     VkAccessFlags          dstAccessMask;
--   >     VkDependencyFlags      dependencyFlags;
--   > } VkSubpassDependency;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubpassDependency VkSubpassDependency registry at www.khronos.org>
data VkSubpassDependency = VkSubpassDependency## Addr## ByteArray##

instance Eq VkSubpassDependency where
        (VkSubpassDependency## a _) == x@(VkSubpassDependency## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSubpassDependency where
        (VkSubpassDependency## a _) `compare` x@(VkSubpassDependency## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSubpassDependency where
        sizeOf ~_ = #{size VkSubpassDependency}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSubpassDependency}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSubpassDependency where
        unsafeAddr (VkSubpassDependency## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSubpassDependency## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSubpassDependency## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSubpassDependency where
        type StructFields VkSubpassDependency =
             '["srcSubpass", "dstSubpass", "srcStageMask", "dstStageMask", -- ' closing tick for hsc2hs
               "srcAccessMask", "dstAccessMask", "dependencyFlags"]
        type CUnionType VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSubpassDependency = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "srcSubpass" VkSubpassDependency where
        type FieldType "srcSubpass" VkSubpassDependency = Word32
        type FieldOptional "srcSubpass" VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcSubpass" VkSubpassDependency =
             #{offset VkSubpassDependency, srcSubpass}
        type FieldIsArray "srcSubpass" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubpassDependency, srcSubpass}

instance {-# OVERLAPPING #-}
         CanReadField "srcSubpass" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, srcSubpass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, srcSubpass}

instance {-# OVERLAPPING #-}
         CanWriteField "srcSubpass" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, srcSubpass}

instance {-# OVERLAPPING #-}
         HasField "dstSubpass" VkSubpassDependency where
        type FieldType "dstSubpass" VkSubpassDependency = Word32
        type FieldOptional "dstSubpass" VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstSubpass" VkSubpassDependency =
             #{offset VkSubpassDependency, dstSubpass}
        type FieldIsArray "dstSubpass" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSubpassDependency, dstSubpass}

instance {-# OVERLAPPING #-}
         CanReadField "dstSubpass" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dstSubpass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, dstSubpass}

instance {-# OVERLAPPING #-}
         CanWriteField "dstSubpass" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, dstSubpass}

instance {-# OVERLAPPING #-}
         HasField "srcStageMask" VkSubpassDependency where
        type FieldType "srcStageMask" VkSubpassDependency =
             VkPipelineStageFlags
        type FieldOptional "srcStageMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcStageMask" VkSubpassDependency =
             #{offset VkSubpassDependency, srcStageMask}
        type FieldIsArray "srcStageMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDependency, srcStageMask}

instance {-# OVERLAPPING #-}
         CanReadField "srcStageMask" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, srcStageMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, srcStageMask}

instance {-# OVERLAPPING #-}
         CanWriteField "srcStageMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, srcStageMask}

instance {-# OVERLAPPING #-}
         HasField "dstStageMask" VkSubpassDependency where
        type FieldType "dstStageMask" VkSubpassDependency =
             VkPipelineStageFlags
        type FieldOptional "dstStageMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstStageMask" VkSubpassDependency =
             #{offset VkSubpassDependency, dstStageMask}
        type FieldIsArray "dstStageMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDependency, dstStageMask}

instance {-# OVERLAPPING #-}
         CanReadField "dstStageMask" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dstStageMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, dstStageMask}

instance {-# OVERLAPPING #-}
         CanWriteField "dstStageMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, dstStageMask}

instance {-# OVERLAPPING #-}
         HasField "srcAccessMask" VkSubpassDependency where
        type FieldType "srcAccessMask" VkSubpassDependency = VkAccessFlags
        type FieldOptional "srcAccessMask" VkSubpassDependency = 'True -- ' closing tick for hsc2hs
        type FieldOffset "srcAccessMask" VkSubpassDependency =
             #{offset VkSubpassDependency, srcAccessMask}
        type FieldIsArray "srcAccessMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDependency, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "srcAccessMask" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, srcAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, srcAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "srcAccessMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, srcAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dstAccessMask" VkSubpassDependency where
        type FieldType "dstAccessMask" VkSubpassDependency = VkAccessFlags
        type FieldOptional "dstAccessMask" VkSubpassDependency = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dstAccessMask" VkSubpassDependency =
             #{offset VkSubpassDependency, dstAccessMask}
        type FieldIsArray "dstAccessMask" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDependency, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanReadField "dstAccessMask" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dstAccessMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, dstAccessMask}

instance {-# OVERLAPPING #-}
         CanWriteField "dstAccessMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, dstAccessMask}

instance {-# OVERLAPPING #-}
         HasField "dependencyFlags" VkSubpassDependency where
        type FieldType "dependencyFlags" VkSubpassDependency =
             VkDependencyFlags
        type FieldOptional "dependencyFlags" VkSubpassDependency = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dependencyFlags" VkSubpassDependency =
             #{offset VkSubpassDependency, dependencyFlags}
        type FieldIsArray "dependencyFlags" VkSubpassDependency = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassDependency, dependencyFlags}

instance {-# OVERLAPPING #-}
         CanReadField "dependencyFlags" VkSubpassDependency where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dependencyFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassDependency, dependencyFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "dependencyFlags" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassDependency, dependencyFlags}

instance Show VkSubpassDependency where
        showsPrec d x
          = showString "VkSubpassDependency {" .
              showString "srcSubpass = " .
                showsPrec d (getField @"srcSubpass" x) .
                  showString ", " .
                    showString "dstSubpass = " .
                      showsPrec d (getField @"dstSubpass" x) .
                        showString ", " .
                          showString "srcStageMask = " .
                            showsPrec d (getField @"srcStageMask" x) .
                              showString ", " .
                                showString "dstStageMask = " .
                                  showsPrec d (getField @"dstStageMask" x) .
                                    showString ", " .
                                      showString "srcAccessMask = " .
                                        showsPrec d (getField @"srcAccessMask" x) .
                                          showString ", " .
                                            showString "dstAccessMask = " .
                                              showsPrec d (getField @"dstAccessMask" x) .
                                                showString ", " .
                                                  showString "dependencyFlags = " .
                                                    showsPrec d (getField @"dependencyFlags" x) .
                                                      showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubpassDescription VkSubpassDescription registry at www.khronos.org>
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

-- | > typedef struct VkSubpassSampleLocationsEXT {
--   >     uint32_t                         subpassIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkSubpassSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSubpassSampleLocationsEXT VkSubpassSampleLocationsEXT registry at www.khronos.org>
data VkSubpassSampleLocationsEXT = VkSubpassSampleLocationsEXT## Addr##
                                                                ByteArray##

instance Eq VkSubpassSampleLocationsEXT where
        (VkSubpassSampleLocationsEXT## a _) ==
          x@(VkSubpassSampleLocationsEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSubpassSampleLocationsEXT where
        (VkSubpassSampleLocationsEXT## a _) `compare`
          x@(VkSubpassSampleLocationsEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSubpassSampleLocationsEXT where
        sizeOf ~_ = #{size VkSubpassSampleLocationsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSubpassSampleLocationsEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSubpassSampleLocationsEXT where
        unsafeAddr (VkSubpassSampleLocationsEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSubpassSampleLocationsEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSubpassSampleLocationsEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSubpassSampleLocationsEXT where
        type StructFields VkSubpassSampleLocationsEXT =
             '["subpassIndex", "sampleLocationsInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkSubpassSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSubpassSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSubpassSampleLocationsEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "subpassIndex" VkSubpassSampleLocationsEXT where
        type FieldType "subpassIndex" VkSubpassSampleLocationsEXT = Word32
        type FieldOptional "subpassIndex" VkSubpassSampleLocationsEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subpassIndex" VkSubpassSampleLocationsEXT =
             #{offset VkSubpassSampleLocationsEXT, subpassIndex}
        type FieldIsArray "subpassIndex" VkSubpassSampleLocationsEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassSampleLocationsEXT, subpassIndex}

instance {-# OVERLAPPING #-}
         CanReadField "subpassIndex" VkSubpassSampleLocationsEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassSampleLocationsEXT, subpassIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassSampleLocationsEXT, subpassIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "subpassIndex" VkSubpassSampleLocationsEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassSampleLocationsEXT, subpassIndex}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsInfo" VkSubpassSampleLocationsEXT where
        type FieldType "sampleLocationsInfo" VkSubpassSampleLocationsEXT =
             VkSampleLocationsInfoEXT
        type FieldOptional "sampleLocationsInfo"
               VkSubpassSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsInfo" VkSubpassSampleLocationsEXT
             =
             #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}
        type FieldIsArray "sampleLocationsInfo" VkSubpassSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationsInfo" VkSubpassSampleLocationsEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationsInfo" VkSubpassSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

instance Show VkSubpassSampleLocationsEXT where
        showsPrec d x
          = showString "VkSubpassSampleLocationsEXT {" .
              showString "subpassIndex = " .
                showsPrec d (getField @"subpassIndex" x) .
                  showString ", " .
                    showString "sampleLocationsInfo = " .
                      showsPrec d (getField @"sampleLocationsInfo" x) . showChar '}'
