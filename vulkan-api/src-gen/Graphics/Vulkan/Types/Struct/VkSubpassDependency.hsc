#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSubpassDependency
       (VkSubpassDependency(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkAccessFlags        (VkAccessFlags)
import           Graphics.Vulkan.Types.Enum.VkDependencyFlags    (VkDependencyFlags)
import           Graphics.Vulkan.Types.Enum.VkPipelineStageFlags (VkPipelineStageFlags)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSubpassDependency.html VkSubpassDependency registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSrcSubpass VkSubpassDependency
         where
        type VkSrcSubpassMType VkSubpassDependency = Word32

        {-# NOINLINE vkSrcSubpass #-}
        vkSrcSubpass x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, srcSubpass})

        {-# INLINE vkSrcSubpassByteOffset #-}
        vkSrcSubpassByteOffset ~_
          = #{offset VkSubpassDependency, srcSubpass}

        {-# INLINE readVkSrcSubpass #-}
        readVkSrcSubpass p
          = peekByteOff p #{offset VkSubpassDependency, srcSubpass}

        {-# INLINE writeVkSrcSubpass #-}
        writeVkSrcSubpass p
          = pokeByteOff p #{offset VkSubpassDependency, srcSubpass}

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

instance CanReadField "srcSubpass" VkSubpassDependency where
        {-# INLINE getField #-}
        getField = vkSrcSubpass

        {-# INLINE readField #-}
        readField = readVkSrcSubpass

instance CanWriteField "srcSubpass" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField = writeVkSrcSubpass

instance {-# OVERLAPPING #-} HasVkDstSubpass VkSubpassDependency
         where
        type VkDstSubpassMType VkSubpassDependency = Word32

        {-# NOINLINE vkDstSubpass #-}
        vkDstSubpass x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dstSubpass})

        {-# INLINE vkDstSubpassByteOffset #-}
        vkDstSubpassByteOffset ~_
          = #{offset VkSubpassDependency, dstSubpass}

        {-# INLINE readVkDstSubpass #-}
        readVkDstSubpass p
          = peekByteOff p #{offset VkSubpassDependency, dstSubpass}

        {-# INLINE writeVkDstSubpass #-}
        writeVkDstSubpass p
          = pokeByteOff p #{offset VkSubpassDependency, dstSubpass}

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

instance CanReadField "dstSubpass" VkSubpassDependency where
        {-# INLINE getField #-}
        getField = vkDstSubpass

        {-# INLINE readField #-}
        readField = readVkDstSubpass

instance CanWriteField "dstSubpass" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField = writeVkDstSubpass

instance {-# OVERLAPPING #-} HasVkSrcStageMask VkSubpassDependency
         where
        type VkSrcStageMaskMType VkSubpassDependency = VkPipelineStageFlags

        {-# NOINLINE vkSrcStageMask #-}
        vkSrcStageMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, srcStageMask})

        {-# INLINE vkSrcStageMaskByteOffset #-}
        vkSrcStageMaskByteOffset ~_
          = #{offset VkSubpassDependency, srcStageMask}

        {-# INLINE readVkSrcStageMask #-}
        readVkSrcStageMask p
          = peekByteOff p #{offset VkSubpassDependency, srcStageMask}

        {-# INLINE writeVkSrcStageMask #-}
        writeVkSrcStageMask p
          = pokeByteOff p #{offset VkSubpassDependency, srcStageMask}

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

instance CanReadField "srcStageMask" VkSubpassDependency where
        {-# INLINE getField #-}
        getField = vkSrcStageMask

        {-# INLINE readField #-}
        readField = readVkSrcStageMask

instance CanWriteField "srcStageMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField = writeVkSrcStageMask

instance {-# OVERLAPPING #-} HasVkDstStageMask VkSubpassDependency
         where
        type VkDstStageMaskMType VkSubpassDependency = VkPipelineStageFlags

        {-# NOINLINE vkDstStageMask #-}
        vkDstStageMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dstStageMask})

        {-# INLINE vkDstStageMaskByteOffset #-}
        vkDstStageMaskByteOffset ~_
          = #{offset VkSubpassDependency, dstStageMask}

        {-# INLINE readVkDstStageMask #-}
        readVkDstStageMask p
          = peekByteOff p #{offset VkSubpassDependency, dstStageMask}

        {-# INLINE writeVkDstStageMask #-}
        writeVkDstStageMask p
          = pokeByteOff p #{offset VkSubpassDependency, dstStageMask}

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

instance CanReadField "dstStageMask" VkSubpassDependency where
        {-# INLINE getField #-}
        getField = vkDstStageMask

        {-# INLINE readField #-}
        readField = readVkDstStageMask

instance CanWriteField "dstStageMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField = writeVkDstStageMask

instance {-# OVERLAPPING #-} HasVkSrcAccessMask VkSubpassDependency
         where
        type VkSrcAccessMaskMType VkSubpassDependency = VkAccessFlags

        {-# NOINLINE vkSrcAccessMask #-}
        vkSrcAccessMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, srcAccessMask})

        {-# INLINE vkSrcAccessMaskByteOffset #-}
        vkSrcAccessMaskByteOffset ~_
          = #{offset VkSubpassDependency, srcAccessMask}

        {-# INLINE readVkSrcAccessMask #-}
        readVkSrcAccessMask p
          = peekByteOff p #{offset VkSubpassDependency, srcAccessMask}

        {-# INLINE writeVkSrcAccessMask #-}
        writeVkSrcAccessMask p
          = pokeByteOff p #{offset VkSubpassDependency, srcAccessMask}

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

instance CanReadField "srcAccessMask" VkSubpassDependency where
        {-# INLINE getField #-}
        getField = vkSrcAccessMask

        {-# INLINE readField #-}
        readField = readVkSrcAccessMask

instance CanWriteField "srcAccessMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField = writeVkSrcAccessMask

instance {-# OVERLAPPING #-} HasVkDstAccessMask VkSubpassDependency
         where
        type VkDstAccessMaskMType VkSubpassDependency = VkAccessFlags

        {-# NOINLINE vkDstAccessMask #-}
        vkDstAccessMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dstAccessMask})

        {-# INLINE vkDstAccessMaskByteOffset #-}
        vkDstAccessMaskByteOffset ~_
          = #{offset VkSubpassDependency, dstAccessMask}

        {-# INLINE readVkDstAccessMask #-}
        readVkDstAccessMask p
          = peekByteOff p #{offset VkSubpassDependency, dstAccessMask}

        {-# INLINE writeVkDstAccessMask #-}
        writeVkDstAccessMask p
          = pokeByteOff p #{offset VkSubpassDependency, dstAccessMask}

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

instance CanReadField "dstAccessMask" VkSubpassDependency where
        {-# INLINE getField #-}
        getField = vkDstAccessMask

        {-# INLINE readField #-}
        readField = readVkDstAccessMask

instance CanWriteField "dstAccessMask" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField = writeVkDstAccessMask

instance {-# OVERLAPPING #-}
         HasVkDependencyFlags VkSubpassDependency where
        type VkDependencyFlagsMType VkSubpassDependency = VkDependencyFlags

        {-# NOINLINE vkDependencyFlags #-}
        vkDependencyFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassDependency, dependencyFlags})

        {-# INLINE vkDependencyFlagsByteOffset #-}
        vkDependencyFlagsByteOffset ~_
          = #{offset VkSubpassDependency, dependencyFlags}

        {-# INLINE readVkDependencyFlags #-}
        readVkDependencyFlags p
          = peekByteOff p #{offset VkSubpassDependency, dependencyFlags}

        {-# INLINE writeVkDependencyFlags #-}
        writeVkDependencyFlags p
          = pokeByteOff p #{offset VkSubpassDependency, dependencyFlags}

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

instance CanReadField "dependencyFlags" VkSubpassDependency where
        {-# INLINE getField #-}
        getField = vkDependencyFlags

        {-# INLINE readField #-}
        readField = readVkDependencyFlags

instance CanWriteField "dependencyFlags" VkSubpassDependency where
        {-# INLINE writeField #-}
        writeField = writeVkDependencyFlags

instance Show VkSubpassDependency where
        showsPrec d x
          = showString "VkSubpassDependency {" .
              showString "vkSrcSubpass = " .
                showsPrec d (vkSrcSubpass x) .
                  showString ", " .
                    showString "vkDstSubpass = " .
                      showsPrec d (vkDstSubpass x) .
                        showString ", " .
                          showString "vkSrcStageMask = " .
                            showsPrec d (vkSrcStageMask x) .
                              showString ", " .
                                showString "vkDstStageMask = " .
                                  showsPrec d (vkDstStageMask x) .
                                    showString ", " .
                                      showString "vkSrcAccessMask = " .
                                        showsPrec d (vkSrcAccessMask x) .
                                          showString ", " .
                                            showString "vkDstAccessMask = " .
                                              showsPrec d (vkDstAccessMask x) .
                                                showString ", " .
                                                  showString "vkDependencyFlags = " .
                                                    showsPrec d (vkDependencyFlags x) . showChar '}'
