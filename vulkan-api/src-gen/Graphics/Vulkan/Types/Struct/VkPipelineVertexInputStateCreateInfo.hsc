#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineVertexInputStateCreateInfo
       (VkPipelineVertexInputStateCreateInfo(..)) where
import           Foreign.Storable                                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                                 (VkPipelineVertexInputStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkVertexInputAttributeDescription (VkVertexInputAttributeDescription)
import           Graphics.Vulkan.Types.Struct.VkVertexInputBindingDescription   (VkVertexInputBindingDescription)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                               (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineVertexInputStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineVertexInputStateCreateFlags    flags;
--   >     uint32_t               vertexBindingDescriptionCount;
--   >     const VkVertexInputBindingDescription* pVertexBindingDescriptions;
--   >     uint32_t               vertexAttributeDescriptionCount;
--   >     const VkVertexInputAttributeDescription* pVertexAttributeDescriptions;
--   > } VkPipelineVertexInputStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineVertexInputStateCreateInfo.html VkPipelineVertexInputStateCreateInfo registry at www.khronos.org>
data VkPipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo## Addr##
                                                                                  ByteArray##

instance Eq VkPipelineVertexInputStateCreateInfo where
        (VkPipelineVertexInputStateCreateInfo## a _) ==
          x@(VkPipelineVertexInputStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineVertexInputStateCreateInfo where
        (VkPipelineVertexInputStateCreateInfo## a _) `compare`
          x@(VkPipelineVertexInputStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineVertexInputStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineVertexInputStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineVertexInputStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineVertexInputStateCreateInfo
         where
        unsafeAddr (VkPipelineVertexInputStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineVertexInputStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineVertexInputStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineVertexInputStateCreateInfo where
        type StructFields VkPipelineVertexInputStateCreateInfo =
             '["sType", "pNext", "flags", "vertexBindingDescriptionCount", -- ' closing tick for hsc2hs
               "pVertexBindingDescriptions", "vertexAttributeDescriptionCount",
               "pVertexAttributeDescriptions"]
        type CUnionType VkPipelineVertexInputStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineVertexInputStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineVertexInputStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineVertexInputStateCreateInfo where
        type VkSTypeMType VkPipelineVertexInputStateCreateInfo =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineVertexInputStateCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineVertexInputStateCreateInfo where
        type FieldType "sType" VkPipelineVertexInputStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineVertexInputStateCreateInfo =
             #{offset VkPipelineVertexInputStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, sType}

instance CanReadField "sType" VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineVertexInputStateCreateInfo where
        type VkPNextMType VkPipelineVertexInputStateCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineVertexInputStateCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineVertexInputStateCreateInfo where
        type FieldType "pNext" VkPipelineVertexInputStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineVertexInputStateCreateInfo =
             #{offset VkPipelineVertexInputStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, pNext}

instance CanReadField "pNext" VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineVertexInputStateCreateInfo where
        type VkFlagsMType VkPipelineVertexInputStateCreateInfo =
             VkPipelineVertexInputStateCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineVertexInputStateCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineVertexInputStateCreateInfo where
        type FieldType "flags" VkPipelineVertexInputStateCreateInfo =
             VkPipelineVertexInputStateCreateFlags
        type FieldOptional "flags" VkPipelineVertexInputStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineVertexInputStateCreateInfo =
             #{offset VkPipelineVertexInputStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, flags}

instance CanReadField "flags" VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkVertexBindingDescriptionCount
           VkPipelineVertexInputStateCreateInfo
         where
        type VkVertexBindingDescriptionCountMType
               VkPipelineVertexInputStateCreateInfo
             = Word32

        {-# NOINLINE vkVertexBindingDescriptionCount #-}
        vkVertexBindingDescriptionCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount})

        {-# INLINE vkVertexBindingDescriptionCountByteOffset #-}
        vkVertexBindingDescriptionCountByteOffset ~_
          = #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}

        {-# INLINE readVkVertexBindingDescriptionCount #-}
        readVkVertexBindingDescriptionCount p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}

        {-# INLINE writeVkVertexBindingDescriptionCount #-}
        writeVkVertexBindingDescriptionCount p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}

instance {-# OVERLAPPING #-}
         HasField "vertexBindingDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = Word32
        type FieldOptional "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}
        type FieldIsArray "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}

instance CanReadField "vertexBindingDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkVertexBindingDescriptionCount

        {-# INLINE readField #-}
        readField = readVkVertexBindingDescriptionCount

instance CanWriteField "vertexBindingDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkVertexBindingDescriptionCount

instance {-# OVERLAPPING #-}
         HasVkPVertexBindingDescriptions
           VkPipelineVertexInputStateCreateInfo
         where
        type VkPVertexBindingDescriptionsMType
               VkPipelineVertexInputStateCreateInfo
             = Ptr VkVertexInputBindingDescription

        {-# NOINLINE vkPVertexBindingDescriptions #-}
        vkPVertexBindingDescriptions x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions})

        {-# INLINE vkPVertexBindingDescriptionsByteOffset #-}
        vkPVertexBindingDescriptionsByteOffset ~_
          = #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}

        {-# INLINE readVkPVertexBindingDescriptions #-}
        readVkPVertexBindingDescriptions p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}

        {-# INLINE writeVkPVertexBindingDescriptions #-}
        writeVkPVertexBindingDescriptions p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}

instance {-# OVERLAPPING #-}
         HasField "pVertexBindingDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = Ptr VkVertexInputBindingDescription
        type FieldOptional "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}
        type FieldIsArray "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}

instance CanReadField "pVertexBindingDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPVertexBindingDescriptions

        {-# INLINE readField #-}
        readField = readVkPVertexBindingDescriptions

instance CanWriteField "pVertexBindingDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPVertexBindingDescriptions

instance {-# OVERLAPPING #-}
         HasVkVertexAttributeDescriptionCount
           VkPipelineVertexInputStateCreateInfo
         where
        type VkVertexAttributeDescriptionCountMType
               VkPipelineVertexInputStateCreateInfo
             = Word32

        {-# NOINLINE vkVertexAttributeDescriptionCount #-}
        vkVertexAttributeDescriptionCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount})

        {-# INLINE vkVertexAttributeDescriptionCountByteOffset #-}
        vkVertexAttributeDescriptionCountByteOffset ~_
          = #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}

        {-# INLINE readVkVertexAttributeDescriptionCount #-}
        readVkVertexAttributeDescriptionCount p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}

        {-# INLINE writeVkVertexAttributeDescriptionCount #-}
        writeVkVertexAttributeDescriptionCount p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}

instance {-# OVERLAPPING #-}
         HasField "vertexAttributeDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = Word32
        type FieldOptional "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}
        type FieldIsArray "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}

instance CanReadField "vertexAttributeDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkVertexAttributeDescriptionCount

        {-# INLINE readField #-}
        readField = readVkVertexAttributeDescriptionCount

instance CanWriteField "vertexAttributeDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkVertexAttributeDescriptionCount

instance {-# OVERLAPPING #-}
         HasVkPVertexAttributeDescriptions
           VkPipelineVertexInputStateCreateInfo
         where
        type VkPVertexAttributeDescriptionsMType
               VkPipelineVertexInputStateCreateInfo
             = Ptr VkVertexInputAttributeDescription

        {-# NOINLINE vkPVertexAttributeDescriptions #-}
        vkPVertexAttributeDescriptions x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions})

        {-# INLINE vkPVertexAttributeDescriptionsByteOffset #-}
        vkPVertexAttributeDescriptionsByteOffset ~_
          = #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}

        {-# INLINE readVkPVertexAttributeDescriptions #-}
        readVkPVertexAttributeDescriptions p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}

        {-# INLINE writeVkPVertexAttributeDescriptions #-}
        writeVkPVertexAttributeDescriptions p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}

instance {-# OVERLAPPING #-}
         HasField "pVertexAttributeDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = Ptr VkVertexInputAttributeDescription
        type FieldOptional "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}
        type FieldIsArray "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}

instance CanReadField "pVertexAttributeDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPVertexAttributeDescriptions

        {-# INLINE readField #-}
        readField = readVkPVertexAttributeDescriptions

instance CanWriteField "pVertexAttributeDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPVertexAttributeDescriptions

instance Show VkPipelineVertexInputStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineVertexInputStateCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkVertexBindingDescriptionCount = " .
                                  showsPrec d (vkVertexBindingDescriptionCount x) .
                                    showString ", " .
                                      showString "vkPVertexBindingDescriptions = " .
                                        showsPrec d (vkPVertexBindingDescriptions x) .
                                          showString ", " .
                                            showString "vkVertexAttributeDescriptionCount = " .
                                              showsPrec d (vkVertexAttributeDescriptionCount x) .
                                                showString ", " .
                                                  showString "vkPVertexAttributeDescriptions = " .
                                                    showsPrec d (vkPVertexAttributeDescriptions x) .
                                                      showChar '}'
