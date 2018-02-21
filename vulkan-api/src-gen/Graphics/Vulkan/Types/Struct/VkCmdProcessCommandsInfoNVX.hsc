#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCmdProcessCommandsInfoNVX
       (VkCmdProcessCommandsInfoNVX(..)) where
import           Foreign.Storable                                        (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                         (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType              (VkStructureType)
import           Graphics.Vulkan.Types.Handles                           (VkBuffer,
                                                                          VkCommandBuffer,
                                                                          VkIndirectCommandsLayoutNVX,
                                                                          VkObjectTableNVX)
import           Graphics.Vulkan.Types.Struct.VkIndirectCommandsTokenNVX (VkIndirectCommandsTokenNVX)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

-- | > typedef struct VkCmdProcessCommandsInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkObjectTableNVX                                         objectTable;
--   >     VkIndirectCommandsLayoutNVX                              indirectCommandsLayout;
--   >     uint32_t                                                 indirectCommandsTokenCount;
--   >     const VkIndirectCommandsTokenNVX*       pIndirectCommandsTokens;
--   >     uint32_t                                                 maxSequencesCount;
--   >     VkCommandBuffer                          targetCommandBuffer;
--   >     VkBuffer                                 sequencesCountBuffer;
--   >     VkDeviceSize                             sequencesCountOffset;
--   >     VkBuffer                                 sequencesIndexBuffer;
--   >     VkDeviceSize                             sequencesIndexOffset;
--   > } VkCmdProcessCommandsInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkCmdProcessCommandsInfoNVX.html VkCmdProcessCommandsInfoNVX registry at www.khronos.org>
data VkCmdProcessCommandsInfoNVX = VkCmdProcessCommandsInfoNVX## Addr##
                                                                ByteArray##

instance Eq VkCmdProcessCommandsInfoNVX where
        (VkCmdProcessCommandsInfoNVX## a _) ==
          x@(VkCmdProcessCommandsInfoNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCmdProcessCommandsInfoNVX where
        (VkCmdProcessCommandsInfoNVX## a _) `compare`
          x@(VkCmdProcessCommandsInfoNVX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCmdProcessCommandsInfoNVX where
        sizeOf ~_ = #{size VkCmdProcessCommandsInfoNVX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCmdProcessCommandsInfoNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCmdProcessCommandsInfoNVX where
        unsafeAddr (VkCmdProcessCommandsInfoNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCmdProcessCommandsInfoNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCmdProcessCommandsInfoNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCmdProcessCommandsInfoNVX where
        type StructFields VkCmdProcessCommandsInfoNVX =
             '["sType", "pNext", "objectTable", "indirectCommandsLayout", -- ' closing tick for hsc2hs
               "indirectCommandsTokenCount", "pIndirectCommandsTokens",
               "maxSequencesCount", "targetCommandBuffer", "sequencesCountBuffer",
               "sequencesCountOffset", "sequencesIndexBuffer",
               "sequencesIndexOffset"]
        type CUnionType VkCmdProcessCommandsInfoNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCmdProcessCommandsInfoNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCmdProcessCommandsInfoNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkCmdProcessCommandsInfoNVX
         where
        type VkSTypeMType VkCmdProcessCommandsInfoNVX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkCmdProcessCommandsInfoNVX where
        type FieldType "sType" VkCmdProcessCommandsInfoNVX =
             VkStructureType
        type FieldOptional "sType" VkCmdProcessCommandsInfoNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCmdProcessCommandsInfoNVX =
             #{offset VkCmdProcessCommandsInfoNVX, sType}
        type FieldIsArray "sType" VkCmdProcessCommandsInfoNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, sType}

instance CanReadField "sType" VkCmdProcessCommandsInfoNVX where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkCmdProcessCommandsInfoNVX where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkCmdProcessCommandsInfoNVX
         where
        type VkPNextMType VkCmdProcessCommandsInfoNVX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCmdProcessCommandsInfoNVX where
        type FieldType "pNext" VkCmdProcessCommandsInfoNVX = Ptr Void
        type FieldOptional "pNext" VkCmdProcessCommandsInfoNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCmdProcessCommandsInfoNVX =
             #{offset VkCmdProcessCommandsInfoNVX, pNext}
        type FieldIsArray "pNext" VkCmdProcessCommandsInfoNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, pNext}

instance CanReadField "pNext" VkCmdProcessCommandsInfoNVX where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkCmdProcessCommandsInfoNVX where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkObjectTable VkCmdProcessCommandsInfoNVX where
        type VkObjectTableMType VkCmdProcessCommandsInfoNVX =
             VkObjectTableNVX

        {-# NOINLINE vkObjectTable #-}
        vkObjectTable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, objectTable})

        {-# INLINE vkObjectTableByteOffset #-}
        vkObjectTableByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, objectTable}

        {-# INLINE readVkObjectTable #-}
        readVkObjectTable p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, objectTable}

        {-# INLINE writeVkObjectTable #-}
        writeVkObjectTable p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, objectTable}

instance {-# OVERLAPPING #-}
         HasField "objectTable" VkCmdProcessCommandsInfoNVX where
        type FieldType "objectTable" VkCmdProcessCommandsInfoNVX =
             VkObjectTableNVX
        type FieldOptional "objectTable" VkCmdProcessCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "objectTable" VkCmdProcessCommandsInfoNVX =
             #{offset VkCmdProcessCommandsInfoNVX, objectTable}
        type FieldIsArray "objectTable" VkCmdProcessCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, objectTable}

instance CanReadField "objectTable" VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkObjectTable

        {-# INLINE readField #-}
        readField = readVkObjectTable

instance CanWriteField "objectTable" VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkObjectTable

instance {-# OVERLAPPING #-}
         HasVkIndirectCommandsLayout VkCmdProcessCommandsInfoNVX where
        type VkIndirectCommandsLayoutMType VkCmdProcessCommandsInfoNVX =
             VkIndirectCommandsLayoutNVX

        {-# NOINLINE vkIndirectCommandsLayout #-}
        vkIndirectCommandsLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsLayout})

        {-# INLINE vkIndirectCommandsLayoutByteOffset #-}
        vkIndirectCommandsLayoutByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsLayout}

        {-# INLINE readVkIndirectCommandsLayout #-}
        readVkIndirectCommandsLayout p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsLayout}

        {-# INLINE writeVkIndirectCommandsLayout #-}
        writeVkIndirectCommandsLayout p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsLayout}

instance {-# OVERLAPPING #-}
         HasField "indirectCommandsLayout" VkCmdProcessCommandsInfoNVX where
        type FieldType "indirectCommandsLayout" VkCmdProcessCommandsInfoNVX
             = VkIndirectCommandsLayoutNVX
        type FieldOptional "indirectCommandsLayout"
               VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "indirectCommandsLayout"
               VkCmdProcessCommandsInfoNVX
             =
             #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsLayout}
        type FieldIsArray "indirectCommandsLayout"
               VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsLayout}

instance CanReadField "indirectCommandsLayout"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkIndirectCommandsLayout

        {-# INLINE readField #-}
        readField = readVkIndirectCommandsLayout

instance CanWriteField "indirectCommandsLayout"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkIndirectCommandsLayout

instance {-# OVERLAPPING #-}
         HasVkIndirectCommandsTokenCount VkCmdProcessCommandsInfoNVX where
        type VkIndirectCommandsTokenCountMType VkCmdProcessCommandsInfoNVX
             = Word32

        {-# NOINLINE vkIndirectCommandsTokenCount #-}
        vkIndirectCommandsTokenCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsTokenCount})

        {-# INLINE vkIndirectCommandsTokenCountByteOffset #-}
        vkIndirectCommandsTokenCountByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsTokenCount}

        {-# INLINE readVkIndirectCommandsTokenCount #-}
        readVkIndirectCommandsTokenCount p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsTokenCount}

        {-# INLINE writeVkIndirectCommandsTokenCount #-}
        writeVkIndirectCommandsTokenCount p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsTokenCount}

instance {-# OVERLAPPING #-}
         HasField "indirectCommandsTokenCount" VkCmdProcessCommandsInfoNVX
         where
        type FieldType "indirectCommandsTokenCount"
               VkCmdProcessCommandsInfoNVX
             = Word32
        type FieldOptional "indirectCommandsTokenCount"
               VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "indirectCommandsTokenCount"
               VkCmdProcessCommandsInfoNVX
             =
             #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsTokenCount}
        type FieldIsArray "indirectCommandsTokenCount"
               VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsTokenCount}

instance CanReadField "indirectCommandsTokenCount"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkIndirectCommandsTokenCount

        {-# INLINE readField #-}
        readField = readVkIndirectCommandsTokenCount

instance CanWriteField "indirectCommandsTokenCount"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkIndirectCommandsTokenCount

instance {-# OVERLAPPING #-}
         HasVkPIndirectCommandsTokens VkCmdProcessCommandsInfoNVX where
        type VkPIndirectCommandsTokensMType VkCmdProcessCommandsInfoNVX =
             Ptr VkIndirectCommandsTokenNVX

        {-# NOINLINE vkPIndirectCommandsTokens #-}
        vkPIndirectCommandsTokens x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, pIndirectCommandsTokens})

        {-# INLINE vkPIndirectCommandsTokensByteOffset #-}
        vkPIndirectCommandsTokensByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, pIndirectCommandsTokens}

        {-# INLINE readVkPIndirectCommandsTokens #-}
        readVkPIndirectCommandsTokens p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, pIndirectCommandsTokens}

        {-# INLINE writeVkPIndirectCommandsTokens #-}
        writeVkPIndirectCommandsTokens p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, pIndirectCommandsTokens}

instance {-# OVERLAPPING #-}
         HasField "pIndirectCommandsTokens" VkCmdProcessCommandsInfoNVX
         where
        type FieldType "pIndirectCommandsTokens"
               VkCmdProcessCommandsInfoNVX
             = Ptr VkIndirectCommandsTokenNVX
        type FieldOptional "pIndirectCommandsTokens"
               VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pIndirectCommandsTokens"
               VkCmdProcessCommandsInfoNVX
             =
             #{offset VkCmdProcessCommandsInfoNVX, pIndirectCommandsTokens}
        type FieldIsArray "pIndirectCommandsTokens"
               VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, pIndirectCommandsTokens}

instance CanReadField "pIndirectCommandsTokens"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkPIndirectCommandsTokens

        {-# INLINE readField #-}
        readField = readVkPIndirectCommandsTokens

instance CanWriteField "pIndirectCommandsTokens"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPIndirectCommandsTokens

instance {-# OVERLAPPING #-}
         HasVkMaxSequencesCount VkCmdProcessCommandsInfoNVX where
        type VkMaxSequencesCountMType VkCmdProcessCommandsInfoNVX = Word32

        {-# NOINLINE vkMaxSequencesCount #-}
        vkMaxSequencesCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, maxSequencesCount})

        {-# INLINE vkMaxSequencesCountByteOffset #-}
        vkMaxSequencesCountByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, maxSequencesCount}

        {-# INLINE readVkMaxSequencesCount #-}
        readVkMaxSequencesCount p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, maxSequencesCount}

        {-# INLINE writeVkMaxSequencesCount #-}
        writeVkMaxSequencesCount p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, maxSequencesCount}

instance {-# OVERLAPPING #-}
         HasField "maxSequencesCount" VkCmdProcessCommandsInfoNVX where
        type FieldType "maxSequencesCount" VkCmdProcessCommandsInfoNVX =
             Word32
        type FieldOptional "maxSequencesCount" VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSequencesCount" VkCmdProcessCommandsInfoNVX =
             #{offset VkCmdProcessCommandsInfoNVX, maxSequencesCount}
        type FieldIsArray "maxSequencesCount" VkCmdProcessCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, maxSequencesCount}

instance CanReadField "maxSequencesCount"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkMaxSequencesCount

        {-# INLINE readField #-}
        readField = readVkMaxSequencesCount

instance CanWriteField "maxSequencesCount"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxSequencesCount

instance {-# OVERLAPPING #-}
         HasVkTargetCommandBuffer VkCmdProcessCommandsInfoNVX where
        type VkTargetCommandBufferMType VkCmdProcessCommandsInfoNVX =
             VkCommandBuffer

        {-# NOINLINE vkTargetCommandBuffer #-}
        vkTargetCommandBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, targetCommandBuffer})

        {-# INLINE vkTargetCommandBufferByteOffset #-}
        vkTargetCommandBufferByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, targetCommandBuffer}

        {-# INLINE readVkTargetCommandBuffer #-}
        readVkTargetCommandBuffer p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, targetCommandBuffer}

        {-# INLINE writeVkTargetCommandBuffer #-}
        writeVkTargetCommandBuffer p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, targetCommandBuffer}

instance {-# OVERLAPPING #-}
         HasField "targetCommandBuffer" VkCmdProcessCommandsInfoNVX where
        type FieldType "targetCommandBuffer" VkCmdProcessCommandsInfoNVX =
             VkCommandBuffer
        type FieldOptional "targetCommandBuffer"
               VkCmdProcessCommandsInfoNVX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "targetCommandBuffer" VkCmdProcessCommandsInfoNVX
             =
             #{offset VkCmdProcessCommandsInfoNVX, targetCommandBuffer}
        type FieldIsArray "targetCommandBuffer" VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, targetCommandBuffer}

instance CanReadField "targetCommandBuffer"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkTargetCommandBuffer

        {-# INLINE readField #-}
        readField = readVkTargetCommandBuffer

instance CanWriteField "targetCommandBuffer"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkTargetCommandBuffer

instance {-# OVERLAPPING #-}
         HasVkSequencesCountBuffer VkCmdProcessCommandsInfoNVX where
        type VkSequencesCountBufferMType VkCmdProcessCommandsInfoNVX =
             VkBuffer

        {-# NOINLINE vkSequencesCountBuffer #-}
        vkSequencesCountBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, sequencesCountBuffer})

        {-# INLINE vkSequencesCountBufferByteOffset #-}
        vkSequencesCountBufferByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, sequencesCountBuffer}

        {-# INLINE readVkSequencesCountBuffer #-}
        readVkSequencesCountBuffer p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesCountBuffer}

        {-# INLINE writeVkSequencesCountBuffer #-}
        writeVkSequencesCountBuffer p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesCountBuffer}

instance {-# OVERLAPPING #-}
         HasField "sequencesCountBuffer" VkCmdProcessCommandsInfoNVX where
        type FieldType "sequencesCountBuffer" VkCmdProcessCommandsInfoNVX =
             VkBuffer
        type FieldOptional "sequencesCountBuffer"
               VkCmdProcessCommandsInfoNVX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sequencesCountBuffer" VkCmdProcessCommandsInfoNVX
             =
             #{offset VkCmdProcessCommandsInfoNVX, sequencesCountBuffer}
        type FieldIsArray "sequencesCountBuffer"
               VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, sequencesCountBuffer}

instance CanReadField "sequencesCountBuffer"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkSequencesCountBuffer

        {-# INLINE readField #-}
        readField = readVkSequencesCountBuffer

instance CanWriteField "sequencesCountBuffer"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSequencesCountBuffer

instance {-# OVERLAPPING #-}
         HasVkSequencesCountOffset VkCmdProcessCommandsInfoNVX where
        type VkSequencesCountOffsetMType VkCmdProcessCommandsInfoNVX =
             VkDeviceSize

        {-# NOINLINE vkSequencesCountOffset #-}
        vkSequencesCountOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, sequencesCountOffset})

        {-# INLINE vkSequencesCountOffsetByteOffset #-}
        vkSequencesCountOffsetByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, sequencesCountOffset}

        {-# INLINE readVkSequencesCountOffset #-}
        readVkSequencesCountOffset p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesCountOffset}

        {-# INLINE writeVkSequencesCountOffset #-}
        writeVkSequencesCountOffset p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesCountOffset}

instance {-# OVERLAPPING #-}
         HasField "sequencesCountOffset" VkCmdProcessCommandsInfoNVX where
        type FieldType "sequencesCountOffset" VkCmdProcessCommandsInfoNVX =
             VkDeviceSize
        type FieldOptional "sequencesCountOffset"
               VkCmdProcessCommandsInfoNVX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sequencesCountOffset" VkCmdProcessCommandsInfoNVX
             =
             #{offset VkCmdProcessCommandsInfoNVX, sequencesCountOffset}
        type FieldIsArray "sequencesCountOffset"
               VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, sequencesCountOffset}

instance CanReadField "sequencesCountOffset"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkSequencesCountOffset

        {-# INLINE readField #-}
        readField = readVkSequencesCountOffset

instance CanWriteField "sequencesCountOffset"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSequencesCountOffset

instance {-# OVERLAPPING #-}
         HasVkSequencesIndexBuffer VkCmdProcessCommandsInfoNVX where
        type VkSequencesIndexBufferMType VkCmdProcessCommandsInfoNVX =
             VkBuffer

        {-# NOINLINE vkSequencesIndexBuffer #-}
        vkSequencesIndexBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexBuffer})

        {-# INLINE vkSequencesIndexBufferByteOffset #-}
        vkSequencesIndexBufferByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexBuffer}

        {-# INLINE readVkSequencesIndexBuffer #-}
        readVkSequencesIndexBuffer p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexBuffer}

        {-# INLINE writeVkSequencesIndexBuffer #-}
        writeVkSequencesIndexBuffer p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexBuffer}

instance {-# OVERLAPPING #-}
         HasField "sequencesIndexBuffer" VkCmdProcessCommandsInfoNVX where
        type FieldType "sequencesIndexBuffer" VkCmdProcessCommandsInfoNVX =
             VkBuffer
        type FieldOptional "sequencesIndexBuffer"
               VkCmdProcessCommandsInfoNVX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sequencesIndexBuffer" VkCmdProcessCommandsInfoNVX
             =
             #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexBuffer}
        type FieldIsArray "sequencesIndexBuffer"
               VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexBuffer}

instance CanReadField "sequencesIndexBuffer"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkSequencesIndexBuffer

        {-# INLINE readField #-}
        readField = readVkSequencesIndexBuffer

instance CanWriteField "sequencesIndexBuffer"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSequencesIndexBuffer

instance {-# OVERLAPPING #-}
         HasVkSequencesIndexOffset VkCmdProcessCommandsInfoNVX where
        type VkSequencesIndexOffsetMType VkCmdProcessCommandsInfoNVX =
             VkDeviceSize

        {-# NOINLINE vkSequencesIndexOffset #-}
        vkSequencesIndexOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexOffset})

        {-# INLINE vkSequencesIndexOffsetByteOffset #-}
        vkSequencesIndexOffsetByteOffset ~_
          = #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexOffset}

        {-# INLINE readVkSequencesIndexOffset #-}
        readVkSequencesIndexOffset p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexOffset}

        {-# INLINE writeVkSequencesIndexOffset #-}
        writeVkSequencesIndexOffset p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexOffset}

instance {-# OVERLAPPING #-}
         HasField "sequencesIndexOffset" VkCmdProcessCommandsInfoNVX where
        type FieldType "sequencesIndexOffset" VkCmdProcessCommandsInfoNVX =
             VkDeviceSize
        type FieldOptional "sequencesIndexOffset"
               VkCmdProcessCommandsInfoNVX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sequencesIndexOffset" VkCmdProcessCommandsInfoNVX
             =
             #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexOffset}
        type FieldIsArray "sequencesIndexOffset"
               VkCmdProcessCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexOffset}

instance CanReadField "sequencesIndexOffset"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkSequencesIndexOffset

        {-# INLINE readField #-}
        readField = readVkSequencesIndexOffset

instance CanWriteField "sequencesIndexOffset"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSequencesIndexOffset

instance Show VkCmdProcessCommandsInfoNVX where
        showsPrec d x
          = showString "VkCmdProcessCommandsInfoNVX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkObjectTable = " .
                            showsPrec d (vkObjectTable x) .
                              showString ", " .
                                showString "vkIndirectCommandsLayout = " .
                                  showsPrec d (vkIndirectCommandsLayout x) .
                                    showString ", " .
                                      showString "vkIndirectCommandsTokenCount = " .
                                        showsPrec d (vkIndirectCommandsTokenCount x) .
                                          showString ", " .
                                            showString "vkPIndirectCommandsTokens = " .
                                              showsPrec d (vkPIndirectCommandsTokens x) .
                                                showString ", " .
                                                  showString "vkMaxSequencesCount = " .
                                                    showsPrec d (vkMaxSequencesCount x) .
                                                      showString ", " .
                                                        showString "vkTargetCommandBuffer = " .
                                                          showsPrec d (vkTargetCommandBuffer x) .
                                                            showString ", " .
                                                              showString "vkSequencesCountBuffer = "
                                                                .
                                                                showsPrec d
                                                                  (vkSequencesCountBuffer x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkSequencesCountOffset = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkSequencesCountOffset x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkSequencesIndexBuffer = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkSequencesIndexBuffer
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkSequencesIndexOffset = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkSequencesIndexOffset
                                                                                       x)
                                                                                    . showChar '}'
