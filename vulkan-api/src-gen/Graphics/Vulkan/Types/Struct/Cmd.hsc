#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Cmd
       (VkCmdProcessCommandsInfoNVX(..),
        VkCmdReserveSpaceForCommandsInfoNVX(..))
       where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.StructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                 (VkBuffer,
                                                                VkCommandBuffer,
                                                                VkIndirectCommandsLayoutNVX,
                                                                VkObjectTableNVX)
import           Graphics.Vulkan.Types.Struct.IndirectCommands (VkIndirectCommandsTokenNVX)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCmdProcessCommandsInfoNVX VkCmdProcessCommandsInfoNVX registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCmdProcessCommandsInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCmdProcessCommandsInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCmdProcessCommandsInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCmdProcessCommandsInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "objectTable" VkCmdProcessCommandsInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, objectTable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, objectTable}

instance {-# OVERLAPPING #-}
         CanWriteField "objectTable" VkCmdProcessCommandsInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, objectTable}

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

instance {-# OVERLAPPING #-}
         CanReadField "indirectCommandsLayout" VkCmdProcessCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "indirectCommandsLayout" VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsLayout}

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

instance {-# OVERLAPPING #-}
         CanReadField "indirectCommandsTokenCount"
           VkCmdProcessCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsTokenCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsTokenCount}

instance {-# OVERLAPPING #-}
         CanWriteField "indirectCommandsTokenCount"
           VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, indirectCommandsTokenCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pIndirectCommandsTokens" VkCmdProcessCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, pIndirectCommandsTokens})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, pIndirectCommandsTokens}

instance {-# OVERLAPPING #-}
         CanWriteField "pIndirectCommandsTokens" VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, pIndirectCommandsTokens}

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

instance {-# OVERLAPPING #-}
         CanReadField "maxSequencesCount" VkCmdProcessCommandsInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, maxSequencesCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, maxSequencesCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSequencesCount" VkCmdProcessCommandsInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, maxSequencesCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "targetCommandBuffer" VkCmdProcessCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, targetCommandBuffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, targetCommandBuffer}

instance {-# OVERLAPPING #-}
         CanWriteField "targetCommandBuffer" VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, targetCommandBuffer}

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

instance {-# OVERLAPPING #-}
         CanReadField "sequencesCountBuffer" VkCmdProcessCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, sequencesCountBuffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesCountBuffer}

instance {-# OVERLAPPING #-}
         CanWriteField "sequencesCountBuffer" VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesCountBuffer}

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

instance {-# OVERLAPPING #-}
         CanReadField "sequencesCountOffset" VkCmdProcessCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, sequencesCountOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesCountOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "sequencesCountOffset" VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesCountOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "sequencesIndexBuffer" VkCmdProcessCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexBuffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexBuffer}

instance {-# OVERLAPPING #-}
         CanWriteField "sequencesIndexBuffer" VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexBuffer}

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

instance {-# OVERLAPPING #-}
         CanReadField "sequencesIndexOffset" VkCmdProcessCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "sequencesIndexOffset" VkCmdProcessCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdProcessCommandsInfoNVX, sequencesIndexOffset}

instance Show VkCmdProcessCommandsInfoNVX where
        showsPrec d x
          = showString "VkCmdProcessCommandsInfoNVX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "objectTable = " .
                            showsPrec d (getField @"objectTable" x) .
                              showString ", " .
                                showString "indirectCommandsLayout = " .
                                  showsPrec d (getField @"indirectCommandsLayout" x) .
                                    showString ", " .
                                      showString "indirectCommandsTokenCount = " .
                                        showsPrec d (getField @"indirectCommandsTokenCount" x) .
                                          showString ", " .
                                            showString "pIndirectCommandsTokens = " .
                                              showsPrec d (getField @"pIndirectCommandsTokens" x) .
                                                showString ", " .
                                                  showString "maxSequencesCount = " .
                                                    showsPrec d (getField @"maxSequencesCount" x) .
                                                      showString ", " .
                                                        showString "targetCommandBuffer = " .
                                                          showsPrec d
                                                            (getField @"targetCommandBuffer" x)
                                                            .
                                                            showString ", " .
                                                              showString "sequencesCountBuffer = " .
                                                                showsPrec d
                                                                  (getField @"sequencesCountBuffer"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "sequencesCountOffset = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"sequencesCountOffset"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "sequencesIndexBuffer = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"sequencesIndexBuffer"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "sequencesIndexOffset = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"sequencesIndexOffset"
                                                                                       x)
                                                                                    . showChar '}'

-- | > typedef struct VkCmdReserveSpaceForCommandsInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkObjectTableNVX                                         objectTable;
--   >     VkIndirectCommandsLayoutNVX                              indirectCommandsLayout;
--   >     uint32_t                                                 maxSequencesCount;
--   > } VkCmdReserveSpaceForCommandsInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCmdReserveSpaceForCommandsInfoNVX VkCmdReserveSpaceForCommandsInfoNVX registry at www.khronos.org>
data VkCmdReserveSpaceForCommandsInfoNVX = VkCmdReserveSpaceForCommandsInfoNVX## Addr##
                                                                                ByteArray##

instance Eq VkCmdReserveSpaceForCommandsInfoNVX where
        (VkCmdReserveSpaceForCommandsInfoNVX## a _) ==
          x@(VkCmdReserveSpaceForCommandsInfoNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCmdReserveSpaceForCommandsInfoNVX where
        (VkCmdReserveSpaceForCommandsInfoNVX## a _) `compare`
          x@(VkCmdReserveSpaceForCommandsInfoNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCmdReserveSpaceForCommandsInfoNVX where
        sizeOf ~_ = #{size VkCmdReserveSpaceForCommandsInfoNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkCmdReserveSpaceForCommandsInfoNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCmdReserveSpaceForCommandsInfoNVX
         where
        unsafeAddr (VkCmdReserveSpaceForCommandsInfoNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCmdReserveSpaceForCommandsInfoNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCmdReserveSpaceForCommandsInfoNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCmdReserveSpaceForCommandsInfoNVX where
        type StructFields VkCmdReserveSpaceForCommandsInfoNVX =
             '["sType", "pNext", "objectTable", "indirectCommandsLayout", -- ' closing tick for hsc2hs
               "maxSequencesCount"]
        type CUnionType VkCmdReserveSpaceForCommandsInfoNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCmdReserveSpaceForCommandsInfoNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCmdReserveSpaceForCommandsInfoNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkCmdReserveSpaceForCommandsInfoNVX where
        type FieldType "sType" VkCmdReserveSpaceForCommandsInfoNVX =
             VkStructureType
        type FieldOptional "sType" VkCmdReserveSpaceForCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCmdReserveSpaceForCommandsInfoNVX =
             #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}
        type FieldIsArray "sType" VkCmdReserveSpaceForCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCmdReserveSpaceForCommandsInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCmdReserveSpaceForCommandsInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCmdReserveSpaceForCommandsInfoNVX where
        type FieldType "pNext" VkCmdReserveSpaceForCommandsInfoNVX =
             Ptr Void
        type FieldOptional "pNext" VkCmdReserveSpaceForCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCmdReserveSpaceForCommandsInfoNVX =
             #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}
        type FieldIsArray "pNext" VkCmdReserveSpaceForCommandsInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCmdReserveSpaceForCommandsInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCmdReserveSpaceForCommandsInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "objectTable" VkCmdReserveSpaceForCommandsInfoNVX where
        type FieldType "objectTable" VkCmdReserveSpaceForCommandsInfoNVX =
             VkObjectTableNVX
        type FieldOptional "objectTable"
               VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "objectTable" VkCmdReserveSpaceForCommandsInfoNVX
             =
             #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}
        type FieldIsArray "objectTable" VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}

instance {-# OVERLAPPING #-}
         CanReadField "objectTable" VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}

instance {-# OVERLAPPING #-}
         CanWriteField "objectTable" VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}

instance {-# OVERLAPPING #-}
         HasField "indirectCommandsLayout"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        type FieldType "indirectCommandsLayout"
               VkCmdReserveSpaceForCommandsInfoNVX
             = VkIndirectCommandsLayoutNVX
        type FieldOptional "indirectCommandsLayout"
               VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "indirectCommandsLayout"
               VkCmdReserveSpaceForCommandsInfoNVX
             =
             #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}
        type FieldIsArray "indirectCommandsLayout"
               VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}

instance {-# OVERLAPPING #-}
         CanReadField "indirectCommandsLayout"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "indirectCommandsLayout"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}

instance {-# OVERLAPPING #-}
         HasField "maxSequencesCount" VkCmdReserveSpaceForCommandsInfoNVX
         where
        type FieldType "maxSequencesCount"
               VkCmdReserveSpaceForCommandsInfoNVX
             = Word32
        type FieldOptional "maxSequencesCount"
               VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSequencesCount"
               VkCmdReserveSpaceForCommandsInfoNVX
             =
             #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}
        type FieldIsArray "maxSequencesCount"
               VkCmdReserveSpaceForCommandsInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxSequencesCount"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSequencesCount"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}

instance Show VkCmdReserveSpaceForCommandsInfoNVX where
        showsPrec d x
          = showString "VkCmdReserveSpaceForCommandsInfoNVX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "objectTable = " .
                            showsPrec d (getField @"objectTable" x) .
                              showString ", " .
                                showString "indirectCommandsLayout = " .
                                  showsPrec d (getField @"indirectCommandsLayout" x) .
                                    showString ", " .
                                      showString "maxSequencesCount = " .
                                        showsPrec d (getField @"maxSequencesCount" x) . showChar '}'
