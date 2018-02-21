#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCmdReserveSpaceForCommandsInfoNVX
       (VkCmdReserveSpaceForCommandsInfoNVX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkIndirectCommandsLayoutNVX,
                                                             VkObjectTableNVX)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkCmdReserveSpaceForCommandsInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkObjectTableNVX                                         objectTable;
--   >     VkIndirectCommandsLayoutNVX                              indirectCommandsLayout;
--   >     uint32_t                                                 maxSequencesCount;
--   > } VkCmdReserveSpaceForCommandsInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkCmdReserveSpaceForCommandsInfoNVX.html VkCmdReserveSpaceForCommandsInfoNVX registry at www.khronos.org>
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
         HasVkSType VkCmdReserveSpaceForCommandsInfoNVX where
        type VkSTypeMType VkCmdReserveSpaceForCommandsInfoNVX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, sType}

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

instance CanReadField "sType" VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkCmdReserveSpaceForCommandsInfoNVX where
        type VkPNextMType VkCmdReserveSpaceForCommandsInfoNVX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, pNext}

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

instance CanReadField "pNext" VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkObjectTable VkCmdReserveSpaceForCommandsInfoNVX where
        type VkObjectTableMType VkCmdReserveSpaceForCommandsInfoNVX =
             VkObjectTableNVX

        {-# NOINLINE vkObjectTable #-}
        vkObjectTable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable})

        {-# INLINE vkObjectTableByteOffset #-}
        vkObjectTableByteOffset ~_
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}

        {-# INLINE readVkObjectTable #-}
        readVkObjectTable p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}

        {-# INLINE writeVkObjectTable #-}
        writeVkObjectTable p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, objectTable}

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

instance CanReadField "objectTable"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkObjectTable

        {-# INLINE readField #-}
        readField = readVkObjectTable

instance CanWriteField "objectTable"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkObjectTable

instance {-# OVERLAPPING #-}
         HasVkIndirectCommandsLayout VkCmdReserveSpaceForCommandsInfoNVX
         where
        type VkIndirectCommandsLayoutMType
               VkCmdReserveSpaceForCommandsInfoNVX
             = VkIndirectCommandsLayoutNVX

        {-# NOINLINE vkIndirectCommandsLayout #-}
        vkIndirectCommandsLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout})

        {-# INLINE vkIndirectCommandsLayoutByteOffset #-}
        vkIndirectCommandsLayoutByteOffset ~_
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}

        {-# INLINE readVkIndirectCommandsLayout #-}
        readVkIndirectCommandsLayout p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}

        {-# INLINE writeVkIndirectCommandsLayout #-}
        writeVkIndirectCommandsLayout p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, indirectCommandsLayout}

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

instance CanReadField "indirectCommandsLayout"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkIndirectCommandsLayout

        {-# INLINE readField #-}
        readField = readVkIndirectCommandsLayout

instance CanWriteField "indirectCommandsLayout"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkIndirectCommandsLayout

instance {-# OVERLAPPING #-}
         HasVkMaxSequencesCount VkCmdReserveSpaceForCommandsInfoNVX where
        type VkMaxSequencesCountMType VkCmdReserveSpaceForCommandsInfoNVX =
             Word32

        {-# NOINLINE vkMaxSequencesCount #-}
        vkMaxSequencesCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount})

        {-# INLINE vkMaxSequencesCountByteOffset #-}
        vkMaxSequencesCountByteOffset ~_
          = #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}

        {-# INLINE readVkMaxSequencesCount #-}
        readVkMaxSequencesCount p
          = peekByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}

        {-# INLINE writeVkMaxSequencesCount #-}
        writeVkMaxSequencesCount p
          = pokeByteOff p #{offset VkCmdReserveSpaceForCommandsInfoNVX, maxSequencesCount}

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

instance CanReadField "maxSequencesCount"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkMaxSequencesCount

        {-# INLINE readField #-}
        readField = readVkMaxSequencesCount

instance CanWriteField "maxSequencesCount"
           VkCmdReserveSpaceForCommandsInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxSequencesCount

instance Show VkCmdReserveSpaceForCommandsInfoNVX where
        showsPrec d x
          = showString "VkCmdReserveSpaceForCommandsInfoNVX {" .
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
                                      showString "vkMaxSequencesCount = " .
                                        showsPrec d (vkMaxSequencesCount x) . showChar '}'
