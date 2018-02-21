#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGeneratedCommandsLimitsNVX
       (VkDeviceGeneratedCommandsLimitsNVX(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGeneratedCommandsLimitsNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         maxIndirectCommandsLayoutTokenCount;
--   >     uint32_t                         maxObjectEntryCounts;
--   >     uint32_t                         minSequenceCountBufferOffsetAlignment;
--   >     uint32_t                         minSequenceIndexBufferOffsetAlignment;
--   >     uint32_t                         minCommandsTokenBufferOffsetAlignment;
--   > } VkDeviceGeneratedCommandsLimitsNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGeneratedCommandsLimitsNVX.html VkDeviceGeneratedCommandsLimitsNVX registry at www.khronos.org>
data VkDeviceGeneratedCommandsLimitsNVX = VkDeviceGeneratedCommandsLimitsNVX## Addr##
                                                                              ByteArray##

instance Eq VkDeviceGeneratedCommandsLimitsNVX where
        (VkDeviceGeneratedCommandsLimitsNVX## a _) ==
          x@(VkDeviceGeneratedCommandsLimitsNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGeneratedCommandsLimitsNVX where
        (VkDeviceGeneratedCommandsLimitsNVX## a _) `compare`
          x@(VkDeviceGeneratedCommandsLimitsNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGeneratedCommandsLimitsNVX where
        sizeOf ~_ = #{size VkDeviceGeneratedCommandsLimitsNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGeneratedCommandsLimitsNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGeneratedCommandsLimitsNVX where
        unsafeAddr (VkDeviceGeneratedCommandsLimitsNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGeneratedCommandsLimitsNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGeneratedCommandsLimitsNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGeneratedCommandsLimitsNVX where
        type StructFields VkDeviceGeneratedCommandsLimitsNVX =
             '["sType", "pNext", "maxIndirectCommandsLayoutTokenCount", -- ' closing tick for hsc2hs
               "maxObjectEntryCounts", "minSequenceCountBufferOffsetAlignment",
               "minSequenceIndexBufferOffsetAlignment",
               "minCommandsTokenBufferOffsetAlignment"]
        type CUnionType VkDeviceGeneratedCommandsLimitsNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGeneratedCommandsLimitsNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGeneratedCommandsLimitsNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGeneratedCommandsLimitsNVX where
        type VkSTypeMType VkDeviceGeneratedCommandsLimitsNVX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGeneratedCommandsLimitsNVX where
        type FieldType "sType" VkDeviceGeneratedCommandsLimitsNVX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGeneratedCommandsLimitsNVX =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}
        type FieldIsArray "sType" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}

instance CanReadField "sType" VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGeneratedCommandsLimitsNVX where
        type VkPNextMType VkDeviceGeneratedCommandsLimitsNVX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGeneratedCommandsLimitsNVX where
        type FieldType "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}
        type FieldIsArray "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}

instance CanReadField "pNext" VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMaxIndirectCommandsLayoutTokenCount
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type VkMaxIndirectCommandsLayoutTokenCountMType
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32

        {-# NOINLINE vkMaxIndirectCommandsLayoutTokenCount #-}
        vkMaxIndirectCommandsLayoutTokenCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount})

        {-# INLINE vkMaxIndirectCommandsLayoutTokenCountByteOffset #-}
        vkMaxIndirectCommandsLayoutTokenCountByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}

        {-# INLINE readVkMaxIndirectCommandsLayoutTokenCount #-}
        readVkMaxIndirectCommandsLayoutTokenCount p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}

        {-# INLINE writeVkMaxIndirectCommandsLayoutTokenCount #-}
        writeVkMaxIndirectCommandsLayoutTokenCount p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}

instance {-# OVERLAPPING #-}
         HasField "maxIndirectCommandsLayoutTokenCount"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}
        type FieldIsArray "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}

instance CanReadField "maxIndirectCommandsLayoutTokenCount"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE getField #-}
        getField = vkMaxIndirectCommandsLayoutTokenCount

        {-# INLINE readField #-}
        readField = readVkMaxIndirectCommandsLayoutTokenCount

instance CanWriteField "maxIndirectCommandsLayoutTokenCount"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxIndirectCommandsLayoutTokenCount

instance {-# OVERLAPPING #-}
         HasVkMaxObjectEntryCounts VkDeviceGeneratedCommandsLimitsNVX where
        type VkMaxObjectEntryCountsMType VkDeviceGeneratedCommandsLimitsNVX
             = Word32

        {-# NOINLINE vkMaxObjectEntryCounts #-}
        vkMaxObjectEntryCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts})

        {-# INLINE vkMaxObjectEntryCountsByteOffset #-}
        vkMaxObjectEntryCountsByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}

        {-# INLINE readVkMaxObjectEntryCounts #-}
        readVkMaxObjectEntryCounts p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}

        {-# INLINE writeVkMaxObjectEntryCounts #-}
        writeVkMaxObjectEntryCounts p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}

instance {-# OVERLAPPING #-}
         HasField "maxObjectEntryCounts" VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}
        type FieldIsArray "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}

instance CanReadField "maxObjectEntryCounts"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE getField #-}
        getField = vkMaxObjectEntryCounts

        {-# INLINE readField #-}
        readField = readVkMaxObjectEntryCounts

instance CanWriteField "maxObjectEntryCounts"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxObjectEntryCounts

instance {-# OVERLAPPING #-}
         HasVkMinSequenceCountBufferOffsetAlignment
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type VkMinSequenceCountBufferOffsetAlignmentMType
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32

        {-# NOINLINE vkMinSequenceCountBufferOffsetAlignment #-}
        vkMinSequenceCountBufferOffsetAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment})

        {-# INLINE vkMinSequenceCountBufferOffsetAlignmentByteOffset #-}
        vkMinSequenceCountBufferOffsetAlignmentByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}

        {-# INLINE readVkMinSequenceCountBufferOffsetAlignment #-}
        readVkMinSequenceCountBufferOffsetAlignment p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}

        {-# INLINE writeVkMinSequenceCountBufferOffsetAlignment #-}
        writeVkMinSequenceCountBufferOffsetAlignment p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "minSequenceCountBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}
        type FieldIsArray "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}

instance CanReadField "minSequenceCountBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE getField #-}
        getField = vkMinSequenceCountBufferOffsetAlignment

        {-# INLINE readField #-}
        readField = readVkMinSequenceCountBufferOffsetAlignment

instance CanWriteField "minSequenceCountBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinSequenceCountBufferOffsetAlignment

instance {-# OVERLAPPING #-}
         HasVkMinSequenceIndexBufferOffsetAlignment
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type VkMinSequenceIndexBufferOffsetAlignmentMType
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32

        {-# NOINLINE vkMinSequenceIndexBufferOffsetAlignment #-}
        vkMinSequenceIndexBufferOffsetAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment})

        {-# INLINE vkMinSequenceIndexBufferOffsetAlignmentByteOffset #-}
        vkMinSequenceIndexBufferOffsetAlignmentByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}

        {-# INLINE readVkMinSequenceIndexBufferOffsetAlignment #-}
        readVkMinSequenceIndexBufferOffsetAlignment p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}

        {-# INLINE writeVkMinSequenceIndexBufferOffsetAlignment #-}
        writeVkMinSequenceIndexBufferOffsetAlignment p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "minSequenceIndexBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}
        type FieldIsArray "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}

instance CanReadField "minSequenceIndexBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE getField #-}
        getField = vkMinSequenceIndexBufferOffsetAlignment

        {-# INLINE readField #-}
        readField = readVkMinSequenceIndexBufferOffsetAlignment

instance CanWriteField "minSequenceIndexBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinSequenceIndexBufferOffsetAlignment

instance {-# OVERLAPPING #-}
         HasVkMinCommandsTokenBufferOffsetAlignment
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type VkMinCommandsTokenBufferOffsetAlignmentMType
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32

        {-# NOINLINE vkMinCommandsTokenBufferOffsetAlignment #-}
        vkMinCommandsTokenBufferOffsetAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment})

        {-# INLINE vkMinCommandsTokenBufferOffsetAlignmentByteOffset #-}
        vkMinCommandsTokenBufferOffsetAlignmentByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}

        {-# INLINE readVkMinCommandsTokenBufferOffsetAlignment #-}
        readVkMinCommandsTokenBufferOffsetAlignment p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}

        {-# INLINE writeVkMinCommandsTokenBufferOffsetAlignment #-}
        writeVkMinCommandsTokenBufferOffsetAlignment p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "minCommandsTokenBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}
        type FieldIsArray "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}

instance CanReadField "minCommandsTokenBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE getField #-}
        getField = vkMinCommandsTokenBufferOffsetAlignment

        {-# INLINE readField #-}
        readField = readVkMinCommandsTokenBufferOffsetAlignment

instance CanWriteField "minCommandsTokenBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinCommandsTokenBufferOffsetAlignment

instance Show VkDeviceGeneratedCommandsLimitsNVX where
        showsPrec d x
          = showString "VkDeviceGeneratedCommandsLimitsNVX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMaxIndirectCommandsLayoutTokenCount = " .
                            showsPrec d (vkMaxIndirectCommandsLayoutTokenCount x) .
                              showString ", " .
                                showString "vkMaxObjectEntryCounts = " .
                                  showsPrec d (vkMaxObjectEntryCounts x) .
                                    showString ", " .
                                      showString "vkMinSequenceCountBufferOffsetAlignment = " .
                                        showsPrec d (vkMinSequenceCountBufferOffsetAlignment x) .
                                          showString ", " .
                                            showString "vkMinSequenceIndexBufferOffsetAlignment = "
                                              .
                                              showsPrec d
                                                (vkMinSequenceIndexBufferOffsetAlignment x)
                                                .
                                                showString ", " .
                                                  showString
                                                    "vkMinCommandsTokenBufferOffsetAlignment = "
                                                    .
                                                    showsPrec d
                                                      (vkMinCommandsTokenBufferOffsetAlignment x)
                                                      . showChar '}'
