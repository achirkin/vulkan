#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NVX_device_generated_commands
       (-- * Vulkan extension: @VK_NVX_device_generated_commands@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Christoph Kubisch @pixeljetstream@
        --
        -- author: @NVX@
        --
        -- type: @device@
        --
        -- Extension number: @87@
        VkDeviceGeneratedCommandsFeaturesNVX(..),
        VkDeviceGeneratedCommandsLimitsNVX(..),
        VkIndirectCommandsTokenNVX(..),
        VkIndirectCommandsLayoutTokenNVX(..),
        VkIndirectCommandsLayoutCreateInfoNVX(..),
        VkCmdProcessCommandsInfoNVX(..),
        VkCmdReserveSpaceForCommandsInfoNVX(..),
        VkObjectTableCreateInfoNVX(..), VkObjectTableEntryNVX(..),
        VkObjectTablePipelineEntryNVX(..),
        VkObjectTableDescriptorSetEntryNVX(..),
        VkObjectTableVertexBufferEntryNVX(..),
        VkObjectTableIndexBufferEntryNVX(..),
        VkObjectTablePushConstantEntryNVX(..), vkCmdProcessCommandsNVX,
        vkCmdReserveSpaceForCommandsNVX, vkCreateIndirectCommandsLayoutNVX,
        vkDestroyIndirectCommandsLayoutNVX, vkCreateObjectTableNVX,
        vkDestroyObjectTableNVX, vkRegisterObjectsNVX,
        vkUnregisterObjectsNVX,
        vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX,
        VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION,
        pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION,
        VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME,
        pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX,
        pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX,
        pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX,
        pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX,
        pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX,
        pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX,
        pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX,
        pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX,
        pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX,
        pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGeneratedCommandsFeaturesNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         computeBindingPointSupport;
--   > } VkDeviceGeneratedCommandsFeaturesNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGeneratedCommandsFeaturesNVX.html VkDeviceGeneratedCommandsFeaturesNVX registry at www.khronos.org>
data VkDeviceGeneratedCommandsFeaturesNVX = VkDeviceGeneratedCommandsFeaturesNVX## ByteArray##

instance Eq VkDeviceGeneratedCommandsFeaturesNVX where
        (VkDeviceGeneratedCommandsFeaturesNVX## a) ==
          (VkDeviceGeneratedCommandsFeaturesNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGeneratedCommandsFeaturesNVX where
        (VkDeviceGeneratedCommandsFeaturesNVX## a) `compare`
          (VkDeviceGeneratedCommandsFeaturesNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceGeneratedCommandsFeaturesNVX where
        sizeOf ~_
          = #{size VkDeviceGeneratedCommandsFeaturesNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGeneratedCommandsFeaturesNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkDeviceGeneratedCommandsFeaturesNVX),
            I## a <- alignment
                      (undefined :: VkDeviceGeneratedCommandsFeaturesNVX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceGeneratedCommandsFeaturesNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceGeneratedCommandsFeaturesNVX## ba)
          | I## n <- sizeOf
                      (undefined :: VkDeviceGeneratedCommandsFeaturesNVX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceGeneratedCommandsFeaturesNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkDeviceGeneratedCommandsFeaturesNVX),
            I## a <- alignment
                      (undefined :: VkDeviceGeneratedCommandsFeaturesNVX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceGeneratedCommandsFeaturesNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceGeneratedCommandsFeaturesNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDeviceGeneratedCommandsFeaturesNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceGeneratedCommandsFeaturesNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceGeneratedCommandsFeaturesNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceGeneratedCommandsFeaturesNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDeviceGeneratedCommandsFeaturesNVX where
        type VkSTypeMType VkDeviceGeneratedCommandsFeaturesNVX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkDeviceGeneratedCommandsFeaturesNVX where
        type VkPNextMType VkDeviceGeneratedCommandsFeaturesNVX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

instance {-# OVERLAPPING #-}
         HasVkComputeBindingPointSupport
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        type VkComputeBindingPointSupportMType
               VkDeviceGeneratedCommandsFeaturesNVX
             = VkBool32

        {-# NOINLINE vkComputeBindingPointSupport #-}
        vkComputeBindingPointSupport x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport})

        {-# INLINE vkComputeBindingPointSupportByteOffset #-}
        vkComputeBindingPointSupportByteOffset ~_
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

        {-# INLINE readVkComputeBindingPointSupport #-}
        readVkComputeBindingPointSupport p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

        {-# INLINE writeVkComputeBindingPointSupport #-}
        writeVkComputeBindingPointSupport p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

instance Show VkDeviceGeneratedCommandsFeaturesNVX where
        showsPrec d x
          = showString "VkDeviceGeneratedCommandsFeaturesNVX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkComputeBindingPointSupport = " .
                            showsPrec d (vkComputeBindingPointSupport x) . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDeviceGeneratedCommandsLimitsNVX.html VkDeviceGeneratedCommandsLimitsNVX registry at www.khronos.org>
data VkDeviceGeneratedCommandsLimitsNVX = VkDeviceGeneratedCommandsLimitsNVX## ByteArray##

instance Eq VkDeviceGeneratedCommandsLimitsNVX where
        (VkDeviceGeneratedCommandsLimitsNVX## a) ==
          (VkDeviceGeneratedCommandsLimitsNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGeneratedCommandsLimitsNVX where
        (VkDeviceGeneratedCommandsLimitsNVX## a) `compare`
          (VkDeviceGeneratedCommandsLimitsNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDeviceGeneratedCommandsLimitsNVX where
        sizeOf ~_ = #{size VkDeviceGeneratedCommandsLimitsNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGeneratedCommandsLimitsNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDeviceGeneratedCommandsLimitsNVX),
            I## a <- alignment (undefined :: VkDeviceGeneratedCommandsLimitsNVX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDeviceGeneratedCommandsLimitsNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDeviceGeneratedCommandsLimitsNVX## ba)
          | I## n <- sizeOf (undefined :: VkDeviceGeneratedCommandsLimitsNVX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDeviceGeneratedCommandsLimitsNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDeviceGeneratedCommandsLimitsNVX),
            I## a <- alignment (undefined :: VkDeviceGeneratedCommandsLimitsNVX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDeviceGeneratedCommandsLimitsNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDeviceGeneratedCommandsLimitsNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDeviceGeneratedCommandsLimitsNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDeviceGeneratedCommandsLimitsNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDeviceGeneratedCommandsLimitsNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDeviceGeneratedCommandsLimitsNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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

-- | > typedef struct VkIndirectCommandsTokenNVX {
--   >     VkIndirectCommandsTokenTypeNVX      tokenType;
--   >     VkBuffer                         buffer;
--   >     VkDeviceSize                     offset;
--   > } VkIndirectCommandsTokenNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkIndirectCommandsTokenNVX.html VkIndirectCommandsTokenNVX registry at www.khronos.org>
data VkIndirectCommandsTokenNVX = VkIndirectCommandsTokenNVX## ByteArray##

instance Eq VkIndirectCommandsTokenNVX where
        (VkIndirectCommandsTokenNVX## a) == (VkIndirectCommandsTokenNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkIndirectCommandsTokenNVX where
        (VkIndirectCommandsTokenNVX## a) `compare`
          (VkIndirectCommandsTokenNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkIndirectCommandsTokenNVX where
        sizeOf ~_ = #{size VkIndirectCommandsTokenNVX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkIndirectCommandsTokenNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkIndirectCommandsTokenNVX),
            I## a <- alignment (undefined :: VkIndirectCommandsTokenNVX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkIndirectCommandsTokenNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkIndirectCommandsTokenNVX## ba)
          | I## n <- sizeOf (undefined :: VkIndirectCommandsTokenNVX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkIndirectCommandsTokenNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkIndirectCommandsTokenNVX),
            I## a <- alignment (undefined :: VkIndirectCommandsTokenNVX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkIndirectCommandsTokenNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkIndirectCommandsTokenNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkIndirectCommandsTokenNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkIndirectCommandsTokenNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkIndirectCommandsTokenNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkIndirectCommandsTokenNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkTokenType VkIndirectCommandsTokenNVX where
        type VkTokenTypeMType VkIndirectCommandsTokenNVX =
             VkIndirectCommandsTokenTypeNVX

        {-# NOINLINE vkTokenType #-}
        vkTokenType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, tokenType})

        {-# INLINE vkTokenTypeByteOffset #-}
        vkTokenTypeByteOffset ~_
          = #{offset VkIndirectCommandsTokenNVX, tokenType}

        {-# INLINE readVkTokenType #-}
        readVkTokenType p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, tokenType}

        {-# INLINE writeVkTokenType #-}
        writeVkTokenType p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, tokenType}

instance {-# OVERLAPPING #-} HasVkBuffer VkIndirectCommandsTokenNVX
         where
        type VkBufferMType VkIndirectCommandsTokenNVX = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkIndirectCommandsTokenNVX, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, buffer}

instance {-# OVERLAPPING #-} HasVkOffset VkIndirectCommandsTokenNVX
         where
        type VkOffsetMType VkIndirectCommandsTokenNVX = VkDeviceSize

        {-# NOINLINE vkOffset #-}
        vkOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, offset})

        {-# INLINE vkOffsetByteOffset #-}
        vkOffsetByteOffset ~_
          = #{offset VkIndirectCommandsTokenNVX, offset}

        {-# INLINE readVkOffset #-}
        readVkOffset p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, offset}

        {-# INLINE writeVkOffset #-}
        writeVkOffset p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, offset}

instance Show VkIndirectCommandsTokenNVX where
        showsPrec d x
          = showString "VkIndirectCommandsTokenNVX {" .
              showString "vkTokenType = " .
                showsPrec d (vkTokenType x) .
                  showString ", " .
                    showString "vkBuffer = " .
                      showsPrec d (vkBuffer x) .
                        showString ", " .
                          showString "vkOffset = " . showsPrec d (vkOffset x) . showChar '}'

-- | > typedef struct VkIndirectCommandsLayoutTokenNVX {
--   >     VkIndirectCommandsTokenTypeNVX      tokenType;
--   >     uint32_t                         bindingUnit;
--   >     uint32_t                         dynamicCount;
--   >     uint32_t                         divisor;
--   > } VkIndirectCommandsLayoutTokenNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkIndirectCommandsLayoutTokenNVX.html VkIndirectCommandsLayoutTokenNVX registry at www.khronos.org>
data VkIndirectCommandsLayoutTokenNVX = VkIndirectCommandsLayoutTokenNVX## ByteArray##

instance Eq VkIndirectCommandsLayoutTokenNVX where
        (VkIndirectCommandsLayoutTokenNVX## a) ==
          (VkIndirectCommandsLayoutTokenNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkIndirectCommandsLayoutTokenNVX where
        (VkIndirectCommandsLayoutTokenNVX## a) `compare`
          (VkIndirectCommandsLayoutTokenNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkIndirectCommandsLayoutTokenNVX where
        sizeOf ~_ = #{size VkIndirectCommandsLayoutTokenNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkIndirectCommandsLayoutTokenNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkIndirectCommandsLayoutTokenNVX),
            I## a <- alignment (undefined :: VkIndirectCommandsLayoutTokenNVX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkIndirectCommandsLayoutTokenNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkIndirectCommandsLayoutTokenNVX## ba)
          | I## n <- sizeOf (undefined :: VkIndirectCommandsLayoutTokenNVX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkIndirectCommandsLayoutTokenNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkIndirectCommandsLayoutTokenNVX),
            I## a <- alignment (undefined :: VkIndirectCommandsLayoutTokenNVX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkIndirectCommandsLayoutTokenNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkIndirectCommandsLayoutTokenNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkIndirectCommandsLayoutTokenNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkIndirectCommandsLayoutTokenNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkIndirectCommandsLayoutTokenNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkIndirectCommandsLayoutTokenNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkTokenType VkIndirectCommandsLayoutTokenNVX where
        type VkTokenTypeMType VkIndirectCommandsLayoutTokenNVX =
             VkIndirectCommandsTokenTypeNVX

        {-# NOINLINE vkTokenType #-}
        vkTokenType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, tokenType})

        {-# INLINE vkTokenTypeByteOffset #-}
        vkTokenTypeByteOffset ~_
          = #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}

        {-# INLINE readVkTokenType #-}
        readVkTokenType p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}

        {-# INLINE writeVkTokenType #-}
        writeVkTokenType p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}

instance {-# OVERLAPPING #-}
         HasVkBindingUnit VkIndirectCommandsLayoutTokenNVX where
        type VkBindingUnitMType VkIndirectCommandsLayoutTokenNVX = Word32

        {-# NOINLINE vkBindingUnit #-}
        vkBindingUnit x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit})

        {-# INLINE vkBindingUnitByteOffset #-}
        vkBindingUnitByteOffset ~_
          = #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}

        {-# INLINE readVkBindingUnit #-}
        readVkBindingUnit p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}

        {-# INLINE writeVkBindingUnit #-}
        writeVkBindingUnit p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}

instance {-# OVERLAPPING #-}
         HasVkDynamicCount VkIndirectCommandsLayoutTokenNVX where
        type VkDynamicCountMType VkIndirectCommandsLayoutTokenNVX = Word32

        {-# NOINLINE vkDynamicCount #-}
        vkDynamicCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount})

        {-# INLINE vkDynamicCountByteOffset #-}
        vkDynamicCountByteOffset ~_
          = #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}

        {-# INLINE readVkDynamicCount #-}
        readVkDynamicCount p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}

        {-# INLINE writeVkDynamicCount #-}
        writeVkDynamicCount p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}

instance {-# OVERLAPPING #-}
         HasVkDivisor VkIndirectCommandsLayoutTokenNVX where
        type VkDivisorMType VkIndirectCommandsLayoutTokenNVX = Word32

        {-# NOINLINE vkDivisor #-}
        vkDivisor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, divisor})

        {-# INLINE vkDivisorByteOffset #-}
        vkDivisorByteOffset ~_
          = #{offset VkIndirectCommandsLayoutTokenNVX, divisor}

        {-# INLINE readVkDivisor #-}
        readVkDivisor p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, divisor}

        {-# INLINE writeVkDivisor #-}
        writeVkDivisor p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, divisor}

instance Show VkIndirectCommandsLayoutTokenNVX where
        showsPrec d x
          = showString "VkIndirectCommandsLayoutTokenNVX {" .
              showString "vkTokenType = " .
                showsPrec d (vkTokenType x) .
                  showString ", " .
                    showString "vkBindingUnit = " .
                      showsPrec d (vkBindingUnit x) .
                        showString ", " .
                          showString "vkDynamicCount = " .
                            showsPrec d (vkDynamicCount x) .
                              showString ", " .
                                showString "vkDivisor = " .
                                  showsPrec d (vkDivisor x) . showChar '}'

-- | > typedef struct VkIndirectCommandsLayoutCreateInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkPipelineBindPoint                      pipelineBindPoint;
--   >     VkIndirectCommandsLayoutUsageFlagsNVX    flags;
--   >     uint32_t                                 tokenCount;
--   >     const VkIndirectCommandsLayoutTokenNVX*  pTokens;
--   > } VkIndirectCommandsLayoutCreateInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkIndirectCommandsLayoutCreateInfoNVX.html VkIndirectCommandsLayoutCreateInfoNVX registry at www.khronos.org>
data VkIndirectCommandsLayoutCreateInfoNVX = VkIndirectCommandsLayoutCreateInfoNVX## ByteArray##

instance Eq VkIndirectCommandsLayoutCreateInfoNVX where
        (VkIndirectCommandsLayoutCreateInfoNVX## a) ==
          (VkIndirectCommandsLayoutCreateInfoNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkIndirectCommandsLayoutCreateInfoNVX where
        (VkIndirectCommandsLayoutCreateInfoNVX## a) `compare`
          (VkIndirectCommandsLayoutCreateInfoNVX## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkIndirectCommandsLayoutCreateInfoNVX where
        sizeOf ~_
          = #{size VkIndirectCommandsLayoutCreateInfoNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkIndirectCommandsLayoutCreateInfoNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkIndirectCommandsLayoutCreateInfoNVX),
            I## a <- alignment
                      (undefined :: VkIndirectCommandsLayoutCreateInfoNVX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkIndirectCommandsLayoutCreateInfoNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkIndirectCommandsLayoutCreateInfoNVX## ba)
          | I## n <- sizeOf
                      (undefined :: VkIndirectCommandsLayoutCreateInfoNVX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkIndirectCommandsLayoutCreateInfoNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkIndirectCommandsLayoutCreateInfoNVX),
            I## a <- alignment
                      (undefined :: VkIndirectCommandsLayoutCreateInfoNVX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkIndirectCommandsLayoutCreateInfoNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkIndirectCommandsLayoutCreateInfoNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkIndirectCommandsLayoutCreateInfoNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkIndirectCommandsLayoutCreateInfoNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkIndirectCommandsLayoutCreateInfoNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkIndirectCommandsLayoutCreateInfoNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkIndirectCommandsLayoutCreateInfoNVX where
        type VkSTypeMType VkIndirectCommandsLayoutCreateInfoNVX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkIndirectCommandsLayoutCreateInfoNVX where
        type VkPNextMType VkIndirectCommandsLayoutCreateInfoNVX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         HasVkPipelineBindPoint VkIndirectCommandsLayoutCreateInfoNVX where
        type VkPipelineBindPointMType VkIndirectCommandsLayoutCreateInfoNVX
             = VkPipelineBindPoint

        {-# NOINLINE vkPipelineBindPoint #-}
        vkPipelineBindPoint x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint})

        {-# INLINE vkPipelineBindPointByteOffset #-}
        vkPipelineBindPointByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}

        {-# INLINE readVkPipelineBindPoint #-}
        readVkPipelineBindPoint p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}

        {-# INLINE writeVkPipelineBindPoint #-}
        writeVkPipelineBindPoint p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         HasVkFlags VkIndirectCommandsLayoutCreateInfoNVX where
        type VkFlagsMType VkIndirectCommandsLayoutCreateInfoNVX =
             VkIndirectCommandsLayoutUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}

instance {-# OVERLAPPING #-}
         HasVkTokenCount VkIndirectCommandsLayoutCreateInfoNVX where
        type VkTokenCountMType VkIndirectCommandsLayoutCreateInfoNVX =
             Word32

        {-# NOINLINE vkTokenCount #-}
        vkTokenCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount})

        {-# INLINE vkTokenCountByteOffset #-}
        vkTokenCountByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}

        {-# INLINE readVkTokenCount #-}
        readVkTokenCount p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}

        {-# INLINE writeVkTokenCount #-}
        writeVkTokenCount p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}

instance {-# OVERLAPPING #-}
         HasVkPTokens VkIndirectCommandsLayoutCreateInfoNVX where
        type VkPTokensMType VkIndirectCommandsLayoutCreateInfoNVX =
             Ptr VkIndirectCommandsLayoutTokenNVX

        {-# NOINLINE vkPTokens #-}
        vkPTokens x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens})

        {-# INLINE vkPTokensByteOffset #-}
        vkPTokensByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}

        {-# INLINE readVkPTokens #-}
        readVkPTokens p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}

        {-# INLINE writeVkPTokens #-}
        writeVkPTokens p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}

instance Show VkIndirectCommandsLayoutCreateInfoNVX where
        showsPrec d x
          = showString "VkIndirectCommandsLayoutCreateInfoNVX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPipelineBindPoint = " .
                            showsPrec d (vkPipelineBindPoint x) .
                              showString ", " .
                                showString "vkFlags = " .
                                  showsPrec d (vkFlags x) .
                                    showString ", " .
                                      showString "vkTokenCount = " .
                                        showsPrec d (vkTokenCount x) .
                                          showString ", " .
                                            showString "vkPTokens = " .
                                              showsPrec d (vkPTokens x) . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCmdProcessCommandsInfoNVX.html VkCmdProcessCommandsInfoNVX registry at www.khronos.org>
data VkCmdProcessCommandsInfoNVX = VkCmdProcessCommandsInfoNVX## ByteArray##

instance Eq VkCmdProcessCommandsInfoNVX where
        (VkCmdProcessCommandsInfoNVX## a) ==
          (VkCmdProcessCommandsInfoNVX## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkCmdProcessCommandsInfoNVX where
        (VkCmdProcessCommandsInfoNVX## a) `compare`
          (VkCmdProcessCommandsInfoNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkCmdProcessCommandsInfoNVX where
        sizeOf ~_ = #{size VkCmdProcessCommandsInfoNVX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCmdProcessCommandsInfoNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkCmdProcessCommandsInfoNVX),
            I## a <- alignment (undefined :: VkCmdProcessCommandsInfoNVX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkCmdProcessCommandsInfoNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkCmdProcessCommandsInfoNVX## ba)
          | I## n <- sizeOf (undefined :: VkCmdProcessCommandsInfoNVX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkCmdProcessCommandsInfoNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkCmdProcessCommandsInfoNVX),
            I## a <- alignment (undefined :: VkCmdProcessCommandsInfoNVX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkCmdProcessCommandsInfoNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkCmdProcessCommandsInfoNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkCmdProcessCommandsInfoNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkCmdProcessCommandsInfoNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkCmdProcessCommandsInfoNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkCmdProcessCommandsInfoNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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

-- | > typedef struct VkCmdReserveSpaceForCommandsInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkObjectTableNVX                                         objectTable;
--   >     VkIndirectCommandsLayoutNVX                              indirectCommandsLayout;
--   >     uint32_t                                                 maxSequencesCount;
--   > } VkCmdReserveSpaceForCommandsInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkCmdReserveSpaceForCommandsInfoNVX.html VkCmdReserveSpaceForCommandsInfoNVX registry at www.khronos.org>
data VkCmdReserveSpaceForCommandsInfoNVX = VkCmdReserveSpaceForCommandsInfoNVX## ByteArray##

instance Eq VkCmdReserveSpaceForCommandsInfoNVX where
        (VkCmdReserveSpaceForCommandsInfoNVX## a) ==
          (VkCmdReserveSpaceForCommandsInfoNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkCmdReserveSpaceForCommandsInfoNVX where
        (VkCmdReserveSpaceForCommandsInfoNVX## a) `compare`
          (VkCmdReserveSpaceForCommandsInfoNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkCmdReserveSpaceForCommandsInfoNVX where
        sizeOf ~_ = #{size VkCmdReserveSpaceForCommandsInfoNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkCmdReserveSpaceForCommandsInfoNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkCmdReserveSpaceForCommandsInfoNVX),
            I## a <- alignment
                      (undefined :: VkCmdReserveSpaceForCommandsInfoNVX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkCmdReserveSpaceForCommandsInfoNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkCmdReserveSpaceForCommandsInfoNVX## ba)
          | I## n <- sizeOf (undefined :: VkCmdReserveSpaceForCommandsInfoNVX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkCmdReserveSpaceForCommandsInfoNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkCmdReserveSpaceForCommandsInfoNVX),
            I## a <- alignment
                      (undefined :: VkCmdReserveSpaceForCommandsInfoNVX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkCmdReserveSpaceForCommandsInfoNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkCmdReserveSpaceForCommandsInfoNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkCmdReserveSpaceForCommandsInfoNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkCmdReserveSpaceForCommandsInfoNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkCmdReserveSpaceForCommandsInfoNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkCmdReserveSpaceForCommandsInfoNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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

-- | > typedef struct VkObjectTableCreateInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                                          objectCount;
--   >     const VkObjectEntryTypeNVX*       pObjectEntryTypes;
--   >     const uint32_t*                   pObjectEntryCounts;
--   >     const VkObjectEntryUsageFlagsNVX* pObjectEntryUsageFlags;
--   >     uint32_t maxUniformBuffersPerDescriptor;
--   >     uint32_t maxStorageBuffersPerDescriptor;
--   >     uint32_t maxStorageImagesPerDescriptor;
--   >     uint32_t maxSampledImagesPerDescriptor;
--   >     uint32_t maxPipelineLayouts;
--   > } VkObjectTableCreateInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectTableCreateInfoNVX.html VkObjectTableCreateInfoNVX registry at www.khronos.org>
data VkObjectTableCreateInfoNVX = VkObjectTableCreateInfoNVX## ByteArray##

instance Eq VkObjectTableCreateInfoNVX where
        (VkObjectTableCreateInfoNVX## a) == (VkObjectTableCreateInfoNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableCreateInfoNVX where
        (VkObjectTableCreateInfoNVX## a) `compare`
          (VkObjectTableCreateInfoNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkObjectTableCreateInfoNVX where
        sizeOf ~_ = #{size VkObjectTableCreateInfoNVX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkObjectTableCreateInfoNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkObjectTableCreateInfoNVX),
            I## a <- alignment (undefined :: VkObjectTableCreateInfoNVX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkObjectTableCreateInfoNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkObjectTableCreateInfoNVX## ba)
          | I## n <- sizeOf (undefined :: VkObjectTableCreateInfoNVX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkObjectTableCreateInfoNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkObjectTableCreateInfoNVX),
            I## a <- alignment (undefined :: VkObjectTableCreateInfoNVX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkObjectTableCreateInfoNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkObjectTableCreateInfoNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkObjectTableCreateInfoNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkObjectTableCreateInfoNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkObjectTableCreateInfoNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkObjectTableCreateInfoNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkObjectTableCreateInfoNVX
         where
        type VkSTypeMType VkObjectTableCreateInfoNVX = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkObjectTableCreateInfoNVX
         where
        type VkPNextMType VkObjectTableCreateInfoNVX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         HasVkObjectCount VkObjectTableCreateInfoNVX where
        type VkObjectCountMType VkObjectTableCreateInfoNVX = Word32

        {-# NOINLINE vkObjectCount #-}
        vkObjectCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, objectCount})

        {-# INLINE vkObjectCountByteOffset #-}
        vkObjectCountByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, objectCount}

        {-# INLINE readVkObjectCount #-}
        readVkObjectCount p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, objectCount}

        {-# INLINE writeVkObjectCount #-}
        writeVkObjectCount p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, objectCount}

instance {-# OVERLAPPING #-}
         HasVkPObjectEntryTypes VkObjectTableCreateInfoNVX where
        type VkPObjectEntryTypesMType VkObjectTableCreateInfoNVX =
             Ptr VkObjectEntryTypeNVX

        {-# NOINLINE vkPObjectEntryTypes #-}
        vkPObjectEntryTypes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes})

        {-# INLINE vkPObjectEntryTypesByteOffset #-}
        vkPObjectEntryTypesByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}

        {-# INLINE readVkPObjectEntryTypes #-}
        readVkPObjectEntryTypes p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}

        {-# INLINE writeVkPObjectEntryTypes #-}
        writeVkPObjectEntryTypes p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryTypes}

instance {-# OVERLAPPING #-}
         HasVkPObjectEntryCounts VkObjectTableCreateInfoNVX where
        type VkPObjectEntryCountsMType VkObjectTableCreateInfoNVX =
             Ptr Word32

        {-# NOINLINE vkPObjectEntryCounts #-}
        vkPObjectEntryCounts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts})

        {-# INLINE vkPObjectEntryCountsByteOffset #-}
        vkPObjectEntryCountsByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}

        {-# INLINE readVkPObjectEntryCounts #-}
        readVkPObjectEntryCounts p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}

        {-# INLINE writeVkPObjectEntryCounts #-}
        writeVkPObjectEntryCounts p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryCounts}

instance {-# OVERLAPPING #-}
         HasVkPObjectEntryUsageFlags VkObjectTableCreateInfoNVX where
        type VkPObjectEntryUsageFlagsMType VkObjectTableCreateInfoNVX =
             Ptr VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkPObjectEntryUsageFlags #-}
        vkPObjectEntryUsageFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags})

        {-# INLINE vkPObjectEntryUsageFlagsByteOffset #-}
        vkPObjectEntryUsageFlagsByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}

        {-# INLINE readVkPObjectEntryUsageFlags #-}
        readVkPObjectEntryUsageFlags p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}

        {-# INLINE writeVkPObjectEntryUsageFlags #-}
        writeVkPObjectEntryUsageFlags p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, pObjectEntryUsageFlags}

instance {-# OVERLAPPING #-}
         HasVkMaxUniformBuffersPerDescriptor VkObjectTableCreateInfoNVX
         where
        type VkMaxUniformBuffersPerDescriptorMType
               VkObjectTableCreateInfoNVX
             = Word32

        {-# NOINLINE vkMaxUniformBuffersPerDescriptor #-}
        vkMaxUniformBuffersPerDescriptor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor})

        {-# INLINE vkMaxUniformBuffersPerDescriptorByteOffset #-}
        vkMaxUniformBuffersPerDescriptorByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}

        {-# INLINE readVkMaxUniformBuffersPerDescriptor #-}
        readVkMaxUniformBuffersPerDescriptor p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}

        {-# INLINE writeVkMaxUniformBuffersPerDescriptor #-}
        writeVkMaxUniformBuffersPerDescriptor p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxUniformBuffersPerDescriptor}

instance {-# OVERLAPPING #-}
         HasVkMaxStorageBuffersPerDescriptor VkObjectTableCreateInfoNVX
         where
        type VkMaxStorageBuffersPerDescriptorMType
               VkObjectTableCreateInfoNVX
             = Word32

        {-# NOINLINE vkMaxStorageBuffersPerDescriptor #-}
        vkMaxStorageBuffersPerDescriptor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor})

        {-# INLINE vkMaxStorageBuffersPerDescriptorByteOffset #-}
        vkMaxStorageBuffersPerDescriptorByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}

        {-# INLINE readVkMaxStorageBuffersPerDescriptor #-}
        readVkMaxStorageBuffersPerDescriptor p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}

        {-# INLINE writeVkMaxStorageBuffersPerDescriptor #-}
        writeVkMaxStorageBuffersPerDescriptor p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageBuffersPerDescriptor}

instance {-# OVERLAPPING #-}
         HasVkMaxStorageImagesPerDescriptor VkObjectTableCreateInfoNVX where
        type VkMaxStorageImagesPerDescriptorMType
               VkObjectTableCreateInfoNVX
             = Word32

        {-# NOINLINE vkMaxStorageImagesPerDescriptor #-}
        vkMaxStorageImagesPerDescriptor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor})

        {-# INLINE vkMaxStorageImagesPerDescriptorByteOffset #-}
        vkMaxStorageImagesPerDescriptorByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}

        {-# INLINE readVkMaxStorageImagesPerDescriptor #-}
        readVkMaxStorageImagesPerDescriptor p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}

        {-# INLINE writeVkMaxStorageImagesPerDescriptor #-}
        writeVkMaxStorageImagesPerDescriptor p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxStorageImagesPerDescriptor}

instance {-# OVERLAPPING #-}
         HasVkMaxSampledImagesPerDescriptor VkObjectTableCreateInfoNVX where
        type VkMaxSampledImagesPerDescriptorMType
               VkObjectTableCreateInfoNVX
             = Word32

        {-# NOINLINE vkMaxSampledImagesPerDescriptor #-}
        vkMaxSampledImagesPerDescriptor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor})

        {-# INLINE vkMaxSampledImagesPerDescriptorByteOffset #-}
        vkMaxSampledImagesPerDescriptorByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}

        {-# INLINE readVkMaxSampledImagesPerDescriptor #-}
        readVkMaxSampledImagesPerDescriptor p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}

        {-# INLINE writeVkMaxSampledImagesPerDescriptor #-}
        writeVkMaxSampledImagesPerDescriptor p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxSampledImagesPerDescriptor}

instance {-# OVERLAPPING #-}
         HasVkMaxPipelineLayouts VkObjectTableCreateInfoNVX where
        type VkMaxPipelineLayoutsMType VkObjectTableCreateInfoNVX = Word32

        {-# NOINLINE vkMaxPipelineLayouts #-}
        vkMaxPipelineLayouts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts})

        {-# INLINE vkMaxPipelineLayoutsByteOffset #-}
        vkMaxPipelineLayoutsByteOffset ~_
          = #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}

        {-# INLINE readVkMaxPipelineLayouts #-}
        readVkMaxPipelineLayouts p
          = peekByteOff p #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}

        {-# INLINE writeVkMaxPipelineLayouts #-}
        writeVkMaxPipelineLayouts p
          = pokeByteOff p #{offset VkObjectTableCreateInfoNVX, maxPipelineLayouts}

instance Show VkObjectTableCreateInfoNVX where
        showsPrec d x
          = showString "VkObjectTableCreateInfoNVX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkObjectCount = " .
                            showsPrec d (vkObjectCount x) .
                              showString ", " .
                                showString "vkPObjectEntryTypes = " .
                                  showsPrec d (vkPObjectEntryTypes x) .
                                    showString ", " .
                                      showString "vkPObjectEntryCounts = " .
                                        showsPrec d (vkPObjectEntryCounts x) .
                                          showString ", " .
                                            showString "vkPObjectEntryUsageFlags = " .
                                              showsPrec d (vkPObjectEntryUsageFlags x) .
                                                showString ", " .
                                                  showString "vkMaxUniformBuffersPerDescriptor = " .
                                                    showsPrec d (vkMaxUniformBuffersPerDescriptor x)
                                                      .
                                                      showString ", " .
                                                        showString
                                                          "vkMaxStorageBuffersPerDescriptor = "
                                                          .
                                                          showsPrec d
                                                            (vkMaxStorageBuffersPerDescriptor x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "vkMaxStorageImagesPerDescriptor = "
                                                                .
                                                                showsPrec d
                                                                  (vkMaxStorageImagesPerDescriptor
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkMaxSampledImagesPerDescriptor = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkMaxSampledImagesPerDescriptor
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkMaxPipelineLayouts = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkMaxPipelineLayouts
                                                                                 x)
                                                                              . showChar '}'

-- | > typedef struct VkObjectTableEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   > } VkObjectTableEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectTableEntryNVX.html VkObjectTableEntryNVX registry at www.khronos.org>
data VkObjectTableEntryNVX = VkObjectTableEntryNVX## ByteArray##

instance Eq VkObjectTableEntryNVX where
        (VkObjectTableEntryNVX## a) == (VkObjectTableEntryNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableEntryNVX where
        (VkObjectTableEntryNVX## a) `compare` (VkObjectTableEntryNVX## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkObjectTableEntryNVX where
        sizeOf ~_ = #{size VkObjectTableEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkObjectTableEntryNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkObjectTableEntryNVX),
            I## a <- alignment (undefined :: VkObjectTableEntryNVX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkObjectTableEntryNVX## ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkObjectTableEntryNVX## ba)
          | I## n <- sizeOf (undefined :: VkObjectTableEntryNVX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkObjectTableEntryNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkObjectTableEntryNVX),
            I## a <- alignment (undefined :: VkObjectTableEntryNVX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkObjectTableEntryNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkObjectTableEntryNVX## ba) = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkObjectTableEntryNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkObjectTableEntryNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkObjectTableEntryNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkObjectTableEntryNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkType VkObjectTableEntryNVX where
        type VkTypeMType VkObjectTableEntryNVX = VkObjectEntryTypeNVX

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableEntryNVX, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkObjectTableEntryNVX, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkObjectTableEntryNVX, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkObjectTableEntryNVX, type}

instance {-# OVERLAPPING #-} HasVkFlags VkObjectTableEntryNVX where
        type VkFlagsMType VkObjectTableEntryNVX =
             VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableEntryNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkObjectTableEntryNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkObjectTableEntryNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkObjectTableEntryNVX, flags}

instance Show VkObjectTableEntryNVX where
        showsPrec d x
          = showString "VkObjectTableEntryNVX {" .
              showString "vkType = " .
                showsPrec d (vkType x) .
                  showString ", " .
                    showString "vkFlags = " . showsPrec d (vkFlags x) . showChar '}'

-- | > typedef struct VkObjectTablePipelineEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipeline                   pipeline;
--   > } VkObjectTablePipelineEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectTablePipelineEntryNVX.html VkObjectTablePipelineEntryNVX registry at www.khronos.org>
data VkObjectTablePipelineEntryNVX = VkObjectTablePipelineEntryNVX## ByteArray##

instance Eq VkObjectTablePipelineEntryNVX where
        (VkObjectTablePipelineEntryNVX## a) ==
          (VkObjectTablePipelineEntryNVX## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkObjectTablePipelineEntryNVX where
        (VkObjectTablePipelineEntryNVX## a) `compare`
          (VkObjectTablePipelineEntryNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkObjectTablePipelineEntryNVX where
        sizeOf ~_ = #{size VkObjectTablePipelineEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTablePipelineEntryNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkObjectTablePipelineEntryNVX),
            I## a <- alignment (undefined :: VkObjectTablePipelineEntryNVX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkObjectTablePipelineEntryNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkObjectTablePipelineEntryNVX## ba)
          | I## n <- sizeOf (undefined :: VkObjectTablePipelineEntryNVX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkObjectTablePipelineEntryNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkObjectTablePipelineEntryNVX),
            I## a <- alignment (undefined :: VkObjectTablePipelineEntryNVX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkObjectTablePipelineEntryNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkObjectTablePipelineEntryNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkObjectTablePipelineEntryNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkObjectTablePipelineEntryNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkObjectTablePipelineEntryNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkObjectTablePipelineEntryNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkType VkObjectTablePipelineEntryNVX where
        type VkTypeMType VkObjectTablePipelineEntryNVX =
             VkObjectEntryTypeNVX

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePipelineEntryNVX, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkObjectTablePipelineEntryNVX, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkObjectTablePipelineEntryNVX, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkObjectTablePipelineEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasVkFlags VkObjectTablePipelineEntryNVX where
        type VkFlagsMType VkObjectTablePipelineEntryNVX =
             VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePipelineEntryNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkObjectTablePipelineEntryNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkObjectTablePipelineEntryNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkObjectTablePipelineEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasVkPipeline VkObjectTablePipelineEntryNVX where
        type VkPipelineMType VkObjectTablePipelineEntryNVX = VkPipeline

        {-# NOINLINE vkPipeline #-}
        vkPipeline x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePipelineEntryNVX, pipeline})

        {-# INLINE vkPipelineByteOffset #-}
        vkPipelineByteOffset ~_
          = #{offset VkObjectTablePipelineEntryNVX, pipeline}

        {-# INLINE readVkPipeline #-}
        readVkPipeline p
          = peekByteOff p #{offset VkObjectTablePipelineEntryNVX, pipeline}

        {-# INLINE writeVkPipeline #-}
        writeVkPipeline p
          = pokeByteOff p #{offset VkObjectTablePipelineEntryNVX, pipeline}

instance Show VkObjectTablePipelineEntryNVX where
        showsPrec d x
          = showString "VkObjectTablePipelineEntryNVX {" .
              showString "vkType = " .
                showsPrec d (vkType x) .
                  showString ", " .
                    showString "vkFlags = " .
                      showsPrec d (vkFlags x) .
                        showString ", " .
                          showString "vkPipeline = " .
                            showsPrec d (vkPipeline x) . showChar '}'

-- | > typedef struct VkObjectTableDescriptorSetEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipelineLayout             pipelineLayout;
--   >     VkDescriptorSet              descriptorSet;
--   > } VkObjectTableDescriptorSetEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectTableDescriptorSetEntryNVX.html VkObjectTableDescriptorSetEntryNVX registry at www.khronos.org>
data VkObjectTableDescriptorSetEntryNVX = VkObjectTableDescriptorSetEntryNVX## ByteArray##

instance Eq VkObjectTableDescriptorSetEntryNVX where
        (VkObjectTableDescriptorSetEntryNVX## a) ==
          (VkObjectTableDescriptorSetEntryNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableDescriptorSetEntryNVX where
        (VkObjectTableDescriptorSetEntryNVX## a) `compare`
          (VkObjectTableDescriptorSetEntryNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkObjectTableDescriptorSetEntryNVX where
        sizeOf ~_ = #{size VkObjectTableDescriptorSetEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTableDescriptorSetEntryNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkObjectTableDescriptorSetEntryNVX),
            I## a <- alignment (undefined :: VkObjectTableDescriptorSetEntryNVX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkObjectTableDescriptorSetEntryNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkObjectTableDescriptorSetEntryNVX## ba)
          | I## n <- sizeOf (undefined :: VkObjectTableDescriptorSetEntryNVX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkObjectTableDescriptorSetEntryNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkObjectTableDescriptorSetEntryNVX),
            I## a <- alignment (undefined :: VkObjectTableDescriptorSetEntryNVX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkObjectTableDescriptorSetEntryNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkObjectTableDescriptorSetEntryNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkObjectTableDescriptorSetEntryNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkObjectTableDescriptorSetEntryNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkObjectTableDescriptorSetEntryNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkObjectTableDescriptorSetEntryNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkType VkObjectTableDescriptorSetEntryNVX where
        type VkTypeMType VkObjectTableDescriptorSetEntryNVX =
             VkObjectEntryTypeNVX

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkObjectTableDescriptorSetEntryNVX, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasVkFlags VkObjectTableDescriptorSetEntryNVX where
        type VkFlagsMType VkObjectTableDescriptorSetEntryNVX =
             VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkObjectTableDescriptorSetEntryNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasVkPipelineLayout VkObjectTableDescriptorSetEntryNVX where
        type VkPipelineLayoutMType VkObjectTableDescriptorSetEntryNVX =
             VkPipelineLayout

        {-# NOINLINE vkPipelineLayout #-}
        vkPipelineLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout})

        {-# INLINE vkPipelineLayoutByteOffset #-}
        vkPipelineLayoutByteOffset ~_
          = #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

        {-# INLINE readVkPipelineLayout #-}
        readVkPipelineLayout p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

        {-# INLINE writeVkPipelineLayout #-}
        writeVkPipelineLayout p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         HasVkDescriptorSet VkObjectTableDescriptorSetEntryNVX where
        type VkDescriptorSetMType VkObjectTableDescriptorSetEntryNVX =
             VkDescriptorSet

        {-# NOINLINE vkDescriptorSet #-}
        vkDescriptorSet x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet})

        {-# INLINE vkDescriptorSetByteOffset #-}
        vkDescriptorSetByteOffset ~_
          = #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

        {-# INLINE readVkDescriptorSet #-}
        readVkDescriptorSet p
          = peekByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

        {-# INLINE writeVkDescriptorSet #-}
        writeVkDescriptorSet p
          = pokeByteOff p #{offset VkObjectTableDescriptorSetEntryNVX, descriptorSet}

instance Show VkObjectTableDescriptorSetEntryNVX where
        showsPrec d x
          = showString "VkObjectTableDescriptorSetEntryNVX {" .
              showString "vkType = " .
                showsPrec d (vkType x) .
                  showString ", " .
                    showString "vkFlags = " .
                      showsPrec d (vkFlags x) .
                        showString ", " .
                          showString "vkPipelineLayout = " .
                            showsPrec d (vkPipelineLayout x) .
                              showString ", " .
                                showString "vkDescriptorSet = " .
                                  showsPrec d (vkDescriptorSet x) . showChar '}'

-- | > typedef struct VkObjectTableVertexBufferEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkBuffer                     buffer;
--   > } VkObjectTableVertexBufferEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectTableVertexBufferEntryNVX.html VkObjectTableVertexBufferEntryNVX registry at www.khronos.org>
data VkObjectTableVertexBufferEntryNVX = VkObjectTableVertexBufferEntryNVX## ByteArray##

instance Eq VkObjectTableVertexBufferEntryNVX where
        (VkObjectTableVertexBufferEntryNVX## a) ==
          (VkObjectTableVertexBufferEntryNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableVertexBufferEntryNVX where
        (VkObjectTableVertexBufferEntryNVX## a) `compare`
          (VkObjectTableVertexBufferEntryNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkObjectTableVertexBufferEntryNVX where
        sizeOf ~_ = #{size VkObjectTableVertexBufferEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTableVertexBufferEntryNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkObjectTableVertexBufferEntryNVX),
            I## a <- alignment (undefined :: VkObjectTableVertexBufferEntryNVX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkObjectTableVertexBufferEntryNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkObjectTableVertexBufferEntryNVX## ba)
          | I## n <- sizeOf (undefined :: VkObjectTableVertexBufferEntryNVX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkObjectTableVertexBufferEntryNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkObjectTableVertexBufferEntryNVX),
            I## a <- alignment (undefined :: VkObjectTableVertexBufferEntryNVX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkObjectTableVertexBufferEntryNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkObjectTableVertexBufferEntryNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkObjectTableVertexBufferEntryNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkObjectTableVertexBufferEntryNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkObjectTableVertexBufferEntryNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkObjectTableVertexBufferEntryNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkType VkObjectTableVertexBufferEntryNVX where
        type VkTypeMType VkObjectTableVertexBufferEntryNVX =
             VkObjectEntryTypeNVX

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableVertexBufferEntryNVX, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkObjectTableVertexBufferEntryNVX, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkObjectTableVertexBufferEntryNVX, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkObjectTableVertexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasVkFlags VkObjectTableVertexBufferEntryNVX where
        type VkFlagsMType VkObjectTableVertexBufferEntryNVX =
             VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableVertexBufferEntryNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkObjectTableVertexBufferEntryNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkObjectTableVertexBufferEntryNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkObjectTableVertexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasVkBuffer VkObjectTableVertexBufferEntryNVX where
        type VkBufferMType VkObjectTableVertexBufferEntryNVX = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableVertexBufferEntryNVX, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkObjectTableVertexBufferEntryNVX, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkObjectTableVertexBufferEntryNVX, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkObjectTableVertexBufferEntryNVX, buffer}

instance Show VkObjectTableVertexBufferEntryNVX where
        showsPrec d x
          = showString "VkObjectTableVertexBufferEntryNVX {" .
              showString "vkType = " .
                showsPrec d (vkType x) .
                  showString ", " .
                    showString "vkFlags = " .
                      showsPrec d (vkFlags x) .
                        showString ", " .
                          showString "vkBuffer = " . showsPrec d (vkBuffer x) . showChar '}'

-- | > typedef struct VkObjectTableIndexBufferEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkBuffer                     buffer;
--   >     VkIndexType                  indexType;
--   > } VkObjectTableIndexBufferEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectTableIndexBufferEntryNVX.html VkObjectTableIndexBufferEntryNVX registry at www.khronos.org>
data VkObjectTableIndexBufferEntryNVX = VkObjectTableIndexBufferEntryNVX## ByteArray##

instance Eq VkObjectTableIndexBufferEntryNVX where
        (VkObjectTableIndexBufferEntryNVX## a) ==
          (VkObjectTableIndexBufferEntryNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableIndexBufferEntryNVX where
        (VkObjectTableIndexBufferEntryNVX## a) `compare`
          (VkObjectTableIndexBufferEntryNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkObjectTableIndexBufferEntryNVX where
        sizeOf ~_ = #{size VkObjectTableIndexBufferEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTableIndexBufferEntryNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkObjectTableIndexBufferEntryNVX),
            I## a <- alignment (undefined :: VkObjectTableIndexBufferEntryNVX) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkObjectTableIndexBufferEntryNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkObjectTableIndexBufferEntryNVX## ba)
          | I## n <- sizeOf (undefined :: VkObjectTableIndexBufferEntryNVX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkObjectTableIndexBufferEntryNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkObjectTableIndexBufferEntryNVX),
            I## a <- alignment (undefined :: VkObjectTableIndexBufferEntryNVX) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkObjectTableIndexBufferEntryNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkObjectTableIndexBufferEntryNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkObjectTableIndexBufferEntryNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkObjectTableIndexBufferEntryNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkObjectTableIndexBufferEntryNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkObjectTableIndexBufferEntryNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkType VkObjectTableIndexBufferEntryNVX where
        type VkTypeMType VkObjectTableIndexBufferEntryNVX =
             VkObjectEntryTypeNVX

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkObjectTableIndexBufferEntryNVX, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasVkFlags VkObjectTableIndexBufferEntryNVX where
        type VkFlagsMType VkObjectTableIndexBufferEntryNVX =
             VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkObjectTableIndexBufferEntryNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasVkBuffer VkObjectTableIndexBufferEntryNVX where
        type VkBufferMType VkObjectTableIndexBufferEntryNVX = VkBuffer

        {-# NOINLINE vkBuffer #-}
        vkBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, buffer})

        {-# INLINE vkBufferByteOffset #-}
        vkBufferByteOffset ~_
          = #{offset VkObjectTableIndexBufferEntryNVX, buffer}

        {-# INLINE readVkBuffer #-}
        readVkBuffer p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, buffer}

        {-# INLINE writeVkBuffer #-}
        writeVkBuffer p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, buffer}

instance {-# OVERLAPPING #-}
         HasVkIndexType VkObjectTableIndexBufferEntryNVX where
        type VkIndexTypeMType VkObjectTableIndexBufferEntryNVX =
             VkIndexType

        {-# NOINLINE vkIndexType #-}
        vkIndexType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableIndexBufferEntryNVX, indexType})

        {-# INLINE vkIndexTypeByteOffset #-}
        vkIndexTypeByteOffset ~_
          = #{offset VkObjectTableIndexBufferEntryNVX, indexType}

        {-# INLINE readVkIndexType #-}
        readVkIndexType p
          = peekByteOff p #{offset VkObjectTableIndexBufferEntryNVX, indexType}

        {-# INLINE writeVkIndexType #-}
        writeVkIndexType p
          = pokeByteOff p #{offset VkObjectTableIndexBufferEntryNVX, indexType}

instance Show VkObjectTableIndexBufferEntryNVX where
        showsPrec d x
          = showString "VkObjectTableIndexBufferEntryNVX {" .
              showString "vkType = " .
                showsPrec d (vkType x) .
                  showString ", " .
                    showString "vkFlags = " .
                      showsPrec d (vkFlags x) .
                        showString ", " .
                          showString "vkBuffer = " .
                            showsPrec d (vkBuffer x) .
                              showString ", " .
                                showString "vkIndexType = " .
                                  showsPrec d (vkIndexType x) . showChar '}'

-- | > typedef struct VkObjectTablePushConstantEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipelineLayout             pipelineLayout;
--   >     VkShaderStageFlags           stageFlags;
--   > } VkObjectTablePushConstantEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkObjectTablePushConstantEntryNVX.html VkObjectTablePushConstantEntryNVX registry at www.khronos.org>
data VkObjectTablePushConstantEntryNVX = VkObjectTablePushConstantEntryNVX## ByteArray##

instance Eq VkObjectTablePushConstantEntryNVX where
        (VkObjectTablePushConstantEntryNVX## a) ==
          (VkObjectTablePushConstantEntryNVX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkObjectTablePushConstantEntryNVX where
        (VkObjectTablePushConstantEntryNVX## a) `compare`
          (VkObjectTablePushConstantEntryNVX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkObjectTablePushConstantEntryNVX where
        sizeOf ~_ = #{size VkObjectTablePushConstantEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTablePushConstantEntryNVX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkObjectTablePushConstantEntryNVX),
            I## a <- alignment (undefined :: VkObjectTablePushConstantEntryNVX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkObjectTablePushConstantEntryNVX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkObjectTablePushConstantEntryNVX## ba)
          | I## n <- sizeOf (undefined :: VkObjectTablePushConstantEntryNVX) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkObjectTablePushConstantEntryNVX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkObjectTablePushConstantEntryNVX),
            I## a <- alignment (undefined :: VkObjectTablePushConstantEntryNVX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkObjectTablePushConstantEntryNVX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkObjectTablePushConstantEntryNVX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkObjectTablePushConstantEntryNVX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkObjectTablePushConstantEntryNVX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkObjectTablePushConstantEntryNVX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkObjectTablePushConstantEntryNVX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkType VkObjectTablePushConstantEntryNVX where
        type VkTypeMType VkObjectTablePushConstantEntryNVX =
             VkObjectEntryTypeNVX

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkObjectTablePushConstantEntryNVX, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasVkFlags VkObjectTablePushConstantEntryNVX where
        type VkFlagsMType VkObjectTablePushConstantEntryNVX =
             VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkObjectTablePushConstantEntryNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasVkPipelineLayout VkObjectTablePushConstantEntryNVX where
        type VkPipelineLayoutMType VkObjectTablePushConstantEntryNVX =
             VkPipelineLayout

        {-# NOINLINE vkPipelineLayout #-}
        vkPipelineLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout})

        {-# INLINE vkPipelineLayoutByteOffset #-}
        vkPipelineLayoutByteOffset ~_
          = #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}

        {-# INLINE readVkPipelineLayout #-}
        readVkPipelineLayout p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}

        {-# INLINE writeVkPipelineLayout #-}
        writeVkPipelineLayout p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         HasVkStageFlags VkObjectTablePushConstantEntryNVX where
        type VkStageFlagsMType VkObjectTablePushConstantEntryNVX =
             VkShaderStageFlags

        {-# NOINLINE vkStageFlags #-}
        vkStageFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, stageFlags})

        {-# INLINE vkStageFlagsByteOffset #-}
        vkStageFlagsByteOffset ~_
          = #{offset VkObjectTablePushConstantEntryNVX, stageFlags}

        {-# INLINE readVkStageFlags #-}
        readVkStageFlags p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, stageFlags}

        {-# INLINE writeVkStageFlags #-}
        writeVkStageFlags p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, stageFlags}

instance Show VkObjectTablePushConstantEntryNVX where
        showsPrec d x
          = showString "VkObjectTablePushConstantEntryNVX {" .
              showString "vkType = " .
                showsPrec d (vkType x) .
                  showString ", " .
                    showString "vkFlags = " .
                      showsPrec d (vkFlags x) .
                        showString ", " .
                          showString "vkPipelineLayout = " .
                            showsPrec d (vkPipelineLayout x) .
                              showString ", " .
                                showString "vkStageFlags = " .
                                  showsPrec d (vkStageFlags x) . showChar '}'

-- | queues: @graphics,compute@
--
--   renderpass: @inside@
--
--   > void vkCmdProcessCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdProcessCommandsInfoNVX* pProcessCommandsInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdProcessCommandsNVX.html vkCmdProcessCommandsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdProcessCommandsNVX"
               vkCmdProcessCommandsNVX ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCmdProcessCommandsInfoNVX -- ^ pProcessCommandsInfo
                                                                  -> IO ()

-- | queues: @graphics,compute@
--
--   renderpass: @inside@
--
--   > void vkCmdReserveSpaceForCommandsNVX
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkCmdReserveSpaceForCommandsInfoNVX* pReserveSpaceInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdReserveSpaceForCommandsNVX.html vkCmdReserveSpaceForCommandsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCmdReserveSpaceForCommandsNVX"
               vkCmdReserveSpaceForCommandsNVX ::
               VkCommandBuffer -- ^ commandBuffer
                               -> Ptr VkCmdReserveSpaceForCommandsInfoNVX -- ^ pReserveSpaceInfo
                                                                          -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , const VkIndirectCommandsLayoutCreateInfoNVX* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkIndirectCommandsLayoutNVX* pIndirectCommandsLayout
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateIndirectCommandsLayoutNVX.html vkCreateIndirectCommandsLayoutNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCreateIndirectCommandsLayoutNVX"
               vkCreateIndirectCommandsLayoutNVX ::
               VkDevice -- ^ device
                        ->
                 Ptr VkIndirectCommandsLayoutCreateInfoNVX -- ^ pCreateInfo
                                                           ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkIndirectCommandsLayoutNVX -- ^ pIndirectCommandsLayout
                                                     -> IO VkResult

-- | > void vkDestroyIndirectCommandsLayoutNVX
--   >     ( VkDevice device
--   >     , VkIndirectCommandsLayoutNVX indirectCommandsLayout
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyIndirectCommandsLayoutNVX.html vkDestroyIndirectCommandsLayoutNVX registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyIndirectCommandsLayoutNVX"
               vkDestroyIndirectCommandsLayoutNVX ::
               VkDevice -- ^ device
                        ->
                 VkIndirectCommandsLayoutNVX -- ^ indirectCommandsLayout
                                             -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                          -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateObjectTableNVX
--   >     ( VkDevice device
--   >     , const VkObjectTableCreateInfoNVX* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkObjectTableNVX* pObjectTable
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateObjectTableNVX.html vkCreateObjectTableNVX registry at www.khronos.org>
foreign import ccall unsafe "vkCreateObjectTableNVX"
               vkCreateObjectTableNVX ::
               VkDevice -- ^ device
                        ->
                 Ptr VkObjectTableCreateInfoNVX -- ^ pCreateInfo
                                                ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkObjectTableNVX -- ^ pObjectTable
                                                                     -> IO VkResult

-- | > void vkDestroyObjectTableNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyObjectTableNVX.html vkDestroyObjectTableNVX registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyObjectTableNVX"
               vkDestroyObjectTableNVX ::
               VkDevice -- ^ device
                        -> VkObjectTableNVX -- ^ objectTable
                                            -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                         -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkRegisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectTableEntryNVX* const*    ppObjectTableEntries
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkRegisterObjectsNVX.html vkRegisterObjectsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkRegisterObjectsNVX"
               vkRegisterObjectsNVX ::
               VkDevice -- ^ device
                        ->
                 VkObjectTableNVX -- ^ objectTable
                                  ->
                   Word32 -- ^ objectCount
                          ->
                     Ptr (Ptr VkObjectTableEntryNVX) -- ^ ppObjectTableEntries
                                                     -> Ptr Word32 -- ^ pObjectIndices
                                                                   -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkUnregisterObjectsNVX
--   >     ( VkDevice device
--   >     , VkObjectTableNVX objectTable
--   >     , uint32_t objectCount
--   >     , const VkObjectEntryTypeNVX* pObjectEntryTypes
--   >     , const uint32_t* pObjectIndices
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkUnregisterObjectsNVX.html vkUnregisterObjectsNVX registry at www.khronos.org>
foreign import ccall unsafe "vkUnregisterObjectsNVX"
               vkUnregisterObjectsNVX ::
               VkDevice -- ^ device
                        ->
                 VkObjectTableNVX -- ^ objectTable
                                  ->
                   Word32 -- ^ objectCount
                          -> Ptr VkObjectEntryTypeNVX -- ^ pObjectEntryTypes
                                                      -> Ptr Word32 -- ^ pObjectIndices
                                                                    -> IO VkResult

-- | > void vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDeviceGeneratedCommandsFeaturesNVX* pFeatures
--   >     , VkDeviceGeneratedCommandsLimitsNVX* pLimits
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX.html vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX"
               vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkDeviceGeneratedCommandsFeaturesNVX -- ^ pFeatures
                                                          ->
                   Ptr VkDeviceGeneratedCommandsLimitsNVX -- ^ pLimits
                                                          -> IO ()

pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

type VK_NVX_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: CString

pattern VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME <-
        (is_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME -> True)
  where VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
          = _VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME

_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: CString

{-# INLINE _VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME #-}
_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  = Ptr "VK_NVX_device_generated_commands\NUL"##

is_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME ::
                                                   CString -> Bool

{-# INLINE is_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME #-}
is_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
  = (_VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME ==)

type VK_NVX_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME =
     "VK_NVX_device_generated_commands"

pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX =
        VkStructureType 1000086000

pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
        = VkStructureType 1000086001

pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX =
        VkStructureType 1000086002

pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX =
        VkStructureType 1000086003

pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX =
        VkStructureType 1000086004

pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX =
        VkStructureType 1000086005

-- | bitpos = @17@
pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX ::
        VkPipelineStageFlagBits

pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX =
        VkPipelineStageFlagBits 131072

-- | bitpos = @17@
pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX :: VkAccessFlagBits

pattern VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX =
        VkAccessFlagBits 131072

-- | bitpos = @18@
pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX :: VkAccessFlagBits

pattern VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX =
        VkAccessFlagBits 262144

-- | VkobjectTableNVX
pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX :: VkObjectType

pattern VK_OBJECT_TYPE_OBJECT_TABLE_NVX = VkObjectType 1000086000

-- | VkIndirectCommandsLayoutNVX
pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX :: VkObjectType

pattern VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX =
        VkObjectType 1000086001
