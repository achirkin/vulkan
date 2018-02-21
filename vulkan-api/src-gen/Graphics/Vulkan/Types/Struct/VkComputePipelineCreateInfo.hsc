#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkComputePipelineCreateInfo
       (VkComputePipelineCreateInfo(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkPipelineCreateFlags             (VkPipelineCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                (VkPipeline,
                                                                               VkPipelineLayout)
import           Graphics.Vulkan.Types.Struct.VkPipelineShaderStageCreateInfo (VkPipelineShaderStageCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkComputePipelineCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineCreateFlags  flags;
--   >     VkPipelineShaderStageCreateInfo stage;
--   >     VkPipelineLayout       layout;
--   >     VkPipeline      basePipelineHandle;
--   >     int32_t                basePipelineIndex;
--   > } VkComputePipelineCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkComputePipelineCreateInfo.html VkComputePipelineCreateInfo registry at www.khronos.org>
data VkComputePipelineCreateInfo = VkComputePipelineCreateInfo## Addr##
                                                                ByteArray##

instance Eq VkComputePipelineCreateInfo where
        (VkComputePipelineCreateInfo## a _) ==
          x@(VkComputePipelineCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkComputePipelineCreateInfo where
        (VkComputePipelineCreateInfo## a _) `compare`
          x@(VkComputePipelineCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkComputePipelineCreateInfo where
        sizeOf ~_ = #{size VkComputePipelineCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkComputePipelineCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkComputePipelineCreateInfo where
        unsafeAddr (VkComputePipelineCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkComputePipelineCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkComputePipelineCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkComputePipelineCreateInfo where
        type StructFields VkComputePipelineCreateInfo =
             '["sType", "pNext", "flags", "stage", "layout", -- ' closing tick for hsc2hs
               "basePipelineHandle", "basePipelineIndex"]
        type CUnionType VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkComputePipelineCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkComputePipelineCreateInfo
         where
        type VkSTypeMType VkComputePipelineCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkComputePipelineCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkComputePipelineCreateInfo where
        type FieldType "sType" VkComputePipelineCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkComputePipelineCreateInfo =
             #{offset VkComputePipelineCreateInfo, sType}
        type FieldIsArray "sType" VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkComputePipelineCreateInfo, sType}

instance CanReadField "sType" VkComputePipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkComputePipelineCreateInfo
         where
        type VkPNextMType VkComputePipelineCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkComputePipelineCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkComputePipelineCreateInfo where
        type FieldType "pNext" VkComputePipelineCreateInfo = Ptr Void
        type FieldOptional "pNext" VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkComputePipelineCreateInfo =
             #{offset VkComputePipelineCreateInfo, pNext}
        type FieldIsArray "pNext" VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkComputePipelineCreateInfo, pNext}

instance CanReadField "pNext" VkComputePipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkComputePipelineCreateInfo
         where
        type VkFlagsMType VkComputePipelineCreateInfo =
             VkPipelineCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkComputePipelineCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkComputePipelineCreateInfo where
        type FieldType "flags" VkComputePipelineCreateInfo =
             VkPipelineCreateFlags
        type FieldOptional "flags" VkComputePipelineCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkComputePipelineCreateInfo =
             #{offset VkComputePipelineCreateInfo, flags}
        type FieldIsArray "flags" VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkComputePipelineCreateInfo, flags}

instance CanReadField "flags" VkComputePipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkStage VkComputePipelineCreateInfo
         where
        type VkStageMType VkComputePipelineCreateInfo =
             VkPipelineShaderStageCreateInfo

        {-# NOINLINE vkStage #-}
        vkStage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, stage})

        {-# INLINE vkStageByteOffset #-}
        vkStageByteOffset ~_
          = #{offset VkComputePipelineCreateInfo, stage}

        {-# INLINE readVkStage #-}
        readVkStage p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, stage}

        {-# INLINE writeVkStage #-}
        writeVkStage p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, stage}

instance {-# OVERLAPPING #-}
         HasField "stage" VkComputePipelineCreateInfo where
        type FieldType "stage" VkComputePipelineCreateInfo =
             VkPipelineShaderStageCreateInfo
        type FieldOptional "stage" VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stage" VkComputePipelineCreateInfo =
             #{offset VkComputePipelineCreateInfo, stage}
        type FieldIsArray "stage" VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkComputePipelineCreateInfo, stage}

instance CanReadField "stage" VkComputePipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkStage

        {-# INLINE readField #-}
        readField = readVkStage

instance CanWriteField "stage" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkStage

instance {-# OVERLAPPING #-}
         HasVkLayout VkComputePipelineCreateInfo where
        type VkLayoutMType VkComputePipelineCreateInfo = VkPipelineLayout

        {-# NOINLINE vkLayout #-}
        vkLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, layout})

        {-# INLINE vkLayoutByteOffset #-}
        vkLayoutByteOffset ~_
          = #{offset VkComputePipelineCreateInfo, layout}

        {-# INLINE readVkLayout #-}
        readVkLayout p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, layout}

        {-# INLINE writeVkLayout #-}
        writeVkLayout p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, layout}

instance {-# OVERLAPPING #-}
         HasField "layout" VkComputePipelineCreateInfo where
        type FieldType "layout" VkComputePipelineCreateInfo =
             VkPipelineLayout
        type FieldOptional "layout" VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layout" VkComputePipelineCreateInfo =
             #{offset VkComputePipelineCreateInfo, layout}
        type FieldIsArray "layout" VkComputePipelineCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkComputePipelineCreateInfo, layout}

instance CanReadField "layout" VkComputePipelineCreateInfo where
        {-# INLINE getField #-}
        getField = vkLayout

        {-# INLINE readField #-}
        readField = readVkLayout

instance CanWriteField "layout" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkLayout

instance {-# OVERLAPPING #-}
         HasVkBasePipelineHandle VkComputePipelineCreateInfo where
        type VkBasePipelineHandleMType VkComputePipelineCreateInfo =
             VkPipeline

        {-# NOINLINE vkBasePipelineHandle #-}
        vkBasePipelineHandle x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, basePipelineHandle})

        {-# INLINE vkBasePipelineHandleByteOffset #-}
        vkBasePipelineHandleByteOffset ~_
          = #{offset VkComputePipelineCreateInfo, basePipelineHandle}

        {-# INLINE readVkBasePipelineHandle #-}
        readVkBasePipelineHandle p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, basePipelineHandle}

        {-# INLINE writeVkBasePipelineHandle #-}
        writeVkBasePipelineHandle p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, basePipelineHandle}

instance {-# OVERLAPPING #-}
         HasField "basePipelineHandle" VkComputePipelineCreateInfo where
        type FieldType "basePipelineHandle" VkComputePipelineCreateInfo =
             VkPipeline
        type FieldOptional "basePipelineHandle" VkComputePipelineCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "basePipelineHandle" VkComputePipelineCreateInfo =
             #{offset VkComputePipelineCreateInfo, basePipelineHandle}
        type FieldIsArray "basePipelineHandle" VkComputePipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkComputePipelineCreateInfo, basePipelineHandle}

instance CanReadField "basePipelineHandle"
           VkComputePipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkBasePipelineHandle

        {-# INLINE readField #-}
        readField = readVkBasePipelineHandle

instance CanWriteField "basePipelineHandle"
           VkComputePipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkBasePipelineHandle

instance {-# OVERLAPPING #-}
         HasVkBasePipelineIndex VkComputePipelineCreateInfo where
        type VkBasePipelineIndexMType VkComputePipelineCreateInfo = Int32

        {-# NOINLINE vkBasePipelineIndex #-}
        vkBasePipelineIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, basePipelineIndex})

        {-# INLINE vkBasePipelineIndexByteOffset #-}
        vkBasePipelineIndexByteOffset ~_
          = #{offset VkComputePipelineCreateInfo, basePipelineIndex}

        {-# INLINE readVkBasePipelineIndex #-}
        readVkBasePipelineIndex p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, basePipelineIndex}

        {-# INLINE writeVkBasePipelineIndex #-}
        writeVkBasePipelineIndex p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, basePipelineIndex}

instance {-# OVERLAPPING #-}
         HasField "basePipelineIndex" VkComputePipelineCreateInfo where
        type FieldType "basePipelineIndex" VkComputePipelineCreateInfo =
             Int32
        type FieldOptional "basePipelineIndex" VkComputePipelineCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "basePipelineIndex" VkComputePipelineCreateInfo =
             #{offset VkComputePipelineCreateInfo, basePipelineIndex}
        type FieldIsArray "basePipelineIndex" VkComputePipelineCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkComputePipelineCreateInfo, basePipelineIndex}

instance CanReadField "basePipelineIndex"
           VkComputePipelineCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkBasePipelineIndex

        {-# INLINE readField #-}
        readField = readVkBasePipelineIndex

instance CanWriteField "basePipelineIndex"
           VkComputePipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkBasePipelineIndex

instance Show VkComputePipelineCreateInfo where
        showsPrec d x
          = showString "VkComputePipelineCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkStage = " .
                                  showsPrec d (vkStage x) .
                                    showString ", " .
                                      showString "vkLayout = " .
                                        showsPrec d (vkLayout x) .
                                          showString ", " .
                                            showString "vkBasePipelineHandle = " .
                                              showsPrec d (vkBasePipelineHandle x) .
                                                showString ", " .
                                                  showString "vkBasePipelineIndex = " .
                                                    showsPrec d (vkBasePipelineIndex x) .
                                                      showChar '}'
