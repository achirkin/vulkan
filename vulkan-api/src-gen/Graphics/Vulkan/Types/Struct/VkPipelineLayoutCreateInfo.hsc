#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineLayoutCreateInfo
       (VkPipelineLayoutCreateInfo(..)) where
import           Foreign.Storable                                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                   (VkPipelineLayoutCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType       (VkStructureType)
import           Graphics.Vulkan.Types.Handles                    (VkDescriptorSetLayout)
import           Graphics.Vulkan.Types.Struct.VkPushConstantRange (VkPushConstantRange)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                 (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineLayoutCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineLayoutCreateFlags    flags;
--   >     uint32_t               setLayoutCount;
--   >     const VkDescriptorSetLayout* pSetLayouts;
--   >     uint32_t               pushConstantRangeCount;
--   >     const VkPushConstantRange* pPushConstantRanges;
--   > } VkPipelineLayoutCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineLayoutCreateInfo.html VkPipelineLayoutCreateInfo registry at www.khronos.org>
data VkPipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo## Addr##
                                                              ByteArray##

instance Eq VkPipelineLayoutCreateInfo where
        (VkPipelineLayoutCreateInfo## a _) ==
          x@(VkPipelineLayoutCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineLayoutCreateInfo where
        (VkPipelineLayoutCreateInfo## a _) `compare`
          x@(VkPipelineLayoutCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineLayoutCreateInfo where
        sizeOf ~_ = #{size VkPipelineLayoutCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkPipelineLayoutCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineLayoutCreateInfo where
        unsafeAddr (VkPipelineLayoutCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineLayoutCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineLayoutCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineLayoutCreateInfo where
        type StructFields VkPipelineLayoutCreateInfo =
             '["sType", "pNext", "flags", "setLayoutCount", "pSetLayouts", -- ' closing tick for hsc2hs
               "pushConstantRangeCount", "pPushConstantRanges"]
        type CUnionType VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineLayoutCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkPipelineLayoutCreateInfo
         where
        type VkSTypeMType VkPipelineLayoutCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineLayoutCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineLayoutCreateInfo where
        type FieldType "sType" VkPipelineLayoutCreateInfo = VkStructureType
        type FieldOptional "sType" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, sType}

instance CanReadField "sType" VkPipelineLayoutCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkPipelineLayoutCreateInfo
         where
        type VkPNextMType VkPipelineLayoutCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineLayoutCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineLayoutCreateInfo where
        type FieldType "pNext" VkPipelineLayoutCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, pNext}

instance CanReadField "pNext" VkPipelineLayoutCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkPipelineLayoutCreateInfo
         where
        type VkFlagsMType VkPipelineLayoutCreateInfo =
             VkPipelineLayoutCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineLayoutCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineLayoutCreateInfo where
        type FieldType "flags" VkPipelineLayoutCreateInfo =
             VkPipelineLayoutCreateFlags
        type FieldOptional "flags" VkPipelineLayoutCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, flags}

instance CanReadField "flags" VkPipelineLayoutCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkSetLayoutCount VkPipelineLayoutCreateInfo where
        type VkSetLayoutCountMType VkPipelineLayoutCreateInfo = Word32

        {-# NOINLINE vkSetLayoutCount #-}
        vkSetLayoutCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, setLayoutCount})

        {-# INLINE vkSetLayoutCountByteOffset #-}
        vkSetLayoutCountByteOffset ~_
          = #{offset VkPipelineLayoutCreateInfo, setLayoutCount}

        {-# INLINE readVkSetLayoutCount #-}
        readVkSetLayoutCount p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, setLayoutCount}

        {-# INLINE writeVkSetLayoutCount #-}
        writeVkSetLayoutCount p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, setLayoutCount}

instance {-# OVERLAPPING #-}
         HasField "setLayoutCount" VkPipelineLayoutCreateInfo where
        type FieldType "setLayoutCount" VkPipelineLayoutCreateInfo = Word32
        type FieldOptional "setLayoutCount" VkPipelineLayoutCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "setLayoutCount" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, setLayoutCount}
        type FieldIsArray "setLayoutCount" VkPipelineLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, setLayoutCount}

instance CanReadField "setLayoutCount" VkPipelineLayoutCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSetLayoutCount

        {-# INLINE readField #-}
        readField = readVkSetLayoutCount

instance CanWriteField "setLayoutCount" VkPipelineLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSetLayoutCount

instance {-# OVERLAPPING #-}
         HasVkPSetLayouts VkPipelineLayoutCreateInfo where
        type VkPSetLayoutsMType VkPipelineLayoutCreateInfo =
             Ptr VkDescriptorSetLayout

        {-# NOINLINE vkPSetLayouts #-}
        vkPSetLayouts x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pSetLayouts})

        {-# INLINE vkPSetLayoutsByteOffset #-}
        vkPSetLayoutsByteOffset ~_
          = #{offset VkPipelineLayoutCreateInfo, pSetLayouts}

        {-# INLINE readVkPSetLayouts #-}
        readVkPSetLayouts p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pSetLayouts}

        {-# INLINE writeVkPSetLayouts #-}
        writeVkPSetLayouts p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pSetLayouts}

instance {-# OVERLAPPING #-}
         HasField "pSetLayouts" VkPipelineLayoutCreateInfo where
        type FieldType "pSetLayouts" VkPipelineLayoutCreateInfo =
             Ptr VkDescriptorSetLayout
        type FieldOptional "pSetLayouts" VkPipelineLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pSetLayouts" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, pSetLayouts}
        type FieldIsArray "pSetLayouts" VkPipelineLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, pSetLayouts}

instance CanReadField "pSetLayouts" VkPipelineLayoutCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPSetLayouts

        {-# INLINE readField #-}
        readField = readVkPSetLayouts

instance CanWriteField "pSetLayouts" VkPipelineLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPSetLayouts

instance {-# OVERLAPPING #-}
         HasVkPushConstantRangeCount VkPipelineLayoutCreateInfo where
        type VkPushConstantRangeCountMType VkPipelineLayoutCreateInfo =
             Word32

        {-# NOINLINE vkPushConstantRangeCount #-}
        vkPushConstantRangeCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount})

        {-# INLINE vkPushConstantRangeCountByteOffset #-}
        vkPushConstantRangeCountByteOffset ~_
          = #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}

        {-# INLINE readVkPushConstantRangeCount #-}
        readVkPushConstantRangeCount p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}

        {-# INLINE writeVkPushConstantRangeCount #-}
        writeVkPushConstantRangeCount p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}

instance {-# OVERLAPPING #-}
         HasField "pushConstantRangeCount" VkPipelineLayoutCreateInfo where
        type FieldType "pushConstantRangeCount" VkPipelineLayoutCreateInfo
             = Word32
        type FieldOptional "pushConstantRangeCount"
               VkPipelineLayoutCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pushConstantRangeCount"
               VkPipelineLayoutCreateInfo
             =
             #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}
        type FieldIsArray "pushConstantRangeCount"
               VkPipelineLayoutCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}

instance CanReadField "pushConstantRangeCount"
           VkPipelineLayoutCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPushConstantRangeCount

        {-# INLINE readField #-}
        readField = readVkPushConstantRangeCount

instance CanWriteField "pushConstantRangeCount"
           VkPipelineLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPushConstantRangeCount

instance {-# OVERLAPPING #-}
         HasVkPPushConstantRanges VkPipelineLayoutCreateInfo where
        type VkPPushConstantRangesMType VkPipelineLayoutCreateInfo =
             Ptr VkPushConstantRange

        {-# NOINLINE vkPPushConstantRanges #-}
        vkPPushConstantRanges x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges})

        {-# INLINE vkPPushConstantRangesByteOffset #-}
        vkPPushConstantRangesByteOffset ~_
          = #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}

        {-# INLINE readVkPPushConstantRanges #-}
        readVkPPushConstantRanges p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}

        {-# INLINE writeVkPPushConstantRanges #-}
        writeVkPPushConstantRanges p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}

instance {-# OVERLAPPING #-}
         HasField "pPushConstantRanges" VkPipelineLayoutCreateInfo where
        type FieldType "pPushConstantRanges" VkPipelineLayoutCreateInfo =
             Ptr VkPushConstantRange
        type FieldOptional "pPushConstantRanges" VkPipelineLayoutCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPushConstantRanges" VkPipelineLayoutCreateInfo =
             #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}
        type FieldIsArray "pPushConstantRanges" VkPipelineLayoutCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}

instance CanReadField "pPushConstantRanges"
           VkPipelineLayoutCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPPushConstantRanges

        {-# INLINE readField #-}
        readField = readVkPPushConstantRanges

instance CanWriteField "pPushConstantRanges"
           VkPipelineLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPPushConstantRanges

instance Show VkPipelineLayoutCreateInfo where
        showsPrec d x
          = showString "VkPipelineLayoutCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkSetLayoutCount = " .
                                  showsPrec d (vkSetLayoutCount x) .
                                    showString ", " .
                                      showString "vkPSetLayouts = " .
                                        showsPrec d (vkPSetLayouts x) .
                                          showString ", " .
                                            showString "vkPushConstantRangeCount = " .
                                              showsPrec d (vkPushConstantRangeCount x) .
                                                showString ", " .
                                                  showString "vkPPushConstantRanges = " .
                                                    showsPrec d (vkPPushConstantRanges x) .
                                                      showChar '}'
