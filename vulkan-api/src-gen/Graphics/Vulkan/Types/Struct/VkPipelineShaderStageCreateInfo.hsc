#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineShaderStageCreateInfo
       (VkPipelineShaderStageCreateInfo(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                    (VkPipelineShaderStageCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags     (VkShaderStageFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Handles                     (VkShaderModule)
import           Graphics.Vulkan.Types.Struct.VkSpecializationInfo (VkSpecializationInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineShaderStageCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineShaderStageCreateFlags    flags;
--   >     VkShaderStageFlagBits  stage;
--   >     VkShaderModule         module;
--   >     const char*            pName;
--   >     const VkSpecializationInfo* pSpecializationInfo;
--   > } VkPipelineShaderStageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineShaderStageCreateInfo.html VkPipelineShaderStageCreateInfo registry at www.khronos.org>
data VkPipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo## Addr##
                                                                        ByteArray##

instance Eq VkPipelineShaderStageCreateInfo where
        (VkPipelineShaderStageCreateInfo## a _) ==
          x@(VkPipelineShaderStageCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineShaderStageCreateInfo where
        (VkPipelineShaderStageCreateInfo## a _) `compare`
          x@(VkPipelineShaderStageCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineShaderStageCreateInfo where
        sizeOf ~_ = #{size VkPipelineShaderStageCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineShaderStageCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineShaderStageCreateInfo where
        unsafeAddr (VkPipelineShaderStageCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineShaderStageCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineShaderStageCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineShaderStageCreateInfo where
        type StructFields VkPipelineShaderStageCreateInfo =
             '["sType", "pNext", "flags", "stage", "module", "pName", -- ' closing tick for hsc2hs
               "pSpecializationInfo"]
        type CUnionType VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineShaderStageCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineShaderStageCreateInfo where
        type VkSTypeMType VkPipelineShaderStageCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineShaderStageCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineShaderStageCreateInfo where
        type FieldType "sType" VkPipelineShaderStageCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, sType}

instance CanReadField "sType" VkPipelineShaderStageCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPipelineShaderStageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineShaderStageCreateInfo where
        type VkPNextMType VkPipelineShaderStageCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineShaderStageCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineShaderStageCreateInfo where
        type FieldType "pNext" VkPipelineShaderStageCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, pNext}

instance CanReadField "pNext" VkPipelineShaderStageCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPipelineShaderStageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineShaderStageCreateInfo where
        type VkFlagsMType VkPipelineShaderStageCreateInfo =
             VkPipelineShaderStageCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineShaderStageCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineShaderStageCreateInfo where
        type FieldType "flags" VkPipelineShaderStageCreateInfo =
             VkPipelineShaderStageCreateFlags
        type FieldOptional "flags" VkPipelineShaderStageCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, flags}

instance CanReadField "flags" VkPipelineShaderStageCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkPipelineShaderStageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkStage VkPipelineShaderStageCreateInfo where
        type VkStageMType VkPipelineShaderStageCreateInfo =
             VkShaderStageFlagBits

        {-# NOINLINE vkStage #-}
        vkStage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, stage})

        {-# INLINE vkStageByteOffset #-}
        vkStageByteOffset ~_
          = #{offset VkPipelineShaderStageCreateInfo, stage}

        {-# INLINE readVkStage #-}
        readVkStage p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, stage}

        {-# INLINE writeVkStage #-}
        writeVkStage p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, stage}

instance {-# OVERLAPPING #-}
         HasField "stage" VkPipelineShaderStageCreateInfo where
        type FieldType "stage" VkPipelineShaderStageCreateInfo =
             VkShaderStageFlagBits
        type FieldOptional "stage" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stage" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, stage}
        type FieldIsArray "stage" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, stage}

instance CanReadField "stage" VkPipelineShaderStageCreateInfo where
        {-# INLINE getField #-}
        getField = vkStage

        {-# INLINE readField #-}
        readField = readVkStage

instance CanWriteField "stage" VkPipelineShaderStageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkStage

instance {-# OVERLAPPING #-}
         HasVkModule VkPipelineShaderStageCreateInfo where
        type VkModuleMType VkPipelineShaderStageCreateInfo = VkShaderModule

        {-# NOINLINE vkModule #-}
        vkModule x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, module})

        {-# INLINE vkModuleByteOffset #-}
        vkModuleByteOffset ~_
          = #{offset VkPipelineShaderStageCreateInfo, module}

        {-# INLINE readVkModule #-}
        readVkModule p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, module}

        {-# INLINE writeVkModule #-}
        writeVkModule p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, module}

instance {-# OVERLAPPING #-}
         HasField "module" VkPipelineShaderStageCreateInfo where
        type FieldType "module" VkPipelineShaderStageCreateInfo =
             VkShaderModule
        type FieldOptional "module" VkPipelineShaderStageCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "module" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, module}
        type FieldIsArray "module" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, module}

instance CanReadField "module" VkPipelineShaderStageCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkModule

        {-# INLINE readField #-}
        readField = readVkModule

instance CanWriteField "module" VkPipelineShaderStageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkModule

instance {-# OVERLAPPING #-}
         HasVkPName VkPipelineShaderStageCreateInfo where
        type VkPNameMType VkPipelineShaderStageCreateInfo = CString

        {-# NOINLINE vkPName #-}
        vkPName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, pName})

        {-# INLINE vkPNameByteOffset #-}
        vkPNameByteOffset ~_
          = #{offset VkPipelineShaderStageCreateInfo, pName}

        {-# INLINE readVkPName #-}
        readVkPName p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, pName}

        {-# INLINE writeVkPName #-}
        writeVkPName p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, pName}

instance {-# OVERLAPPING #-}
         HasField "pName" VkPipelineShaderStageCreateInfo where
        type FieldType "pName" VkPipelineShaderStageCreateInfo = CString
        type FieldOptional "pName" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pName" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, pName}
        type FieldIsArray "pName" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, pName}

instance CanReadField "pName" VkPipelineShaderStageCreateInfo where
        {-# INLINE getField #-}
        getField = vkPName

        {-# INLINE readField #-}
        readField = readVkPName

instance CanWriteField "pName" VkPipelineShaderStageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPName

instance {-# OVERLAPPING #-}
         HasVkPSpecializationInfo VkPipelineShaderStageCreateInfo where
        type VkPSpecializationInfoMType VkPipelineShaderStageCreateInfo =
             Ptr VkSpecializationInfo

        {-# NOINLINE vkPSpecializationInfo #-}
        vkPSpecializationInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo})

        {-# INLINE vkPSpecializationInfoByteOffset #-}
        vkPSpecializationInfoByteOffset ~_
          = #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}

        {-# INLINE readVkPSpecializationInfo #-}
        readVkPSpecializationInfo p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}

        {-# INLINE writeVkPSpecializationInfo #-}
        writeVkPSpecializationInfo p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}

instance {-# OVERLAPPING #-}
         HasField "pSpecializationInfo" VkPipelineShaderStageCreateInfo
         where
        type FieldType "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             = Ptr VkSpecializationInfo
        type FieldOptional "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             =
             #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}
        type FieldIsArray "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}

instance CanReadField "pSpecializationInfo"
           VkPipelineShaderStageCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPSpecializationInfo

        {-# INLINE readField #-}
        readField = readVkPSpecializationInfo

instance CanWriteField "pSpecializationInfo"
           VkPipelineShaderStageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPSpecializationInfo

instance Show VkPipelineShaderStageCreateInfo where
        showsPrec d x
          = showString "VkPipelineShaderStageCreateInfo {" .
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
                                      showString "vkModule = " .
                                        showsPrec d (vkModule x) .
                                          showString ", " .
                                            showString "vkPName = " .
                                              showsPrec d (vkPName x) .
                                                showString ", " .
                                                  showString "vkPSpecializationInfo = " .
                                                    showsPrec d (vkPSpecializationInfo x) .
                                                      showChar '}'
