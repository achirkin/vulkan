#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo
       (VkPipelineRasterizationStateCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks             (VkPipelineRasterizationStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkCullModeFlags (VkCullModeFlags)
import           Graphics.Vulkan.Types.Enum.VkFrontFace     (VkFrontFace)
import           Graphics.Vulkan.Types.Enum.VkPolygonMode   (VkPolygonMode)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineRasterizationStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkPipelineRasterizationStateCreateFlags    flags;
--   >     VkBool32               depthClampEnable;
--   >     VkBool32               rasterizerDiscardEnable;
--   >     VkPolygonMode          polygonMode;
--   >     VkCullModeFlags        cullMode;
--   >     VkFrontFace            frontFace;
--   >     VkBool32               depthBiasEnable;
--   >     float                  depthBiasConstantFactor;
--   >     float                  depthBiasClamp;
--   >     float                  depthBiasSlopeFactor;
--   >     float                  lineWidth;
--   > } VkPipelineRasterizationStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineRasterizationStateCreateInfo.html VkPipelineRasterizationStateCreateInfo registry at www.khronos.org>
data VkPipelineRasterizationStateCreateInfo = VkPipelineRasterizationStateCreateInfo## Addr##
                                                                                      ByteArray##

instance Eq VkPipelineRasterizationStateCreateInfo where
        (VkPipelineRasterizationStateCreateInfo## a _) ==
          x@(VkPipelineRasterizationStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineRasterizationStateCreateInfo where
        (VkPipelineRasterizationStateCreateInfo## a _) `compare`
          x@(VkPipelineRasterizationStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineRasterizationStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineRasterizationStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineRasterizationStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineRasterizationStateCreateInfo
         where
        unsafeAddr (VkPipelineRasterizationStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineRasterizationStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineRasterizationStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineRasterizationStateCreateInfo where
        type StructFields VkPipelineRasterizationStateCreateInfo =
             '["sType", "pNext", "flags", "depthClampEnable", -- ' closing tick for hsc2hs
               "rasterizerDiscardEnable", "polygonMode", "cullMode", "frontFace",
               "depthBiasEnable", "depthBiasConstantFactor", "depthBiasClamp",
               "depthBiasSlopeFactor", "lineWidth"]
        type CUnionType VkPipelineRasterizationStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineRasterizationStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineRasterizationStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineRasterizationStateCreateInfo where
        type VkSTypeMType VkPipelineRasterizationStateCreateInfo =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineRasterizationStateCreateInfo where
        type FieldType "sType" VkPipelineRasterizationStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineRasterizationStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineRasterizationStateCreateInfo =
             #{offset VkPipelineRasterizationStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineRasterizationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, sType}

instance CanReadField "sType"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineRasterizationStateCreateInfo where
        type VkPNextMType VkPipelineRasterizationStateCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineRasterizationStateCreateInfo where
        type FieldType "pNext" VkPipelineRasterizationStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineRasterizationStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineRasterizationStateCreateInfo =
             #{offset VkPipelineRasterizationStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineRasterizationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, pNext}

instance CanReadField "pNext"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineRasterizationStateCreateInfo where
        type VkFlagsMType VkPipelineRasterizationStateCreateInfo =
             VkPipelineRasterizationStateCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineRasterizationStateCreateInfo where
        type FieldType "flags" VkPipelineRasterizationStateCreateInfo =
             VkPipelineRasterizationStateCreateFlags
        type FieldOptional "flags" VkPipelineRasterizationStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineRasterizationStateCreateInfo =
             #{offset VkPipelineRasterizationStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineRasterizationStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, flags}

instance CanReadField "flags"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkDepthClampEnable VkPipelineRasterizationStateCreateInfo where
        type VkDepthClampEnableMType VkPipelineRasterizationStateCreateInfo
             = VkBool32

        {-# NOINLINE vkDepthClampEnable #-}
        vkDepthClampEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable})

        {-# INLINE vkDepthClampEnableByteOffset #-}
        vkDepthClampEnableByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}

        {-# INLINE readVkDepthClampEnable #-}
        readVkDepthClampEnable p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}

        {-# INLINE writeVkDepthClampEnable #-}
        writeVkDepthClampEnable p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}

instance {-# OVERLAPPING #-}
         HasField "depthClampEnable" VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "depthClampEnable"
               VkPipelineRasterizationStateCreateInfo
             = VkBool32
        type FieldOptional "depthClampEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthClampEnable"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}
        type FieldIsArray "depthClampEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}

instance CanReadField "depthClampEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDepthClampEnable

        {-# INLINE readField #-}
        readField = readVkDepthClampEnable

instance CanWriteField "depthClampEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDepthClampEnable

instance {-# OVERLAPPING #-}
         HasVkRasterizerDiscardEnable VkPipelineRasterizationStateCreateInfo
         where
        type VkRasterizerDiscardEnableMType
               VkPipelineRasterizationStateCreateInfo
             = VkBool32

        {-# NOINLINE vkRasterizerDiscardEnable #-}
        vkRasterizerDiscardEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable})

        {-# INLINE vkRasterizerDiscardEnableByteOffset #-}
        vkRasterizerDiscardEnableByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}

        {-# INLINE readVkRasterizerDiscardEnable #-}
        readVkRasterizerDiscardEnable p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}

        {-# INLINE writeVkRasterizerDiscardEnable #-}
        writeVkRasterizerDiscardEnable p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}

instance {-# OVERLAPPING #-}
         HasField "rasterizerDiscardEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "rasterizerDiscardEnable"
               VkPipelineRasterizationStateCreateInfo
             = VkBool32
        type FieldOptional "rasterizerDiscardEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "rasterizerDiscardEnable"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}
        type FieldIsArray "rasterizerDiscardEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}

instance CanReadField "rasterizerDiscardEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkRasterizerDiscardEnable

        {-# INLINE readField #-}
        readField = readVkRasterizerDiscardEnable

instance CanWriteField "rasterizerDiscardEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkRasterizerDiscardEnable

instance {-# OVERLAPPING #-}
         HasVkPolygonMode VkPipelineRasterizationStateCreateInfo where
        type VkPolygonModeMType VkPipelineRasterizationStateCreateInfo =
             VkPolygonMode

        {-# NOINLINE vkPolygonMode #-}
        vkPolygonMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, polygonMode})

        {-# INLINE vkPolygonModeByteOffset #-}
        vkPolygonModeByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}

        {-# INLINE readVkPolygonMode #-}
        readVkPolygonMode p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}

        {-# INLINE writeVkPolygonMode #-}
        writeVkPolygonMode p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}

instance {-# OVERLAPPING #-}
         HasField "polygonMode" VkPipelineRasterizationStateCreateInfo where
        type FieldType "polygonMode" VkPipelineRasterizationStateCreateInfo
             = VkPolygonMode
        type FieldOptional "polygonMode"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "polygonMode"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}
        type FieldIsArray "polygonMode"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}

instance CanReadField "polygonMode"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPolygonMode

        {-# INLINE readField #-}
        readField = readVkPolygonMode

instance CanWriteField "polygonMode"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPolygonMode

instance {-# OVERLAPPING #-}
         HasVkCullMode VkPipelineRasterizationStateCreateInfo where
        type VkCullModeMType VkPipelineRasterizationStateCreateInfo =
             VkCullModeFlags

        {-# NOINLINE vkCullMode #-}
        vkCullMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, cullMode})

        {-# INLINE vkCullModeByteOffset #-}
        vkCullModeByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, cullMode}

        {-# INLINE readVkCullMode #-}
        readVkCullMode p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, cullMode}

        {-# INLINE writeVkCullMode #-}
        writeVkCullMode p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, cullMode}

instance {-# OVERLAPPING #-}
         HasField "cullMode" VkPipelineRasterizationStateCreateInfo where
        type FieldType "cullMode" VkPipelineRasterizationStateCreateInfo =
             VkCullModeFlags
        type FieldOptional "cullMode"
               VkPipelineRasterizationStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "cullMode" VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, cullMode}
        type FieldIsArray "cullMode" VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, cullMode}

instance CanReadField "cullMode"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkCullMode

        {-# INLINE readField #-}
        readField = readVkCullMode

instance CanWriteField "cullMode"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkCullMode

instance {-# OVERLAPPING #-}
         HasVkFrontFace VkPipelineRasterizationStateCreateInfo where
        type VkFrontFaceMType VkPipelineRasterizationStateCreateInfo =
             VkFrontFace

        {-# NOINLINE vkFrontFace #-}
        vkFrontFace x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, frontFace})

        {-# INLINE vkFrontFaceByteOffset #-}
        vkFrontFaceByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, frontFace}

        {-# INLINE readVkFrontFace #-}
        readVkFrontFace p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, frontFace}

        {-# INLINE writeVkFrontFace #-}
        writeVkFrontFace p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, frontFace}

instance {-# OVERLAPPING #-}
         HasField "frontFace" VkPipelineRasterizationStateCreateInfo where
        type FieldType "frontFace" VkPipelineRasterizationStateCreateInfo =
             VkFrontFace
        type FieldOptional "frontFace"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "frontFace" VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, frontFace}
        type FieldIsArray "frontFace"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, frontFace}

instance CanReadField "frontFace"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFrontFace

        {-# INLINE readField #-}
        readField = readVkFrontFace

instance CanWriteField "frontFace"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFrontFace

instance {-# OVERLAPPING #-}
         HasVkDepthBiasEnable VkPipelineRasterizationStateCreateInfo where
        type VkDepthBiasEnableMType VkPipelineRasterizationStateCreateInfo
             = VkBool32

        {-# NOINLINE vkDepthBiasEnable #-}
        vkDepthBiasEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable})

        {-# INLINE vkDepthBiasEnableByteOffset #-}
        vkDepthBiasEnableByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}

        {-# INLINE readVkDepthBiasEnable #-}
        readVkDepthBiasEnable p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}

        {-# INLINE writeVkDepthBiasEnable #-}
        writeVkDepthBiasEnable p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}

instance {-# OVERLAPPING #-}
         HasField "depthBiasEnable" VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "depthBiasEnable"
               VkPipelineRasterizationStateCreateInfo
             = VkBool32
        type FieldOptional "depthBiasEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBiasEnable"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}
        type FieldIsArray "depthBiasEnable"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}

instance CanReadField "depthBiasEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDepthBiasEnable

        {-# INLINE readField #-}
        readField = readVkDepthBiasEnable

instance CanWriteField "depthBiasEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDepthBiasEnable

instance {-# OVERLAPPING #-}
         HasVkDepthBiasConstantFactor VkPipelineRasterizationStateCreateInfo
         where
        type VkDepthBiasConstantFactorMType
               VkPipelineRasterizationStateCreateInfo
             = #{type float}

        {-# NOINLINE vkDepthBiasConstantFactor #-}
        vkDepthBiasConstantFactor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor})

        {-# INLINE vkDepthBiasConstantFactorByteOffset #-}
        vkDepthBiasConstantFactorByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}

        {-# INLINE readVkDepthBiasConstantFactor #-}
        readVkDepthBiasConstantFactor p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}

        {-# INLINE writeVkDepthBiasConstantFactor #-}
        writeVkDepthBiasConstantFactor p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}

instance {-# OVERLAPPING #-}
         HasField "depthBiasConstantFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "depthBiasConstantFactor"
               VkPipelineRasterizationStateCreateInfo
             = #{type float}
        type FieldOptional "depthBiasConstantFactor"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBiasConstantFactor"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}
        type FieldIsArray "depthBiasConstantFactor"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}

instance CanReadField "depthBiasConstantFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDepthBiasConstantFactor

        {-# INLINE readField #-}
        readField = readVkDepthBiasConstantFactor

instance CanWriteField "depthBiasConstantFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDepthBiasConstantFactor

instance {-# OVERLAPPING #-}
         HasVkDepthBiasClamp VkPipelineRasterizationStateCreateInfo where
        type VkDepthBiasClampMType VkPipelineRasterizationStateCreateInfo =
             #{type float}

        {-# NOINLINE vkDepthBiasClamp #-}
        vkDepthBiasClamp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp})

        {-# INLINE vkDepthBiasClampByteOffset #-}
        vkDepthBiasClampByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}

        {-# INLINE readVkDepthBiasClamp #-}
        readVkDepthBiasClamp p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}

        {-# INLINE writeVkDepthBiasClamp #-}
        writeVkDepthBiasClamp p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}

instance {-# OVERLAPPING #-}
         HasField "depthBiasClamp" VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "depthBiasClamp"
               VkPipelineRasterizationStateCreateInfo
             = #{type float}
        type FieldOptional "depthBiasClamp"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBiasClamp"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}
        type FieldIsArray "depthBiasClamp"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}

instance CanReadField "depthBiasClamp"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDepthBiasClamp

        {-# INLINE readField #-}
        readField = readVkDepthBiasClamp

instance CanWriteField "depthBiasClamp"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDepthBiasClamp

instance {-# OVERLAPPING #-}
         HasVkDepthBiasSlopeFactor VkPipelineRasterizationStateCreateInfo
         where
        type VkDepthBiasSlopeFactorMType
               VkPipelineRasterizationStateCreateInfo
             = #{type float}

        {-# NOINLINE vkDepthBiasSlopeFactor #-}
        vkDepthBiasSlopeFactor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor})

        {-# INLINE vkDepthBiasSlopeFactorByteOffset #-}
        vkDepthBiasSlopeFactorByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}

        {-# INLINE readVkDepthBiasSlopeFactor #-}
        readVkDepthBiasSlopeFactor p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}

        {-# INLINE writeVkDepthBiasSlopeFactor #-}
        writeVkDepthBiasSlopeFactor p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}

instance {-# OVERLAPPING #-}
         HasField "depthBiasSlopeFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        type FieldType "depthBiasSlopeFactor"
               VkPipelineRasterizationStateCreateInfo
             = #{type float}
        type FieldOptional "depthBiasSlopeFactor"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBiasSlopeFactor"
               VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}
        type FieldIsArray "depthBiasSlopeFactor"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}

instance CanReadField "depthBiasSlopeFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDepthBiasSlopeFactor

        {-# INLINE readField #-}
        readField = readVkDepthBiasSlopeFactor

instance CanWriteField "depthBiasSlopeFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDepthBiasSlopeFactor

instance {-# OVERLAPPING #-}
         HasVkLineWidth VkPipelineRasterizationStateCreateInfo where
        type VkLineWidthMType VkPipelineRasterizationStateCreateInfo =
             #{type float}

        {-# NOINLINE vkLineWidth #-}
        vkLineWidth x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, lineWidth})

        {-# INLINE vkLineWidthByteOffset #-}
        vkLineWidthByteOffset ~_
          = #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}

        {-# INLINE readVkLineWidth #-}
        readVkLineWidth p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}

        {-# INLINE writeVkLineWidth #-}
        writeVkLineWidth p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}

instance {-# OVERLAPPING #-}
         HasField "lineWidth" VkPipelineRasterizationStateCreateInfo where
        type FieldType "lineWidth" VkPipelineRasterizationStateCreateInfo =
             #{type float}
        type FieldOptional "lineWidth"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "lineWidth" VkPipelineRasterizationStateCreateInfo
             =
             #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}
        type FieldIsArray "lineWidth"
               VkPipelineRasterizationStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}

instance CanReadField "lineWidth"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkLineWidth

        {-# INLINE readField #-}
        readField = readVkLineWidth

instance CanWriteField "lineWidth"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkLineWidth

instance Show VkPipelineRasterizationStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineRasterizationStateCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDepthClampEnable = " .
                                  showsPrec d (vkDepthClampEnable x) .
                                    showString ", " .
                                      showString "vkRasterizerDiscardEnable = " .
                                        showsPrec d (vkRasterizerDiscardEnable x) .
                                          showString ", " .
                                            showString "vkPolygonMode = " .
                                              showsPrec d (vkPolygonMode x) .
                                                showString ", " .
                                                  showString "vkCullMode = " .
                                                    showsPrec d (vkCullMode x) .
                                                      showString ", " .
                                                        showString "vkFrontFace = " .
                                                          showsPrec d (vkFrontFace x) .
                                                            showString ", " .
                                                              showString "vkDepthBiasEnable = " .
                                                                showsPrec d (vkDepthBiasEnable x) .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkDepthBiasConstantFactor = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkDepthBiasConstantFactor
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkDepthBiasClamp = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkDepthBiasClamp x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkDepthBiasSlopeFactor = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkDepthBiasSlopeFactor
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "vkLineWidth = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (vkLineWidth
                                                                                             x)
                                                                                          .
                                                                                          showChar
                                                                                            '}'
