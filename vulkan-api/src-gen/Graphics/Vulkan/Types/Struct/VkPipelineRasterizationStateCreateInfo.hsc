#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPipelineRasterizationStateCreateInfo.html VkPipelineRasterizationStateCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineRasterizationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineRasterizationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineRasterizationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineRasterizationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineRasterizationStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineRasterizationStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "depthClampEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "depthClampEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthClampEnable}

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

instance {-# OVERLAPPING #-}
         CanReadField "rasterizerDiscardEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "rasterizerDiscardEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, rasterizerDiscardEnable}

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

instance {-# OVERLAPPING #-}
         CanReadField "polygonMode" VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, polygonMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}

instance {-# OVERLAPPING #-}
         CanWriteField "polygonMode" VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, polygonMode}

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

instance {-# OVERLAPPING #-}
         CanReadField "cullMode" VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, cullMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, cullMode}

instance {-# OVERLAPPING #-}
         CanWriteField "cullMode" VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, cullMode}

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

instance {-# OVERLAPPING #-}
         CanReadField "frontFace" VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, frontFace})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, frontFace}

instance {-# OVERLAPPING #-}
         CanWriteField "frontFace" VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, frontFace}

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

instance {-# OVERLAPPING #-}
         CanReadField "depthBiasEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBiasEnable"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasEnable}

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

instance {-# OVERLAPPING #-}
         CanReadField "depthBiasConstantFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBiasConstantFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasConstantFactor}

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

instance {-# OVERLAPPING #-}
         CanReadField "depthBiasClamp"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBiasClamp"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasClamp}

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

instance {-# OVERLAPPING #-}
         CanReadField "depthBiasSlopeFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBiasSlopeFactor"
           VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, depthBiasSlopeFactor}

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

instance {-# OVERLAPPING #-}
         CanReadField "lineWidth" VkPipelineRasterizationStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateCreateInfo, lineWidth})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}

instance {-# OVERLAPPING #-}
         CanWriteField "lineWidth" VkPipelineRasterizationStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateCreateInfo, lineWidth}

instance Show VkPipelineRasterizationStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineRasterizationStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "depthClampEnable = " .
                                  showsPrec d (getField @"depthClampEnable" x) .
                                    showString ", " .
                                      showString "rasterizerDiscardEnable = " .
                                        showsPrec d (getField @"rasterizerDiscardEnable" x) .
                                          showString ", " .
                                            showString "polygonMode = " .
                                              showsPrec d (getField @"polygonMode" x) .
                                                showString ", " .
                                                  showString "cullMode = " .
                                                    showsPrec d (getField @"cullMode" x) .
                                                      showString ", " .
                                                        showString "frontFace = " .
                                                          showsPrec d (getField @"frontFace" x) .
                                                            showString ", " .
                                                              showString "depthBiasEnable = " .
                                                                showsPrec d
                                                                  (getField @"depthBiasEnable" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "depthBiasConstantFactor = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"depthBiasConstantFactor"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "depthBiasClamp = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"depthBiasClamp"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "depthBiasSlopeFactor = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"depthBiasSlopeFactor"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "lineWidth = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"lineWidth"
                                                                                             x)
                                                                                          .
                                                                                          showChar
                                                                                            '}'
