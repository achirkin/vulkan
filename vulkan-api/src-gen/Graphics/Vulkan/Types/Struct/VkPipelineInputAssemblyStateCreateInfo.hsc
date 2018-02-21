#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineInputAssemblyStateCreateInfo
       (VkPipelineInputAssemblyStateCreateInfo(..)) where
import           Foreign.Storable                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks                 (VkPipelineInputAssemblyStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkPrimitiveTopology (VkPrimitiveTopology)
import           Graphics.Vulkan.Types.Enum.VkStructureType     (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineInputAssemblyStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineInputAssemblyStateCreateFlags    flags;
--   >     VkPrimitiveTopology    topology;
--   >     VkBool32               primitiveRestartEnable;
--   > } VkPipelineInputAssemblyStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineInputAssemblyStateCreateInfo.html VkPipelineInputAssemblyStateCreateInfo registry at www.khronos.org>
data VkPipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo## Addr##
                                                                                      ByteArray##

instance Eq VkPipelineInputAssemblyStateCreateInfo where
        (VkPipelineInputAssemblyStateCreateInfo## a _) ==
          x@(VkPipelineInputAssemblyStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineInputAssemblyStateCreateInfo where
        (VkPipelineInputAssemblyStateCreateInfo## a _) `compare`
          x@(VkPipelineInputAssemblyStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineInputAssemblyStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineInputAssemblyStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineInputAssemblyStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineInputAssemblyStateCreateInfo
         where
        unsafeAddr (VkPipelineInputAssemblyStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineInputAssemblyStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineInputAssemblyStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineInputAssemblyStateCreateInfo where
        type StructFields VkPipelineInputAssemblyStateCreateInfo =
             '["sType", "pNext", "flags", "topology", "primitiveRestartEnable"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineInputAssemblyStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineInputAssemblyStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineInputAssemblyStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineInputAssemblyStateCreateInfo where
        type VkSTypeMType VkPipelineInputAssemblyStateCreateInfo =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineInputAssemblyStateCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "sType" VkPipelineInputAssemblyStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineInputAssemblyStateCreateInfo =
             #{offset VkPipelineInputAssemblyStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, sType}

instance CanReadField "sType"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineInputAssemblyStateCreateInfo where
        type VkPNextMType VkPipelineInputAssemblyStateCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "pNext" VkPipelineInputAssemblyStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineInputAssemblyStateCreateInfo =
             #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}

instance CanReadField "pNext"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineInputAssemblyStateCreateInfo where
        type VkFlagsMType VkPipelineInputAssemblyStateCreateInfo =
             VkPipelineInputAssemblyStateCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineInputAssemblyStateCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "flags" VkPipelineInputAssemblyStateCreateInfo =
             VkPipelineInputAssemblyStateCreateFlags
        type FieldOptional "flags" VkPipelineInputAssemblyStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineInputAssemblyStateCreateInfo =
             #{offset VkPipelineInputAssemblyStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, flags}

instance CanReadField "flags"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkTopology VkPipelineInputAssemblyStateCreateInfo where
        type VkTopologyMType VkPipelineInputAssemblyStateCreateInfo =
             VkPrimitiveTopology

        {-# NOINLINE vkTopology #-}
        vkTopology x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, topology})

        {-# INLINE vkTopologyByteOffset #-}
        vkTopologyByteOffset ~_
          = #{offset VkPipelineInputAssemblyStateCreateInfo, topology}

        {-# INLINE readVkTopology #-}
        readVkTopology p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, topology}

        {-# INLINE writeVkTopology #-}
        writeVkTopology p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, topology}

instance {-# OVERLAPPING #-}
         HasField "topology" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "topology" VkPipelineInputAssemblyStateCreateInfo =
             VkPrimitiveTopology
        type FieldOptional "topology"
               VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "topology" VkPipelineInputAssemblyStateCreateInfo
             =
             #{offset VkPipelineInputAssemblyStateCreateInfo, topology}
        type FieldIsArray "topology" VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, topology}

instance CanReadField "topology"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkTopology

        {-# INLINE readField #-}
        readField = readVkTopology

instance CanWriteField "topology"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkTopology

instance {-# OVERLAPPING #-}
         HasVkPrimitiveRestartEnable VkPipelineInputAssemblyStateCreateInfo
         where
        type VkPrimitiveRestartEnableMType
               VkPipelineInputAssemblyStateCreateInfo
             = VkBool32

        {-# NOINLINE vkPrimitiveRestartEnable #-}
        vkPrimitiveRestartEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable})

        {-# INLINE vkPrimitiveRestartEnableByteOffset #-}
        vkPrimitiveRestartEnableByteOffset ~_
          = #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}

        {-# INLINE readVkPrimitiveRestartEnable #-}
        readVkPrimitiveRestartEnable p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}

        {-# INLINE writeVkPrimitiveRestartEnable #-}
        writeVkPrimitiveRestartEnable p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}

instance {-# OVERLAPPING #-}
         HasField "primitiveRestartEnable"
           VkPipelineInputAssemblyStateCreateInfo
         where
        type FieldType "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             = VkBool32
        type FieldOptional "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             =
             #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}
        type FieldIsArray "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}

instance CanReadField "primitiveRestartEnable"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPrimitiveRestartEnable

        {-# INLINE readField #-}
        readField = readVkPrimitiveRestartEnable

instance CanWriteField "primitiveRestartEnable"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPrimitiveRestartEnable

instance Show VkPipelineInputAssemblyStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineInputAssemblyStateCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkTopology = " .
                                  showsPrec d (vkTopology x) .
                                    showString ", " .
                                      showString "vkPrimitiveRestartEnable = " .
                                        showsPrec d (vkPrimitiveRestartEnable x) . showChar '}'
