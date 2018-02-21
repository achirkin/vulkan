#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineDepthStencilStateCreateInfo
       (VkPipelineDepthStencilStateCreateInfo(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks                (VkPipelineDepthStencilStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkCompareOp        (VkCompareOp)
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkStencilOpState (VkStencilOpState)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineDepthStencilStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineDepthStencilStateCreateFlags    flags;
--   >     VkBool32               depthTestEnable;
--   >     VkBool32               depthWriteEnable;
--   >     VkCompareOp            depthCompareOp;
--   >     VkBool32               depthBoundsTestEnable;
--   >     VkBool32               stencilTestEnable;
--   >     VkStencilOpState       front;
--   >     VkStencilOpState       back;
--   >     float                  minDepthBounds;
--   >     float                  maxDepthBounds;
--   > } VkPipelineDepthStencilStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineDepthStencilStateCreateInfo.html VkPipelineDepthStencilStateCreateInfo registry at www.khronos.org>
data VkPipelineDepthStencilStateCreateInfo = VkPipelineDepthStencilStateCreateInfo## Addr##
                                                                                    ByteArray##

instance Eq VkPipelineDepthStencilStateCreateInfo where
        (VkPipelineDepthStencilStateCreateInfo## a _) ==
          x@(VkPipelineDepthStencilStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineDepthStencilStateCreateInfo where
        (VkPipelineDepthStencilStateCreateInfo## a _) `compare`
          x@(VkPipelineDepthStencilStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineDepthStencilStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineDepthStencilStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineDepthStencilStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineDepthStencilStateCreateInfo
         where
        unsafeAddr (VkPipelineDepthStencilStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineDepthStencilStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineDepthStencilStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineDepthStencilStateCreateInfo where
        type StructFields VkPipelineDepthStencilStateCreateInfo =
             '["sType", "pNext", "flags", "depthTestEnable", "depthWriteEnable", -- ' closing tick for hsc2hs
               "depthCompareOp", "depthBoundsTestEnable", "stencilTestEnable",
               "front", "back", "minDepthBounds", "maxDepthBounds"]
        type CUnionType VkPipelineDepthStencilStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineDepthStencilStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineDepthStencilStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineDepthStencilStateCreateInfo where
        type VkSTypeMType VkPipelineDepthStencilStateCreateInfo =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "sType" VkPipelineDepthStencilStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, sType}

instance CanReadField "sType" VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineDepthStencilStateCreateInfo where
        type VkPNextMType VkPipelineDepthStencilStateCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "pNext" VkPipelineDepthStencilStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, pNext}

instance CanReadField "pNext" VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineDepthStencilStateCreateInfo where
        type VkFlagsMType VkPipelineDepthStencilStateCreateInfo =
             VkPipelineDepthStencilStateCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "flags" VkPipelineDepthStencilStateCreateInfo =
             VkPipelineDepthStencilStateCreateFlags
        type FieldOptional "flags" VkPipelineDepthStencilStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, flags}

instance CanReadField "flags" VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkDepthTestEnable VkPipelineDepthStencilStateCreateInfo where
        type VkDepthTestEnableMType VkPipelineDepthStencilStateCreateInfo =
             VkBool32

        {-# NOINLINE vkDepthTestEnable #-}
        vkDepthTestEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable})

        {-# INLINE vkDepthTestEnableByteOffset #-}
        vkDepthTestEnableByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}

        {-# INLINE readVkDepthTestEnable #-}
        readVkDepthTestEnable p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}

        {-# INLINE writeVkDepthTestEnable #-}
        writeVkDepthTestEnable p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}

instance {-# OVERLAPPING #-}
         HasField "depthTestEnable" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}
        type FieldIsArray "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}

instance CanReadField "depthTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDepthTestEnable

        {-# INLINE readField #-}
        readField = readVkDepthTestEnable

instance CanWriteField "depthTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDepthTestEnable

instance {-# OVERLAPPING #-}
         HasVkDepthWriteEnable VkPipelineDepthStencilStateCreateInfo where
        type VkDepthWriteEnableMType VkPipelineDepthStencilStateCreateInfo
             = VkBool32

        {-# NOINLINE vkDepthWriteEnable #-}
        vkDepthWriteEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable})

        {-# INLINE vkDepthWriteEnableByteOffset #-}
        vkDepthWriteEnableByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}

        {-# INLINE readVkDepthWriteEnable #-}
        readVkDepthWriteEnable p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}

        {-# INLINE writeVkDepthWriteEnable #-}
        writeVkDepthWriteEnable p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}

instance {-# OVERLAPPING #-}
         HasField "depthWriteEnable" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}
        type FieldIsArray "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}

instance CanReadField "depthWriteEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDepthWriteEnable

        {-# INLINE readField #-}
        readField = readVkDepthWriteEnable

instance CanWriteField "depthWriteEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDepthWriteEnable

instance {-# OVERLAPPING #-}
         HasVkDepthCompareOp VkPipelineDepthStencilStateCreateInfo where
        type VkDepthCompareOpMType VkPipelineDepthStencilStateCreateInfo =
             VkCompareOp

        {-# NOINLINE vkDepthCompareOp #-}
        vkDepthCompareOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp})

        {-# INLINE vkDepthCompareOpByteOffset #-}
        vkDepthCompareOpByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}

        {-# INLINE readVkDepthCompareOp #-}
        readVkDepthCompareOp p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}

        {-# INLINE writeVkDepthCompareOp #-}
        writeVkDepthCompareOp p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}

instance {-# OVERLAPPING #-}
         HasField "depthCompareOp" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             = VkCompareOp
        type FieldOptional "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}
        type FieldIsArray "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}

instance CanReadField "depthCompareOp"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDepthCompareOp

        {-# INLINE readField #-}
        readField = readVkDepthCompareOp

instance CanWriteField "depthCompareOp"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDepthCompareOp

instance {-# OVERLAPPING #-}
         HasVkDepthBoundsTestEnable VkPipelineDepthStencilStateCreateInfo
         where
        type VkDepthBoundsTestEnableMType
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32

        {-# NOINLINE vkDepthBoundsTestEnable #-}
        vkDepthBoundsTestEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable})

        {-# INLINE vkDepthBoundsTestEnableByteOffset #-}
        vkDepthBoundsTestEnableByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}

        {-# INLINE readVkDepthBoundsTestEnable #-}
        readVkDepthBoundsTestEnable p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}

        {-# INLINE writeVkDepthBoundsTestEnable #-}
        writeVkDepthBoundsTestEnable p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}

instance {-# OVERLAPPING #-}
         HasField "depthBoundsTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}
        type FieldIsArray "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}

instance CanReadField "depthBoundsTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDepthBoundsTestEnable

        {-# INLINE readField #-}
        readField = readVkDepthBoundsTestEnable

instance CanWriteField "depthBoundsTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDepthBoundsTestEnable

instance {-# OVERLAPPING #-}
         HasVkStencilTestEnable VkPipelineDepthStencilStateCreateInfo where
        type VkStencilTestEnableMType VkPipelineDepthStencilStateCreateInfo
             = VkBool32

        {-# NOINLINE vkStencilTestEnable #-}
        vkStencilTestEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable})

        {-# INLINE vkStencilTestEnableByteOffset #-}
        vkStencilTestEnableByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}

        {-# INLINE readVkStencilTestEnable #-}
        readVkStencilTestEnable p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}

        {-# INLINE writeVkStencilTestEnable #-}
        writeVkStencilTestEnable p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}

instance {-# OVERLAPPING #-}
         HasField "stencilTestEnable" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}
        type FieldIsArray "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}

instance CanReadField "stencilTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkStencilTestEnable

        {-# INLINE readField #-}
        readField = readVkStencilTestEnable

instance CanWriteField "stencilTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkStencilTestEnable

instance {-# OVERLAPPING #-}
         HasVkFront VkPipelineDepthStencilStateCreateInfo where
        type VkFrontMType VkPipelineDepthStencilStateCreateInfo =
             VkStencilOpState

        {-# NOINLINE vkFront #-}
        vkFront x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, front})

        {-# INLINE vkFrontByteOffset #-}
        vkFrontByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, front}

        {-# INLINE readVkFront #-}
        readVkFront p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, front}

        {-# INLINE writeVkFront #-}
        writeVkFront p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, front}

instance {-# OVERLAPPING #-}
         HasField "front" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "front" VkPipelineDepthStencilStateCreateInfo =
             VkStencilOpState
        type FieldOptional "front" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "front" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, front}
        type FieldIsArray "front" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, front}

instance CanReadField "front" VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFront

        {-# INLINE readField #-}
        readField = readVkFront

instance CanWriteField "front"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFront

instance {-# OVERLAPPING #-}
         HasVkBack VkPipelineDepthStencilStateCreateInfo where
        type VkBackMType VkPipelineDepthStencilStateCreateInfo =
             VkStencilOpState

        {-# NOINLINE vkBack #-}
        vkBack x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, back})

        {-# INLINE vkBackByteOffset #-}
        vkBackByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, back}

        {-# INLINE readVkBack #-}
        readVkBack p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, back}

        {-# INLINE writeVkBack #-}
        writeVkBack p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, back}

instance {-# OVERLAPPING #-}
         HasField "back" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "back" VkPipelineDepthStencilStateCreateInfo =
             VkStencilOpState
        type FieldOptional "back" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "back" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, back}
        type FieldIsArray "back" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, back}

instance CanReadField "back" VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkBack

        {-# INLINE readField #-}
        readField = readVkBack

instance CanWriteField "back" VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkBack

instance {-# OVERLAPPING #-}
         HasVkMinDepthBounds VkPipelineDepthStencilStateCreateInfo where
        type VkMinDepthBoundsMType VkPipelineDepthStencilStateCreateInfo =
             #{type float}

        {-# NOINLINE vkMinDepthBounds #-}
        vkMinDepthBounds x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds})

        {-# INLINE vkMinDepthBoundsByteOffset #-}
        vkMinDepthBoundsByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}

        {-# INLINE readVkMinDepthBounds #-}
        readVkMinDepthBounds p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}

        {-# INLINE writeVkMinDepthBounds #-}
        writeVkMinDepthBounds p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}

instance {-# OVERLAPPING #-}
         HasField "minDepthBounds" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = #{type float}
        type FieldOptional "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}
        type FieldIsArray "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}

instance CanReadField "minDepthBounds"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkMinDepthBounds

        {-# INLINE readField #-}
        readField = readVkMinDepthBounds

instance CanWriteField "minDepthBounds"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinDepthBounds

instance {-# OVERLAPPING #-}
         HasVkMaxDepthBounds VkPipelineDepthStencilStateCreateInfo where
        type VkMaxDepthBoundsMType VkPipelineDepthStencilStateCreateInfo =
             #{type float}

        {-# NOINLINE vkMaxDepthBounds #-}
        vkMaxDepthBounds x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds})

        {-# INLINE vkMaxDepthBoundsByteOffset #-}
        vkMaxDepthBoundsByteOffset ~_
          = #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}

        {-# INLINE readVkMaxDepthBounds #-}
        readVkMaxDepthBounds p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}

        {-# INLINE writeVkMaxDepthBounds #-}
        writeVkMaxDepthBounds p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}

instance {-# OVERLAPPING #-}
         HasField "maxDepthBounds" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = #{type float}
        type FieldOptional "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}
        type FieldIsArray "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}

instance CanReadField "maxDepthBounds"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkMaxDepthBounds

        {-# INLINE readField #-}
        readField = readVkMaxDepthBounds

instance CanWriteField "maxDepthBounds"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkMaxDepthBounds

instance Show VkPipelineDepthStencilStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineDepthStencilStateCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDepthTestEnable = " .
                                  showsPrec d (vkDepthTestEnable x) .
                                    showString ", " .
                                      showString "vkDepthWriteEnable = " .
                                        showsPrec d (vkDepthWriteEnable x) .
                                          showString ", " .
                                            showString "vkDepthCompareOp = " .
                                              showsPrec d (vkDepthCompareOp x) .
                                                showString ", " .
                                                  showString "vkDepthBoundsTestEnable = " .
                                                    showsPrec d (vkDepthBoundsTestEnable x) .
                                                      showString ", " .
                                                        showString "vkStencilTestEnable = " .
                                                          showsPrec d (vkStencilTestEnable x) .
                                                            showString ", " .
                                                              showString "vkFront = " .
                                                                showsPrec d (vkFront x) .
                                                                  showString ", " .
                                                                    showString "vkBack = " .
                                                                      showsPrec d (vkBack x) .
                                                                        showString ", " .
                                                                          showString
                                                                            "vkMinDepthBounds = "
                                                                            .
                                                                            showsPrec d
                                                                              (vkMinDepthBounds x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "vkMaxDepthBounds = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (vkMaxDepthBounds
                                                                                       x)
                                                                                    . showChar '}'
