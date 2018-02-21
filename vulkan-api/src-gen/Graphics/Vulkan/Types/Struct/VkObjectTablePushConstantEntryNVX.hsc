#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkObjectTablePushConstantEntryNVX
       (VkObjectTablePushConstantEntryNVX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX       (VkObjectEntryTypeNVX)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX (VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags         (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Handles                         (VkPipelineLayout)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkObjectTablePushConstantEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipelineLayout             pipelineLayout;
--   >     VkShaderStageFlags           stageFlags;
--   > } VkObjectTablePushConstantEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkObjectTablePushConstantEntryNVX.html VkObjectTablePushConstantEntryNVX registry at www.khronos.org>
data VkObjectTablePushConstantEntryNVX = VkObjectTablePushConstantEntryNVX## Addr##
                                                                            ByteArray##

instance Eq VkObjectTablePushConstantEntryNVX where
        (VkObjectTablePushConstantEntryNVX## a _) ==
          x@(VkObjectTablePushConstantEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTablePushConstantEntryNVX where
        (VkObjectTablePushConstantEntryNVX## a _) `compare`
          x@(VkObjectTablePushConstantEntryNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTablePushConstantEntryNVX where
        sizeOf ~_ = #{size VkObjectTablePushConstantEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTablePushConstantEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTablePushConstantEntryNVX where
        unsafeAddr (VkObjectTablePushConstantEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTablePushConstantEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTablePushConstantEntryNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTablePushConstantEntryNVX where
        type StructFields VkObjectTablePushConstantEntryNVX =
             '["type", "flags", "pipelineLayout", "stageFlags"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTablePushConstantEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTablePushConstantEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTablePushConstantEntryNVX = '[] -- ' closing tick for hsc2hs

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
         HasField "type" VkObjectTablePushConstantEntryNVX where
        type FieldType "type" VkObjectTablePushConstantEntryNVX =
             VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTablePushConstantEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTablePushConstantEntryNVX =
             #{offset VkObjectTablePushConstantEntryNVX, type}
        type FieldIsArray "type" VkObjectTablePushConstantEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePushConstantEntryNVX, type}

instance CanReadField "type" VkObjectTablePushConstantEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkType

        {-# INLINE readField #-}
        readField = readVkType

instance CanWriteField "type" VkObjectTablePushConstantEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkType

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
         HasField "flags" VkObjectTablePushConstantEntryNVX where
        type FieldType "flags" VkObjectTablePushConstantEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTablePushConstantEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTablePushConstantEntryNVX =
             #{offset VkObjectTablePushConstantEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTablePushConstantEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePushConstantEntryNVX, flags}

instance CanReadField "flags" VkObjectTablePushConstantEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkObjectTablePushConstantEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

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
         HasField "pipelineLayout" VkObjectTablePushConstantEntryNVX where
        type FieldType "pipelineLayout" VkObjectTablePushConstantEntryNVX =
             VkPipelineLayout
        type FieldOptional "pipelineLayout"
               VkObjectTablePushConstantEntryNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineLayout" VkObjectTablePushConstantEntryNVX
             =
             #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}
        type FieldIsArray "pipelineLayout"
               VkObjectTablePushConstantEntryNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}

instance CanReadField "pipelineLayout"
           VkObjectTablePushConstantEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkPipelineLayout

        {-# INLINE readField #-}
        readField = readVkPipelineLayout

instance CanWriteField "pipelineLayout"
           VkObjectTablePushConstantEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPipelineLayout

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

instance {-# OVERLAPPING #-}
         HasField "stageFlags" VkObjectTablePushConstantEntryNVX where
        type FieldType "stageFlags" VkObjectTablePushConstantEntryNVX =
             VkShaderStageFlags
        type FieldOptional "stageFlags" VkObjectTablePushConstantEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stageFlags" VkObjectTablePushConstantEntryNVX =
             #{offset VkObjectTablePushConstantEntryNVX, stageFlags}
        type FieldIsArray "stageFlags" VkObjectTablePushConstantEntryNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePushConstantEntryNVX, stageFlags}

instance CanReadField "stageFlags"
           VkObjectTablePushConstantEntryNVX
         where
        {-# INLINE getField #-}
        getField = vkStageFlags

        {-# INLINE readField #-}
        readField = readVkStageFlags

instance CanWriteField "stageFlags"
           VkObjectTablePushConstantEntryNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkStageFlags

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
