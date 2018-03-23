#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkObjectTablePushConstantEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipelineLayout             pipelineLayout;
--   >     VkShaderStageFlags           stageFlags;
--   > } VkObjectTablePushConstantEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkObjectTablePushConstantEntryNVX.html VkObjectTablePushConstantEntryNVX registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTablePushConstantEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTablePushConstantEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, type}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTablePushConstantEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTablePushConstantEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "pipelineLayout" VkObjectTablePushConstantEntryNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineLayout" VkObjectTablePushConstantEntryNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, pipelineLayout}

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

instance {-# OVERLAPPING #-}
         CanReadField "stageFlags" VkObjectTablePushConstantEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePushConstantEntryNVX, stageFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePushConstantEntryNVX, stageFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "stageFlags" VkObjectTablePushConstantEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePushConstantEntryNVX, stageFlags}

instance Show VkObjectTablePushConstantEntryNVX where
        showsPrec d x
          = showString "VkObjectTablePushConstantEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) .
                        showString ", " .
                          showString "pipelineLayout = " .
                            showsPrec d (getField @"pipelineLayout" x) .
                              showString ", " .
                                showString "stageFlags = " .
                                  showsPrec d (getField @"stageFlags" x) . showChar '}'
