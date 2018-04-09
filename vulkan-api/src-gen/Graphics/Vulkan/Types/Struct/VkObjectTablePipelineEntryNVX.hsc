#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkObjectTablePipelineEntryNVX
       (VkObjectTablePipelineEntryNVX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX       (VkObjectEntryTypeNVX)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX (VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.Handles                         (VkPipeline)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkObjectTablePipelineEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   >     VkPipeline                   pipeline;
--   > } VkObjectTablePipelineEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkObjectTablePipelineEntryNVXVkObjectTablePipelineEntryNVX registry at www.khronos.org>
data VkObjectTablePipelineEntryNVX = VkObjectTablePipelineEntryNVX## Addr##
                                                                    ByteArray##

instance Eq VkObjectTablePipelineEntryNVX where
        (VkObjectTablePipelineEntryNVX## a _) ==
          x@(VkObjectTablePipelineEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTablePipelineEntryNVX where
        (VkObjectTablePipelineEntryNVX## a _) `compare`
          x@(VkObjectTablePipelineEntryNVX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTablePipelineEntryNVX where
        sizeOf ~_ = #{size VkObjectTablePipelineEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkObjectTablePipelineEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTablePipelineEntryNVX where
        unsafeAddr (VkObjectTablePipelineEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTablePipelineEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTablePipelineEntryNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTablePipelineEntryNVX where
        type StructFields VkObjectTablePipelineEntryNVX =
             '["type", "flags", "pipeline"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTablePipelineEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "type" VkObjectTablePipelineEntryNVX where
        type FieldType "type" VkObjectTablePipelineEntryNVX =
             VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTablePipelineEntryNVX =
             #{offset VkObjectTablePipelineEntryNVX, type}
        type FieldIsArray "type" VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePipelineEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanReadField "type" VkObjectTablePipelineEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePipelineEntryNVX, type})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePipelineEntryNVX, type}

instance {-# OVERLAPPING #-}
         CanWriteField "type" VkObjectTablePipelineEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePipelineEntryNVX, type}

instance {-# OVERLAPPING #-}
         HasField "flags" VkObjectTablePipelineEntryNVX where
        type FieldType "flags" VkObjectTablePipelineEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTablePipelineEntryNVX =
             #{offset VkObjectTablePipelineEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePipelineEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkObjectTablePipelineEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePipelineEntryNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePipelineEntryNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkObjectTablePipelineEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePipelineEntryNVX, flags}

instance {-# OVERLAPPING #-}
         HasField "pipeline" VkObjectTablePipelineEntryNVX where
        type FieldType "pipeline" VkObjectTablePipelineEntryNVX =
             VkPipeline
        type FieldOptional "pipeline" VkObjectTablePipelineEntryNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pipeline" VkObjectTablePipelineEntryNVX =
             #{offset VkObjectTablePipelineEntryNVX, pipeline}
        type FieldIsArray "pipeline" VkObjectTablePipelineEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkObjectTablePipelineEntryNVX, pipeline}

instance {-# OVERLAPPING #-}
         CanReadField "pipeline" VkObjectTablePipelineEntryNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTablePipelineEntryNVX, pipeline})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkObjectTablePipelineEntryNVX, pipeline}

instance {-# OVERLAPPING #-}
         CanWriteField "pipeline" VkObjectTablePipelineEntryNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkObjectTablePipelineEntryNVX, pipeline}

instance Show VkObjectTablePipelineEntryNVX where
        showsPrec d x
          = showString "VkObjectTablePipelineEntryNVX {" .
              showString "type = " .
                showsPrec d (getField @"type" x) .
                  showString ", " .
                    showString "flags = " .
                      showsPrec d (getField @"flags" x) .
                        showString ", " .
                          showString "pipeline = " .
                            showsPrec d (getField @"pipeline" x) . showChar '}'
