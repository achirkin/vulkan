#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.ComputePipelineCreateInfo
       (VkComputePipelineCreateInfo(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Pipeline      (VkPipelineCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkPipeline,
                                                           VkPipelineLayout)
import           Graphics.Vulkan.Types.Struct.Pipeline    (VkPipelineShaderStageCreateInfo)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkComputePipelineCreateInfo VkComputePipelineCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkComputePipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkComputePipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkComputePipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "stage" VkComputePipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, stage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, stage}

instance {-# OVERLAPPING #-}
         CanWriteField "stage" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, stage}

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

instance {-# OVERLAPPING #-}
         CanReadField "layout" VkComputePipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, layout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, layout}

instance {-# OVERLAPPING #-}
         CanWriteField "layout" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, layout}

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

instance {-# OVERLAPPING #-}
         CanReadField "basePipelineHandle" VkComputePipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, basePipelineHandle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, basePipelineHandle}

instance {-# OVERLAPPING #-}
         CanWriteField "basePipelineHandle" VkComputePipelineCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, basePipelineHandle}

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

instance {-# OVERLAPPING #-}
         CanReadField "basePipelineIndex" VkComputePipelineCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkComputePipelineCreateInfo, basePipelineIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkComputePipelineCreateInfo, basePipelineIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "basePipelineIndex" VkComputePipelineCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkComputePipelineCreateInfo, basePipelineIndex}

instance Show VkComputePipelineCreateInfo where
        showsPrec d x
          = showString "VkComputePipelineCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "stage = " .
                                  showsPrec d (getField @"stage" x) .
                                    showString ", " .
                                      showString "layout = " .
                                        showsPrec d (getField @"layout" x) .
                                          showString ", " .
                                            showString "basePipelineHandle = " .
                                              showsPrec d (getField @"basePipelineHandle" x) .
                                                showString ", " .
                                                  showString "basePipelineIndex = " .
                                                    showsPrec d (getField @"basePipelineIndex" x) .
                                                      showChar '}'
