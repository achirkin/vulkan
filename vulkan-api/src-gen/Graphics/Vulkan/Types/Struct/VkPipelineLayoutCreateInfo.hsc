#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineLayoutCreateInfo
       (VkPipelineLayoutCreateInfo(..)) where
import           Foreign.Storable                                 (Storable (..))
import           GHC.Base                                         (Addr##,
                                                                   ByteArray##,
                                                                   byteArrayContents##,
                                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                   (VkPipelineLayoutCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType       (VkStructureType)
import           Graphics.Vulkan.Types.Handles                    (VkDescriptorSetLayout)
import           Graphics.Vulkan.Types.Struct.VkPushConstantRange (VkPushConstantRange)
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineLayoutCreateInfo VkPipelineLayoutCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "setLayoutCount" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, setLayoutCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, setLayoutCount}

instance {-# OVERLAPPING #-}
         CanWriteField "setLayoutCount" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, setLayoutCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pSetLayouts" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pSetLayouts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pSetLayouts}

instance {-# OVERLAPPING #-}
         CanWriteField "pSetLayouts" VkPipelineLayoutCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pSetLayouts}

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

instance {-# OVERLAPPING #-}
         CanReadField "pushConstantRangeCount" VkPipelineLayoutCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}

instance {-# OVERLAPPING #-}
         CanWriteField "pushConstantRangeCount" VkPipelineLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pushConstantRangeCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pPushConstantRanges" VkPipelineLayoutCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}

instance {-# OVERLAPPING #-}
         CanWriteField "pPushConstantRanges" VkPipelineLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineLayoutCreateInfo, pPushConstantRanges}

instance Show VkPipelineLayoutCreateInfo where
        showsPrec d x
          = showString "VkPipelineLayoutCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "setLayoutCount = " .
                                  showsPrec d (getField @"setLayoutCount" x) .
                                    showString ", " .
                                      showString "pSetLayouts = " .
                                        showsPrec d (getField @"pSetLayouts" x) .
                                          showString ", " .
                                            showString "pushConstantRangeCount = " .
                                              showsPrec d (getField @"pushConstantRangeCount" x) .
                                                showString ", " .
                                                  showString "pPushConstantRanges = " .
                                                    showsPrec d (getField @"pPushConstantRanges" x)
                                                      . showChar '}'
