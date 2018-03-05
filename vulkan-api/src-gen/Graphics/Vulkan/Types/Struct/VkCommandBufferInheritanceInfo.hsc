#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo
       (VkCommandBufferInheritanceInfo(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkQueryControlFlags           (VkQueryControlFlags)
import           Graphics.Vulkan.Types.Enum.VkQueryPipelineStatisticFlags (VkQueryPipelineStatisticFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Handles                            (VkFramebuffer,
                                                                           VkRenderPass)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkCommandBufferInheritanceInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRenderPass    renderPass;
--   >     uint32_t               subpass;
--   >     VkFramebuffer   framebuffer;
--   >     VkBool32               occlusionQueryEnable;
--   >     VkQueryControlFlags    queryFlags;
--   >     VkQueryPipelineStatisticFlags pipelineStatistics;
--   > } VkCommandBufferInheritanceInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkCommandBufferInheritanceInfo.html VkCommandBufferInheritanceInfo registry at www.khronos.org>
data VkCommandBufferInheritanceInfo = VkCommandBufferInheritanceInfo## Addr##
                                                                      ByteArray##

instance Eq VkCommandBufferInheritanceInfo where
        (VkCommandBufferInheritanceInfo## a _) ==
          x@(VkCommandBufferInheritanceInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandBufferInheritanceInfo where
        (VkCommandBufferInheritanceInfo## a _) `compare`
          x@(VkCommandBufferInheritanceInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandBufferInheritanceInfo where
        sizeOf ~_ = #{size VkCommandBufferInheritanceInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkCommandBufferInheritanceInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandBufferInheritanceInfo where
        unsafeAddr (VkCommandBufferInheritanceInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandBufferInheritanceInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandBufferInheritanceInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandBufferInheritanceInfo where
        type StructFields VkCommandBufferInheritanceInfo =
             '["sType", "pNext", "renderPass", "subpass", "framebuffer", -- ' closing tick for hsc2hs
               "occlusionQueryEnable", "queryFlags", "pipelineStatistics"]
        type CUnionType VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandBufferInheritanceInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandBufferInheritanceInfo where
        type FieldType "sType" VkCommandBufferInheritanceInfo =
             VkStructureType
        type FieldOptional "sType" VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, sType}
        type FieldIsArray "sType" VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCommandBufferInheritanceInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCommandBufferInheritanceInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandBufferInheritanceInfo where
        type FieldType "pNext" VkCommandBufferInheritanceInfo = Ptr Void
        type FieldOptional "pNext" VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, pNext}
        type FieldIsArray "pNext" VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCommandBufferInheritanceInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCommandBufferInheritanceInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "renderPass" VkCommandBufferInheritanceInfo where
        type FieldType "renderPass" VkCommandBufferInheritanceInfo =
             VkRenderPass
        type FieldOptional "renderPass" VkCommandBufferInheritanceInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "renderPass" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, renderPass}
        type FieldIsArray "renderPass" VkCommandBufferInheritanceInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, renderPass}

instance {-# OVERLAPPING #-}
         CanReadField "renderPass" VkCommandBufferInheritanceInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, renderPass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, renderPass}

instance {-# OVERLAPPING #-}
         CanWriteField "renderPass" VkCommandBufferInheritanceInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, renderPass}

instance {-# OVERLAPPING #-}
         HasField "subpass" VkCommandBufferInheritanceInfo where
        type FieldType "subpass" VkCommandBufferInheritanceInfo = Word32
        type FieldOptional "subpass" VkCommandBufferInheritanceInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subpass" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, subpass}
        type FieldIsArray "subpass" VkCommandBufferInheritanceInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, subpass}

instance {-# OVERLAPPING #-}
         CanReadField "subpass" VkCommandBufferInheritanceInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, subpass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, subpass}

instance {-# OVERLAPPING #-}
         CanWriteField "subpass" VkCommandBufferInheritanceInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, subpass}

instance {-# OVERLAPPING #-}
         HasField "framebuffer" VkCommandBufferInheritanceInfo where
        type FieldType "framebuffer" VkCommandBufferInheritanceInfo =
             VkFramebuffer
        type FieldOptional "framebuffer" VkCommandBufferInheritanceInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "framebuffer" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, framebuffer}
        type FieldIsArray "framebuffer" VkCommandBufferInheritanceInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, framebuffer}

instance {-# OVERLAPPING #-}
         CanReadField "framebuffer" VkCommandBufferInheritanceInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, framebuffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, framebuffer}

instance {-# OVERLAPPING #-}
         CanWriteField "framebuffer" VkCommandBufferInheritanceInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, framebuffer}

instance {-# OVERLAPPING #-}
         HasField "occlusionQueryEnable" VkCommandBufferInheritanceInfo
         where
        type FieldType "occlusionQueryEnable"
               VkCommandBufferInheritanceInfo
             = VkBool32
        type FieldOptional "occlusionQueryEnable"
               VkCommandBufferInheritanceInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "occlusionQueryEnable"
               VkCommandBufferInheritanceInfo
             =
             #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable}
        type FieldIsArray "occlusionQueryEnable"
               VkCommandBufferInheritanceInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable}

instance {-# OVERLAPPING #-}
         CanReadField "occlusionQueryEnable" VkCommandBufferInheritanceInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "occlusionQueryEnable" VkCommandBufferInheritanceInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, occlusionQueryEnable}

instance {-# OVERLAPPING #-}
         HasField "queryFlags" VkCommandBufferInheritanceInfo where
        type FieldType "queryFlags" VkCommandBufferInheritanceInfo =
             VkQueryControlFlags
        type FieldOptional "queryFlags" VkCommandBufferInheritanceInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "queryFlags" VkCommandBufferInheritanceInfo =
             #{offset VkCommandBufferInheritanceInfo, queryFlags}
        type FieldIsArray "queryFlags" VkCommandBufferInheritanceInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, queryFlags}

instance {-# OVERLAPPING #-}
         CanReadField "queryFlags" VkCommandBufferInheritanceInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, queryFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, queryFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "queryFlags" VkCommandBufferInheritanceInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, queryFlags}

instance {-# OVERLAPPING #-}
         HasField "pipelineStatistics" VkCommandBufferInheritanceInfo where
        type FieldType "pipelineStatistics" VkCommandBufferInheritanceInfo
             = VkQueryPipelineStatisticFlags
        type FieldOptional "pipelineStatistics"
               VkCommandBufferInheritanceInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pipelineStatistics"
               VkCommandBufferInheritanceInfo
             =
             #{offset VkCommandBufferInheritanceInfo, pipelineStatistics}
        type FieldIsArray "pipelineStatistics"
               VkCommandBufferInheritanceInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferInheritanceInfo, pipelineStatistics}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineStatistics" VkCommandBufferInheritanceInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferInheritanceInfo, pipelineStatistics})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferInheritanceInfo, pipelineStatistics}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineStatistics" VkCommandBufferInheritanceInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferInheritanceInfo, pipelineStatistics}

instance Show VkCommandBufferInheritanceInfo where
        showsPrec d x
          = showString "VkCommandBufferInheritanceInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "renderPass = " .
                            showsPrec d (getField @"renderPass" x) .
                              showString ", " .
                                showString "subpass = " .
                                  showsPrec d (getField @"subpass" x) .
                                    showString ", " .
                                      showString "framebuffer = " .
                                        showsPrec d (getField @"framebuffer" x) .
                                          showString ", " .
                                            showString "occlusionQueryEnable = " .
                                              showsPrec d (getField @"occlusionQueryEnable" x) .
                                                showString ", " .
                                                  showString "queryFlags = " .
                                                    showsPrec d (getField @"queryFlags" x) .
                                                      showString ", " .
                                                        showString "pipelineStatistics = " .
                                                          showsPrec d
                                                            (getField @"pipelineStatistics" x)
                                                            . showChar '}'
