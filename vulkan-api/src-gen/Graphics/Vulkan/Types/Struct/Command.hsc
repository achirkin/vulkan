#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Command
       (VkCommandBufferAllocateInfo(..), VkCommandBufferBeginInfo(..),
        VkCommandBufferInheritanceInfo(..), VkCommandPoolCreateInfo(..))
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import           Graphics.Vulkan.Types.Enum.Command       (VkCommandBufferLevel, VkCommandBufferUsageFlags,
                                                           VkCommandPoolCreateFlags)
import           Graphics.Vulkan.Types.Enum.Query         (VkQueryControlFlags, VkQueryPipelineStatisticFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkCommandPool,
                                                           VkFramebuffer,
                                                           VkRenderPass)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkCommandBufferAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandPool          commandPool;
--   >     VkCommandBufferLevel   level;
--   >     uint32_t               commandBufferCount;
--   > } VkCommandBufferAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandBufferAllocateInfo VkCommandBufferAllocateInfo registry at www.khronos.org>
data VkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo## Addr##
                                                                ByteArray##

instance Eq VkCommandBufferAllocateInfo where
        (VkCommandBufferAllocateInfo## a _) ==
          x@(VkCommandBufferAllocateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandBufferAllocateInfo where
        (VkCommandBufferAllocateInfo## a _) `compare`
          x@(VkCommandBufferAllocateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandBufferAllocateInfo where
        sizeOf ~_ = #{size VkCommandBufferAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCommandBufferAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandBufferAllocateInfo where
        unsafeAddr (VkCommandBufferAllocateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandBufferAllocateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandBufferAllocateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandBufferAllocateInfo where
        type StructFields VkCommandBufferAllocateInfo =
             '["sType", "pNext", "commandPool", "level", "commandBufferCount"] -- ' closing tick for hsc2hs
        type CUnionType VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandBufferAllocateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandBufferAllocateInfo where
        type FieldType "sType" VkCommandBufferAllocateInfo =
             VkStructureType
        type FieldOptional "sType" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, sType}
        type FieldIsArray "sType" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCommandBufferAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandBufferAllocateInfo where
        type FieldType "pNext" VkCommandBufferAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, pNext}
        type FieldIsArray "pNext" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCommandBufferAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "commandPool" VkCommandBufferAllocateInfo where
        type FieldType "commandPool" VkCommandBufferAllocateInfo =
             VkCommandPool
        type FieldOptional "commandPool" VkCommandBufferAllocateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "commandPool" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, commandPool}
        type FieldIsArray "commandPool" VkCommandBufferAllocateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, commandPool}

instance {-# OVERLAPPING #-}
         CanReadField "commandPool" VkCommandBufferAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, commandPool})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, commandPool}

instance {-# OVERLAPPING #-}
         CanWriteField "commandPool" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, commandPool}

instance {-# OVERLAPPING #-}
         HasField "level" VkCommandBufferAllocateInfo where
        type FieldType "level" VkCommandBufferAllocateInfo =
             VkCommandBufferLevel
        type FieldOptional "level" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "level" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, level}
        type FieldIsArray "level" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, level}

instance {-# OVERLAPPING #-}
         CanReadField "level" VkCommandBufferAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, level})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, level}

instance {-# OVERLAPPING #-}
         CanWriteField "level" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, level}

instance {-# OVERLAPPING #-}
         HasField "commandBufferCount" VkCommandBufferAllocateInfo where
        type FieldType "commandBufferCount" VkCommandBufferAllocateInfo =
             Word32
        type FieldOptional "commandBufferCount" VkCommandBufferAllocateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "commandBufferCount" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, commandBufferCount}
        type FieldIsArray "commandBufferCount" VkCommandBufferAllocateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         CanReadField "commandBufferCount" VkCommandBufferAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, commandBufferCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         CanWriteField "commandBufferCount" VkCommandBufferAllocateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, commandBufferCount}

instance Show VkCommandBufferAllocateInfo where
        showsPrec d x
          = showString "VkCommandBufferAllocateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "commandPool = " .
                            showsPrec d (getField @"commandPool" x) .
                              showString ", " .
                                showString "level = " .
                                  showsPrec d (getField @"level" x) .
                                    showString ", " .
                                      showString "commandBufferCount = " .
                                        showsPrec d (getField @"commandBufferCount" x) .
                                          showChar '}'

-- | > typedef struct VkCommandBufferBeginInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandBufferUsageFlags  flags;
--   >     const VkCommandBufferInheritanceInfo*       pInheritanceInfo;
--   > } VkCommandBufferBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandBufferBeginInfo VkCommandBufferBeginInfo registry at www.khronos.org>
data VkCommandBufferBeginInfo = VkCommandBufferBeginInfo## Addr##
                                                          ByteArray##

instance Eq VkCommandBufferBeginInfo where
        (VkCommandBufferBeginInfo## a _) ==
          x@(VkCommandBufferBeginInfo## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandBufferBeginInfo where
        (VkCommandBufferBeginInfo## a _) `compare`
          x@(VkCommandBufferBeginInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandBufferBeginInfo where
        sizeOf ~_ = #{size VkCommandBufferBeginInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCommandBufferBeginInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandBufferBeginInfo where
        unsafeAddr (VkCommandBufferBeginInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandBufferBeginInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandBufferBeginInfo## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandBufferBeginInfo where
        type StructFields VkCommandBufferBeginInfo =
             '["sType", "pNext", "flags", "pInheritanceInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandBufferBeginInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandBufferBeginInfo where
        type FieldType "sType" VkCommandBufferBeginInfo = VkStructureType
        type FieldOptional "sType" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, sType}
        type FieldIsArray "sType" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandBufferBeginInfo where
        type FieldType "pNext" VkCommandBufferBeginInfo = Ptr Void
        type FieldOptional "pNext" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, pNext}
        type FieldIsArray "pNext" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkCommandBufferBeginInfo where
        type FieldType "flags" VkCommandBufferBeginInfo =
             VkCommandBufferUsageFlags
        type FieldOptional "flags" VkCommandBufferBeginInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, flags}
        type FieldIsArray "flags" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandBufferBeginInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "pInheritanceInfo" VkCommandBufferBeginInfo where
        type FieldType "pInheritanceInfo" VkCommandBufferBeginInfo =
             Ptr VkCommandBufferInheritanceInfo
        type FieldOptional "pInheritanceInfo" VkCommandBufferBeginInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pInheritanceInfo" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, pInheritanceInfo}
        type FieldIsArray "pInheritanceInfo" VkCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferBeginInfo, pInheritanceInfo}

instance {-# OVERLAPPING #-}
         CanReadField "pInheritanceInfo" VkCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, pInheritanceInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, pInheritanceInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "pInheritanceInfo" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, pInheritanceInfo}

instance Show VkCommandBufferBeginInfo where
        showsPrec d x
          = showString "VkCommandBufferBeginInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "pInheritanceInfo = " .
                                  showsPrec d (getField @"pInheritanceInfo" x) . showChar '}'

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandBufferInheritanceInfo VkCommandBufferInheritanceInfo registry at www.khronos.org>
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

-- | > typedef struct VkCommandPoolCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandPoolCreateFlags   flags;
--   >     uint32_t               queueFamilyIndex;
--   > } VkCommandPoolCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkCommandPoolCreateInfo VkCommandPoolCreateInfo registry at www.khronos.org>
data VkCommandPoolCreateInfo = VkCommandPoolCreateInfo## Addr##
                                                        ByteArray##

instance Eq VkCommandPoolCreateInfo where
        (VkCommandPoolCreateInfo## a _) == x@(VkCommandPoolCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandPoolCreateInfo where
        (VkCommandPoolCreateInfo## a _) `compare`
          x@(VkCommandPoolCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandPoolCreateInfo where
        sizeOf ~_ = #{size VkCommandPoolCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCommandPoolCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandPoolCreateInfo where
        unsafeAddr (VkCommandPoolCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandPoolCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandPoolCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandPoolCreateInfo where
        type StructFields VkCommandPoolCreateInfo =
             '["sType", "pNext", "flags", "queueFamilyIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandPoolCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandPoolCreateInfo where
        type FieldType "sType" VkCommandPoolCreateInfo = VkStructureType
        type FieldOptional "sType" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, sType}
        type FieldIsArray "sType" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCommandPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandPoolCreateInfo where
        type FieldType "pNext" VkCommandPoolCreateInfo = Ptr Void
        type FieldOptional "pNext" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, pNext}
        type FieldIsArray "pNext" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCommandPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkCommandPoolCreateInfo where
        type FieldType "flags" VkCommandPoolCreateInfo =
             VkCommandPoolCreateFlags
        type FieldOptional "flags" VkCommandPoolCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, flags}
        type FieldIsArray "flags" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkCommandPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndex" VkCommandPoolCreateInfo where
        type FieldType "queueFamilyIndex" VkCommandPoolCreateInfo = Word32
        type FieldOptional "queueFamilyIndex" VkCommandPoolCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndex" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, queueFamilyIndex}
        type FieldIsArray "queueFamilyIndex" VkCommandPoolCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandPoolCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyIndex" VkCommandPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, queueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyIndex" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, queueFamilyIndex}

instance Show VkCommandPoolCreateInfo where
        showsPrec d x
          = showString "VkCommandPoolCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "queueFamilyIndex = " .
                                  showsPrec d (getField @"queueFamilyIndex" x) . showChar '}'
