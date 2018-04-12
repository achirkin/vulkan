#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.QueryPoolCreateInfo
       (VkQueryPoolCreateInfo(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkQueryPoolCreateFlags)
import           Graphics.Vulkan.Types.Enum.Query         (VkQueryPipelineStatisticFlags,
                                                           VkQueryType)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkQueryPoolCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkQueryPoolCreateFlags flags;
--   >     VkQueryType            queryType;
--   >     uint32_t               queryCount;
--   >     VkQueryPipelineStatisticFlags pipelineStatistics;
--   > } VkQueryPoolCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkQueryPoolCreateInfo VkQueryPoolCreateInfo registry at www.khronos.org>
data VkQueryPoolCreateInfo = VkQueryPoolCreateInfo## Addr##
                                                    ByteArray##

instance Eq VkQueryPoolCreateInfo where
        (VkQueryPoolCreateInfo## a _) == x@(VkQueryPoolCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkQueryPoolCreateInfo where
        (VkQueryPoolCreateInfo## a _) `compare`
          x@(VkQueryPoolCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkQueryPoolCreateInfo where
        sizeOf ~_ = #{size VkQueryPoolCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkQueryPoolCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkQueryPoolCreateInfo where
        unsafeAddr (VkQueryPoolCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkQueryPoolCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkQueryPoolCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkQueryPoolCreateInfo where
        type StructFields VkQueryPoolCreateInfo =
             '["sType", "pNext", "flags", "queryType", "queryCount", -- ' closing tick for hsc2hs
               "pipelineStatistics"]
        type CUnionType VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkQueryPoolCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkQueryPoolCreateInfo
         where
        type FieldType "sType" VkQueryPoolCreateInfo = VkStructureType
        type FieldOptional "sType" VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkQueryPoolCreateInfo =
             #{offset VkQueryPoolCreateInfo, sType}
        type FieldIsArray "sType" VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkQueryPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkQueryPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueryPoolCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueryPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkQueryPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueryPoolCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkQueryPoolCreateInfo
         where
        type FieldType "pNext" VkQueryPoolCreateInfo = Ptr Void
        type FieldOptional "pNext" VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkQueryPoolCreateInfo =
             #{offset VkQueryPoolCreateInfo, pNext}
        type FieldIsArray "pNext" VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkQueryPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkQueryPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueryPoolCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueryPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkQueryPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueryPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkQueryPoolCreateInfo
         where
        type FieldType "flags" VkQueryPoolCreateInfo =
             VkQueryPoolCreateFlags
        type FieldOptional "flags" VkQueryPoolCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkQueryPoolCreateInfo =
             #{offset VkQueryPoolCreateInfo, flags}
        type FieldIsArray "flags" VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkQueryPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkQueryPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueryPoolCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueryPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkQueryPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueryPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "queryType" VkQueryPoolCreateInfo where
        type FieldType "queryType" VkQueryPoolCreateInfo = VkQueryType
        type FieldOptional "queryType" VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queryType" VkQueryPoolCreateInfo =
             #{offset VkQueryPoolCreateInfo, queryType}
        type FieldIsArray "queryType" VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueryPoolCreateInfo, queryType}

instance {-# OVERLAPPING #-}
         CanReadField "queryType" VkQueryPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueryPoolCreateInfo, queryType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueryPoolCreateInfo, queryType}

instance {-# OVERLAPPING #-}
         CanWriteField "queryType" VkQueryPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueryPoolCreateInfo, queryType}

instance {-# OVERLAPPING #-}
         HasField "queryCount" VkQueryPoolCreateInfo where
        type FieldType "queryCount" VkQueryPoolCreateInfo = Word32
        type FieldOptional "queryCount" VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queryCount" VkQueryPoolCreateInfo =
             #{offset VkQueryPoolCreateInfo, queryCount}
        type FieldIsArray "queryCount" VkQueryPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueryPoolCreateInfo, queryCount}

instance {-# OVERLAPPING #-}
         CanReadField "queryCount" VkQueryPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueryPoolCreateInfo, queryCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueryPoolCreateInfo, queryCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queryCount" VkQueryPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueryPoolCreateInfo, queryCount}

instance {-# OVERLAPPING #-}
         HasField "pipelineStatistics" VkQueryPoolCreateInfo where
        type FieldType "pipelineStatistics" VkQueryPoolCreateInfo =
             VkQueryPipelineStatisticFlags
        type FieldOptional "pipelineStatistics" VkQueryPoolCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pipelineStatistics" VkQueryPoolCreateInfo =
             #{offset VkQueryPoolCreateInfo, pipelineStatistics}
        type FieldIsArray "pipelineStatistics" VkQueryPoolCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkQueryPoolCreateInfo, pipelineStatistics}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineStatistics" VkQueryPoolCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueryPoolCreateInfo, pipelineStatistics})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkQueryPoolCreateInfo, pipelineStatistics}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineStatistics" VkQueryPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkQueryPoolCreateInfo, pipelineStatistics}

instance Show VkQueryPoolCreateInfo where
        showsPrec d x
          = showString "VkQueryPoolCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "queryType = " .
                                  showsPrec d (getField @"queryType" x) .
                                    showString ", " .
                                      showString "queryCount = " .
                                        showsPrec d (getField @"queryCount" x) .
                                          showString ", " .
                                            showString "pipelineStatistics = " .
                                              showsPrec d (getField @"pipelineStatistics" x) .
                                                showChar '}'
