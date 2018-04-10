#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineVertexInputStateCreateInfo
       (VkPipelineVertexInputStateCreateInfo(..)) where
import           Foreign.Storable                                               (Storable (..))
import           GHC.Base                                                       (Addr##,
                                                                                 ByteArray##,
                                                                                 byteArrayContents##,
                                                                                 plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                                 (VkPipelineVertexInputStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkVertexInputAttributeDescription (VkVertexInputAttributeDescription)
import           Graphics.Vulkan.Types.Struct.VkVertexInputBindingDescription   (VkVertexInputBindingDescription)
import           System.IO.Unsafe                                               (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineVertexInputStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineVertexInputStateCreateFlags    flags;
--   >     uint32_t               vertexBindingDescriptionCount;
--   >     const VkVertexInputBindingDescription* pVertexBindingDescriptions;
--   >     uint32_t               vertexAttributeDescriptionCount;
--   >     const VkVertexInputAttributeDescription* pVertexAttributeDescriptions;
--   > } VkPipelineVertexInputStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPipelineVertexInputStateCreateInfo VkPipelineVertexInputStateCreateInfo registry at www.khronos.org>
data VkPipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo## Addr##
                                                                                  ByteArray##

instance Eq VkPipelineVertexInputStateCreateInfo where
        (VkPipelineVertexInputStateCreateInfo## a _) ==
          x@(VkPipelineVertexInputStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineVertexInputStateCreateInfo where
        (VkPipelineVertexInputStateCreateInfo## a _) `compare`
          x@(VkPipelineVertexInputStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineVertexInputStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineVertexInputStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineVertexInputStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineVertexInputStateCreateInfo
         where
        unsafeAddr (VkPipelineVertexInputStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineVertexInputStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineVertexInputStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineVertexInputStateCreateInfo where
        type StructFields VkPipelineVertexInputStateCreateInfo =
             '["sType", "pNext", "flags", "vertexBindingDescriptionCount", -- ' closing tick for hsc2hs
               "pVertexBindingDescriptions", "vertexAttributeDescriptionCount",
               "pVertexAttributeDescriptions"]
        type CUnionType VkPipelineVertexInputStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineVertexInputStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineVertexInputStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineVertexInputStateCreateInfo where
        type FieldType "sType" VkPipelineVertexInputStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineVertexInputStateCreateInfo =
             #{offset VkPipelineVertexInputStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineVertexInputStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineVertexInputStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineVertexInputStateCreateInfo where
        type FieldType "pNext" VkPipelineVertexInputStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineVertexInputStateCreateInfo =
             #{offset VkPipelineVertexInputStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineVertexInputStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineVertexInputStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineVertexInputStateCreateInfo where
        type FieldType "flags" VkPipelineVertexInputStateCreateInfo =
             VkPipelineVertexInputStateCreateFlags
        type FieldOptional "flags" VkPipelineVertexInputStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineVertexInputStateCreateInfo =
             #{offset VkPipelineVertexInputStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineVertexInputStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineVertexInputStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineVertexInputStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "vertexBindingDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = Word32
        type FieldOptional "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}
        type FieldIsArray "vertexBindingDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}

instance {-# OVERLAPPING #-}
         CanReadField "vertexBindingDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}

instance {-# OVERLAPPING #-}
         CanWriteField "vertexBindingDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexBindingDescriptionCount}

instance {-# OVERLAPPING #-}
         HasField "pVertexBindingDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = Ptr VkVertexInputBindingDescription
        type FieldOptional "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}
        type FieldIsArray "pVertexBindingDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}

instance {-# OVERLAPPING #-}
         CanReadField "pVertexBindingDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}

instance {-# OVERLAPPING #-}
         CanWriteField "pVertexBindingDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexBindingDescriptions}

instance {-# OVERLAPPING #-}
         HasField "vertexAttributeDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = Word32
        type FieldOptional "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}
        type FieldIsArray "vertexAttributeDescriptionCount"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}

instance {-# OVERLAPPING #-}
         CanReadField "vertexAttributeDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}

instance {-# OVERLAPPING #-}
         CanWriteField "vertexAttributeDescriptionCount"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, vertexAttributeDescriptionCount}

instance {-# OVERLAPPING #-}
         HasField "pVertexAttributeDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        type FieldType "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = Ptr VkVertexInputAttributeDescription
        type FieldOptional "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             =
             #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}
        type FieldIsArray "pVertexAttributeDescriptions"
               VkPipelineVertexInputStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}

instance {-# OVERLAPPING #-}
         CanReadField "pVertexAttributeDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}

instance {-# OVERLAPPING #-}
         CanWriteField "pVertexAttributeDescriptions"
           VkPipelineVertexInputStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputStateCreateInfo, pVertexAttributeDescriptions}

instance Show VkPipelineVertexInputStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineVertexInputStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "vertexBindingDescriptionCount = " .
                                  showsPrec d (getField @"vertexBindingDescriptionCount" x) .
                                    showString ", " .
                                      showString "pVertexBindingDescriptions = " .
                                        showsPrec d (getField @"pVertexBindingDescriptions" x) .
                                          showString ", " .
                                            showString "vertexAttributeDescriptionCount = " .
                                              showsPrec d
                                                (getField @"vertexAttributeDescriptionCount" x)
                                                .
                                                showString ", " .
                                                  showString "pVertexAttributeDescriptions = " .
                                                    showsPrec d
                                                      (getField @"pVertexAttributeDescriptions" x)
                                                      . showChar '}'
