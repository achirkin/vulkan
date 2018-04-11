#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo
       (VkPipelineViewportStateCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkPipelineViewportStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkRect2D      (VkRect2D)
import           Graphics.Vulkan.Types.Struct.VkViewport    (VkViewport)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineViewportStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineViewportStateCreateFlags    flags;
--   >     uint32_t               viewportCount;
--   >     const VkViewport*      pViewports;
--   >     uint32_t               scissorCount;
--   >     const VkRect2D*        pScissors;
--   > } VkPipelineViewportStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineViewportStateCreateInfo VkPipelineViewportStateCreateInfo registry at www.khronos.org>
data VkPipelineViewportStateCreateInfo = VkPipelineViewportStateCreateInfo## Addr##
                                                                            ByteArray##

instance Eq VkPipelineViewportStateCreateInfo where
        (VkPipelineViewportStateCreateInfo## a _) ==
          x@(VkPipelineViewportStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineViewportStateCreateInfo where
        (VkPipelineViewportStateCreateInfo## a _) `compare`
          x@(VkPipelineViewportStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineViewportStateCreateInfo where
        sizeOf ~_ = #{size VkPipelineViewportStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineViewportStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineViewportStateCreateInfo where
        unsafeAddr (VkPipelineViewportStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineViewportStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineViewportStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineViewportStateCreateInfo where
        type StructFields VkPipelineViewportStateCreateInfo =
             '["sType", "pNext", "flags", "viewportCount", "pViewports", -- ' closing tick for hsc2hs
               "scissorCount", "pScissors"]
        type CUnionType VkPipelineViewportStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineViewportStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineViewportStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineViewportStateCreateInfo where
        type FieldType "sType" VkPipelineViewportStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineViewportStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineViewportStateCreateInfo where
        type FieldType "pNext" VkPipelineViewportStateCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineViewportStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineViewportStateCreateInfo where
        type FieldType "flags" VkPipelineViewportStateCreateInfo =
             VkPipelineViewportStateCreateFlags
        type FieldOptional "flags" VkPipelineViewportStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineViewportStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "viewportCount" VkPipelineViewportStateCreateInfo where
        type FieldType "viewportCount" VkPipelineViewportStateCreateInfo =
             Word32
        type FieldOptional "viewportCount"
               VkPipelineViewportStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportCount" VkPipelineViewportStateCreateInfo
             =
             #{offset VkPipelineViewportStateCreateInfo, viewportCount}
        type FieldIsArray "viewportCount" VkPipelineViewportStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, viewportCount}

instance {-# OVERLAPPING #-}
         CanReadField "viewportCount" VkPipelineViewportStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, viewportCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, viewportCount}

instance {-# OVERLAPPING #-}
         CanWriteField "viewportCount" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, viewportCount}

instance {-# OVERLAPPING #-}
         HasField "pViewports" VkPipelineViewportStateCreateInfo where
        type FieldType "pViewports" VkPipelineViewportStateCreateInfo =
             Ptr VkViewport
        type FieldOptional "pViewports" VkPipelineViewportStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pViewports" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, pViewports}
        type FieldIsArray "pViewports" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, pViewports}

instance {-# OVERLAPPING #-}
         CanReadField "pViewports" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, pViewports})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, pViewports}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewports" VkPipelineViewportStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, pViewports}

instance {-# OVERLAPPING #-}
         HasField "scissorCount" VkPipelineViewportStateCreateInfo where
        type FieldType "scissorCount" VkPipelineViewportStateCreateInfo =
             Word32
        type FieldOptional "scissorCount" VkPipelineViewportStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "scissorCount" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, scissorCount}
        type FieldIsArray "scissorCount" VkPipelineViewportStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, scissorCount}

instance {-# OVERLAPPING #-}
         CanReadField "scissorCount" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, scissorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, scissorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "scissorCount" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, scissorCount}

instance {-# OVERLAPPING #-}
         HasField "pScissors" VkPipelineViewportStateCreateInfo where
        type FieldType "pScissors" VkPipelineViewportStateCreateInfo =
             Ptr VkRect2D
        type FieldOptional "pScissors" VkPipelineViewportStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pScissors" VkPipelineViewportStateCreateInfo =
             #{offset VkPipelineViewportStateCreateInfo, pScissors}
        type FieldIsArray "pScissors" VkPipelineViewportStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportStateCreateInfo, pScissors}

instance {-# OVERLAPPING #-}
         CanReadField "pScissors" VkPipelineViewportStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, pScissors})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, pScissors}

instance {-# OVERLAPPING #-}
         CanWriteField "pScissors" VkPipelineViewportStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, pScissors}

instance Show VkPipelineViewportStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineViewportStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "viewportCount = " .
                                  showsPrec d (getField @"viewportCount" x) .
                                    showString ", " .
                                      showString "pViewports = " .
                                        showsPrec d (getField @"pViewports" x) .
                                          showString ", " .
                                            showString "scissorCount = " .
                                              showsPrec d (getField @"scissorCount" x) .
                                                showString ", " .
                                                  showString "pScissors = " .
                                                    showsPrec d (getField @"pScissors" x) .
                                                      showChar '}'
