#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo
       (VkRenderPassBeginInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkFramebuffer,
                                                             VkRenderPass)
import           Graphics.Vulkan.Types.Struct.VkClearValue  (VkClearValue)
import           Graphics.Vulkan.Types.Struct.VkRect2D      (VkRect2D)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkRenderPassBeginInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRenderPass           renderPass;
--   >     VkFramebuffer          framebuffer;
--   >     VkRect2D               renderArea;
--   >     uint32_t               clearValueCount;
--   >     const VkClearValue*    pClearValues;
--   > } VkRenderPassBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkRenderPassBeginInfo.html VkRenderPassBeginInfo registry at www.khronos.org>
data VkRenderPassBeginInfo = VkRenderPassBeginInfo## Addr##
                                                    ByteArray##

instance Eq VkRenderPassBeginInfo where
        (VkRenderPassBeginInfo## a _) == x@(VkRenderPassBeginInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassBeginInfo where
        (VkRenderPassBeginInfo## a _) `compare`
          x@(VkRenderPassBeginInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassBeginInfo where
        sizeOf ~_ = #{size VkRenderPassBeginInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkRenderPassBeginInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassBeginInfo where
        unsafeAddr (VkRenderPassBeginInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassBeginInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassBeginInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassBeginInfo where
        type StructFields VkRenderPassBeginInfo =
             '["sType", "pNext", "renderPass", "framebuffer", "renderArea", -- ' closing tick for hsc2hs
               "clearValueCount", "pClearValues"]
        type CUnionType VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassBeginInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkRenderPassBeginInfo
         where
        type FieldType "sType" VkRenderPassBeginInfo = VkStructureType
        type FieldOptional "sType" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, sType}
        type FieldIsArray "sType" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkRenderPassBeginInfo
         where
        type FieldType "pNext" VkRenderPassBeginInfo = Ptr Void
        type FieldOptional "pNext" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, pNext}
        type FieldIsArray "pNext" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "renderPass" VkRenderPassBeginInfo where
        type FieldType "renderPass" VkRenderPassBeginInfo = VkRenderPass
        type FieldOptional "renderPass" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "renderPass" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, renderPass}
        type FieldIsArray "renderPass" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassBeginInfo, renderPass}

instance {-# OVERLAPPING #-}
         CanReadField "renderPass" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, renderPass})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, renderPass}

instance {-# OVERLAPPING #-}
         CanWriteField "renderPass" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, renderPass}

instance {-# OVERLAPPING #-}
         HasField "framebuffer" VkRenderPassBeginInfo where
        type FieldType "framebuffer" VkRenderPassBeginInfo = VkFramebuffer
        type FieldOptional "framebuffer" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "framebuffer" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, framebuffer}
        type FieldIsArray "framebuffer" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassBeginInfo, framebuffer}

instance {-# OVERLAPPING #-}
         CanReadField "framebuffer" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, framebuffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, framebuffer}

instance {-# OVERLAPPING #-}
         CanWriteField "framebuffer" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, framebuffer}

instance {-# OVERLAPPING #-}
         HasField "renderArea" VkRenderPassBeginInfo where
        type FieldType "renderArea" VkRenderPassBeginInfo = VkRect2D
        type FieldOptional "renderArea" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "renderArea" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, renderArea}
        type FieldIsArray "renderArea" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassBeginInfo, renderArea}

instance {-# OVERLAPPING #-}
         CanReadField "renderArea" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, renderArea})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, renderArea}

instance {-# OVERLAPPING #-}
         CanWriteField "renderArea" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, renderArea}

instance {-# OVERLAPPING #-}
         HasField "clearValueCount" VkRenderPassBeginInfo where
        type FieldType "clearValueCount" VkRenderPassBeginInfo = Word32
        type FieldOptional "clearValueCount" VkRenderPassBeginInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "clearValueCount" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, clearValueCount}
        type FieldIsArray "clearValueCount" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassBeginInfo, clearValueCount}

instance {-# OVERLAPPING #-}
         CanReadField "clearValueCount" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, clearValueCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, clearValueCount}

instance {-# OVERLAPPING #-}
         CanWriteField "clearValueCount" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, clearValueCount}

instance {-# OVERLAPPING #-}
         HasField "pClearValues" VkRenderPassBeginInfo where
        type FieldType "pClearValues" VkRenderPassBeginInfo =
             Ptr VkClearValue
        type FieldOptional "pClearValues" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pClearValues" VkRenderPassBeginInfo =
             #{offset VkRenderPassBeginInfo, pClearValues}
        type FieldIsArray "pClearValues" VkRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassBeginInfo, pClearValues}

instance {-# OVERLAPPING #-}
         CanReadField "pClearValues" VkRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, pClearValues})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassBeginInfo, pClearValues}

instance {-# OVERLAPPING #-}
         CanWriteField "pClearValues" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, pClearValues}

instance Show VkRenderPassBeginInfo where
        showsPrec d x
          = showString "VkRenderPassBeginInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "renderPass = " .
                            showsPrec d (getField @"renderPass" x) .
                              showString ", " .
                                showString "framebuffer = " .
                                  showsPrec d (getField @"framebuffer" x) .
                                    showString ", " .
                                      showString "renderArea = " .
                                        showsPrec d (getField @"renderArea" x) .
                                          showString ", " .
                                            showString "clearValueCount = " .
                                              showsPrec d (getField @"clearValueCount" x) .
                                                showString ", " .
                                                  showString "pClearValues = " .
                                                    showsPrec d (getField @"pClearValues" x) .
                                                      showChar '}'
