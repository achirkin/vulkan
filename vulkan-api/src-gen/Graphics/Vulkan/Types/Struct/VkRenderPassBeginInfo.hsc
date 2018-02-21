#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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

instance {-# OVERLAPPING #-} HasVkSType VkRenderPassBeginInfo where
        type VkSTypeMType VkRenderPassBeginInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkRenderPassBeginInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkRenderPassBeginInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, sType}

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

instance CanReadField "sType" VkRenderPassBeginInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkRenderPassBeginInfo where
        type VkPNextMType VkRenderPassBeginInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkRenderPassBeginInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkRenderPassBeginInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, pNext}

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

instance CanReadField "pNext" VkRenderPassBeginInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkRenderPass VkRenderPassBeginInfo
         where
        type VkRenderPassMType VkRenderPassBeginInfo = VkRenderPass

        {-# NOINLINE vkRenderPass #-}
        vkRenderPass x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, renderPass})

        {-# INLINE vkRenderPassByteOffset #-}
        vkRenderPassByteOffset ~_
          = #{offset VkRenderPassBeginInfo, renderPass}

        {-# INLINE readVkRenderPass #-}
        readVkRenderPass p
          = peekByteOff p #{offset VkRenderPassBeginInfo, renderPass}

        {-# INLINE writeVkRenderPass #-}
        writeVkRenderPass p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, renderPass}

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

instance CanReadField "renderPass" VkRenderPassBeginInfo where
        {-# INLINE getField #-}
        getField = vkRenderPass

        {-# INLINE readField #-}
        readField = readVkRenderPass

instance CanWriteField "renderPass" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField = writeVkRenderPass

instance {-# OVERLAPPING #-} HasVkFramebuffer VkRenderPassBeginInfo
         where
        type VkFramebufferMType VkRenderPassBeginInfo = VkFramebuffer

        {-# NOINLINE vkFramebuffer #-}
        vkFramebuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, framebuffer})

        {-# INLINE vkFramebufferByteOffset #-}
        vkFramebufferByteOffset ~_
          = #{offset VkRenderPassBeginInfo, framebuffer}

        {-# INLINE readVkFramebuffer #-}
        readVkFramebuffer p
          = peekByteOff p #{offset VkRenderPassBeginInfo, framebuffer}

        {-# INLINE writeVkFramebuffer #-}
        writeVkFramebuffer p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, framebuffer}

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

instance CanReadField "framebuffer" VkRenderPassBeginInfo where
        {-# INLINE getField #-}
        getField = vkFramebuffer

        {-# INLINE readField #-}
        readField = readVkFramebuffer

instance CanWriteField "framebuffer" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFramebuffer

instance {-# OVERLAPPING #-} HasVkRenderArea VkRenderPassBeginInfo
         where
        type VkRenderAreaMType VkRenderPassBeginInfo = VkRect2D

        {-# NOINLINE vkRenderArea #-}
        vkRenderArea x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, renderArea})

        {-# INLINE vkRenderAreaByteOffset #-}
        vkRenderAreaByteOffset ~_
          = #{offset VkRenderPassBeginInfo, renderArea}

        {-# INLINE readVkRenderArea #-}
        readVkRenderArea p
          = peekByteOff p #{offset VkRenderPassBeginInfo, renderArea}

        {-# INLINE writeVkRenderArea #-}
        writeVkRenderArea p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, renderArea}

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

instance CanReadField "renderArea" VkRenderPassBeginInfo where
        {-# INLINE getField #-}
        getField = vkRenderArea

        {-# INLINE readField #-}
        readField = readVkRenderArea

instance CanWriteField "renderArea" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField = writeVkRenderArea

instance {-# OVERLAPPING #-}
         HasVkClearValueCount VkRenderPassBeginInfo where
        type VkClearValueCountMType VkRenderPassBeginInfo = Word32

        {-# NOINLINE vkClearValueCount #-}
        vkClearValueCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, clearValueCount})

        {-# INLINE vkClearValueCountByteOffset #-}
        vkClearValueCountByteOffset ~_
          = #{offset VkRenderPassBeginInfo, clearValueCount}

        {-# INLINE readVkClearValueCount #-}
        readVkClearValueCount p
          = peekByteOff p #{offset VkRenderPassBeginInfo, clearValueCount}

        {-# INLINE writeVkClearValueCount #-}
        writeVkClearValueCount p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, clearValueCount}

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

instance CanReadField "clearValueCount" VkRenderPassBeginInfo where
        {-# INLINE getField #-}
        getField = vkClearValueCount

        {-# INLINE readField #-}
        readField = readVkClearValueCount

instance CanWriteField "clearValueCount" VkRenderPassBeginInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkClearValueCount

instance {-# OVERLAPPING #-}
         HasVkPClearValues VkRenderPassBeginInfo where
        type VkPClearValuesMType VkRenderPassBeginInfo = Ptr VkClearValue

        {-# NOINLINE vkPClearValues #-}
        vkPClearValues x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassBeginInfo, pClearValues})

        {-# INLINE vkPClearValuesByteOffset #-}
        vkPClearValuesByteOffset ~_
          = #{offset VkRenderPassBeginInfo, pClearValues}

        {-# INLINE readVkPClearValues #-}
        readVkPClearValues p
          = peekByteOff p #{offset VkRenderPassBeginInfo, pClearValues}

        {-# INLINE writeVkPClearValues #-}
        writeVkPClearValues p
          = pokeByteOff p #{offset VkRenderPassBeginInfo, pClearValues}

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

instance CanReadField "pClearValues" VkRenderPassBeginInfo where
        {-# INLINE getField #-}
        getField = vkPClearValues

        {-# INLINE readField #-}
        readField = readVkPClearValues

instance CanWriteField "pClearValues" VkRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPClearValues

instance Show VkRenderPassBeginInfo where
        showsPrec d x
          = showString "VkRenderPassBeginInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkRenderPass = " .
                            showsPrec d (vkRenderPass x) .
                              showString ", " .
                                showString "vkFramebuffer = " .
                                  showsPrec d (vkFramebuffer x) .
                                    showString ", " .
                                      showString "vkRenderArea = " .
                                        showsPrec d (vkRenderArea x) .
                                          showString ", " .
                                            showString "vkClearValueCount = " .
                                              showsPrec d (vkClearValueCount x) .
                                                showString ", " .
                                                  showString "vkPClearValues = " .
                                                    showsPrec d (vkPClearValues x) . showChar '}'
