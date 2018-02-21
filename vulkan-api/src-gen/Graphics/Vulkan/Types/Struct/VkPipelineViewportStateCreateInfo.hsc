#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo
       (VkPipelineViewportStateCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkPipelineViewportStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkRect2D      (VkRect2D)
import           Graphics.Vulkan.Types.Struct.VkViewport    (VkViewport)
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineViewportStateCreateInfo.html VkPipelineViewportStateCreateInfo registry at www.khronos.org>
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
         HasVkSType VkPipelineViewportStateCreateInfo where
        type VkSTypeMType VkPipelineViewportStateCreateInfo =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineViewportStateCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, sType}

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

instance CanReadField "sType" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineViewportStateCreateInfo where
        type VkPNextMType VkPipelineViewportStateCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineViewportStateCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, pNext}

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

instance CanReadField "pNext" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineViewportStateCreateInfo where
        type VkFlagsMType VkPipelineViewportStateCreateInfo =
             VkPipelineViewportStateCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineViewportStateCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, flags}

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

instance CanReadField "flags" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkViewportCount VkPipelineViewportStateCreateInfo where
        type VkViewportCountMType VkPipelineViewportStateCreateInfo =
             Word32

        {-# NOINLINE vkViewportCount #-}
        vkViewportCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, viewportCount})

        {-# INLINE vkViewportCountByteOffset #-}
        vkViewportCountByteOffset ~_
          = #{offset VkPipelineViewportStateCreateInfo, viewportCount}

        {-# INLINE readVkViewportCount #-}
        readVkViewportCount p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, viewportCount}

        {-# INLINE writeVkViewportCount #-}
        writeVkViewportCount p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, viewportCount}

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

instance CanReadField "viewportCount"
           VkPipelineViewportStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkViewportCount

        {-# INLINE readField #-}
        readField = readVkViewportCount

instance CanWriteField "viewportCount"
           VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkViewportCount

instance {-# OVERLAPPING #-}
         HasVkPViewports VkPipelineViewportStateCreateInfo where
        type VkPViewportsMType VkPipelineViewportStateCreateInfo =
             Ptr VkViewport

        {-# NOINLINE vkPViewports #-}
        vkPViewports x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, pViewports})

        {-# INLINE vkPViewportsByteOffset #-}
        vkPViewportsByteOffset ~_
          = #{offset VkPipelineViewportStateCreateInfo, pViewports}

        {-# INLINE readVkPViewports #-}
        readVkPViewports p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, pViewports}

        {-# INLINE writeVkPViewports #-}
        writeVkPViewports p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, pViewports}

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

instance CanReadField "pViewports"
           VkPipelineViewportStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPViewports

        {-# INLINE readField #-}
        readField = readVkPViewports

instance CanWriteField "pViewports"
           VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPViewports

instance {-# OVERLAPPING #-}
         HasVkScissorCount VkPipelineViewportStateCreateInfo where
        type VkScissorCountMType VkPipelineViewportStateCreateInfo = Word32

        {-# NOINLINE vkScissorCount #-}
        vkScissorCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, scissorCount})

        {-# INLINE vkScissorCountByteOffset #-}
        vkScissorCountByteOffset ~_
          = #{offset VkPipelineViewportStateCreateInfo, scissorCount}

        {-# INLINE readVkScissorCount #-}
        readVkScissorCount p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, scissorCount}

        {-# INLINE writeVkScissorCount #-}
        writeVkScissorCount p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, scissorCount}

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

instance CanReadField "scissorCount"
           VkPipelineViewportStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkScissorCount

        {-# INLINE readField #-}
        readField = readVkScissorCount

instance CanWriteField "scissorCount"
           VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkScissorCount

instance {-# OVERLAPPING #-}
         HasVkPScissors VkPipelineViewportStateCreateInfo where
        type VkPScissorsMType VkPipelineViewportStateCreateInfo =
             Ptr VkRect2D

        {-# NOINLINE vkPScissors #-}
        vkPScissors x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportStateCreateInfo, pScissors})

        {-# INLINE vkPScissorsByteOffset #-}
        vkPScissorsByteOffset ~_
          = #{offset VkPipelineViewportStateCreateInfo, pScissors}

        {-# INLINE readVkPScissors #-}
        readVkPScissors p
          = peekByteOff p #{offset VkPipelineViewportStateCreateInfo, pScissors}

        {-# INLINE writeVkPScissors #-}
        writeVkPScissors p
          = pokeByteOff p #{offset VkPipelineViewportStateCreateInfo, pScissors}

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

instance CanReadField "pScissors" VkPipelineViewportStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPScissors

        {-# INLINE readField #-}
        readField = readVkPScissors

instance CanWriteField "pScissors"
           VkPipelineViewportStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPScissors

instance Show VkPipelineViewportStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineViewportStateCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkViewportCount = " .
                                  showsPrec d (vkViewportCount x) .
                                    showString ", " .
                                      showString "vkPViewports = " .
                                        showsPrec d (vkPViewports x) .
                                          showString ", " .
                                            showString "vkScissorCount = " .
                                              showsPrec d (vkScissorCount x) .
                                                showString ", " .
                                                  showString "vkPScissors = " .
                                                    showsPrec d (vkPScissors x) . showChar '}'
