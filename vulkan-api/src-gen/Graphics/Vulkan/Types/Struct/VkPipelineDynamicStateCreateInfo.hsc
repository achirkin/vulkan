#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineDynamicStateCreateInfo
       (VkPipelineDynamicStateCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkPipelineDynamicStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkDynamicState  (VkDynamicState)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineDynamicStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineDynamicStateCreateFlags    flags;
--   >     uint32_t               dynamicStateCount;
--   >     const VkDynamicState*  pDynamicStates;
--   > } VkPipelineDynamicStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineDynamicStateCreateInfo.html VkPipelineDynamicStateCreateInfo registry at www.khronos.org>
data VkPipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo## Addr##
                                                                          ByteArray##

instance Eq VkPipelineDynamicStateCreateInfo where
        (VkPipelineDynamicStateCreateInfo## a _) ==
          x@(VkPipelineDynamicStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineDynamicStateCreateInfo where
        (VkPipelineDynamicStateCreateInfo## a _) `compare`
          x@(VkPipelineDynamicStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineDynamicStateCreateInfo where
        sizeOf ~_ = #{size VkPipelineDynamicStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineDynamicStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineDynamicStateCreateInfo where
        unsafeAddr (VkPipelineDynamicStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineDynamicStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineDynamicStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineDynamicStateCreateInfo where
        type StructFields VkPipelineDynamicStateCreateInfo =
             '["sType", "pNext", "flags", "dynamicStateCount", "pDynamicStates"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineDynamicStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineDynamicStateCreateInfo where
        type VkSTypeMType VkPipelineDynamicStateCreateInfo =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineDynamicStateCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineDynamicStateCreateInfo where
        type FieldType "sType" VkPipelineDynamicStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineDynamicStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineDynamicStateCreateInfo =
             #{offset VkPipelineDynamicStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, sType}

instance CanReadField "sType" VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineDynamicStateCreateInfo where
        type VkPNextMType VkPipelineDynamicStateCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineDynamicStateCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineDynamicStateCreateInfo where
        type FieldType "pNext" VkPipelineDynamicStateCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineDynamicStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineDynamicStateCreateInfo =
             #{offset VkPipelineDynamicStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, pNext}

instance CanReadField "pNext" VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineDynamicStateCreateInfo where
        type VkFlagsMType VkPipelineDynamicStateCreateInfo =
             VkPipelineDynamicStateCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineDynamicStateCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineDynamicStateCreateInfo where
        type FieldType "flags" VkPipelineDynamicStateCreateInfo =
             VkPipelineDynamicStateCreateFlags
        type FieldOptional "flags" VkPipelineDynamicStateCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineDynamicStateCreateInfo =
             #{offset VkPipelineDynamicStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineDynamicStateCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, flags}

instance CanReadField "flags" VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkDynamicStateCount VkPipelineDynamicStateCreateInfo where
        type VkDynamicStateCountMType VkPipelineDynamicStateCreateInfo =
             Word32

        {-# NOINLINE vkDynamicStateCount #-}
        vkDynamicStateCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount})

        {-# INLINE vkDynamicStateCountByteOffset #-}
        vkDynamicStateCountByteOffset ~_
          = #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}

        {-# INLINE readVkDynamicStateCount #-}
        readVkDynamicStateCount p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}

        {-# INLINE writeVkDynamicStateCount #-}
        writeVkDynamicStateCount p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}

instance {-# OVERLAPPING #-}
         HasField "dynamicStateCount" VkPipelineDynamicStateCreateInfo where
        type FieldType "dynamicStateCount" VkPipelineDynamicStateCreateInfo
             = Word32
        type FieldOptional "dynamicStateCount"
               VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dynamicStateCount"
               VkPipelineDynamicStateCreateInfo
             =
             #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}
        type FieldIsArray "dynamicStateCount"
               VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, dynamicStateCount}

instance CanReadField "dynamicStateCount"
           VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDynamicStateCount

        {-# INLINE readField #-}
        readField = readVkDynamicStateCount

instance CanWriteField "dynamicStateCount"
           VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDynamicStateCount

instance {-# OVERLAPPING #-}
         HasVkPDynamicStates VkPipelineDynamicStateCreateInfo where
        type VkPDynamicStatesMType VkPipelineDynamicStateCreateInfo =
             Ptr VkDynamicState

        {-# NOINLINE vkPDynamicStates #-}
        vkPDynamicStates x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates})

        {-# INLINE vkPDynamicStatesByteOffset #-}
        vkPDynamicStatesByteOffset ~_
          = #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}

        {-# INLINE readVkPDynamicStates #-}
        readVkPDynamicStates p
          = peekByteOff p #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}

        {-# INLINE writeVkPDynamicStates #-}
        writeVkPDynamicStates p
          = pokeByteOff p #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}

instance {-# OVERLAPPING #-}
         HasField "pDynamicStates" VkPipelineDynamicStateCreateInfo where
        type FieldType "pDynamicStates" VkPipelineDynamicStateCreateInfo =
             Ptr VkDynamicState
        type FieldOptional "pDynamicStates"
               VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDynamicStates" VkPipelineDynamicStateCreateInfo
             =
             #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}
        type FieldIsArray "pDynamicStates" VkPipelineDynamicStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDynamicStateCreateInfo, pDynamicStates}

instance CanReadField "pDynamicStates"
           VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPDynamicStates

        {-# INLINE readField #-}
        readField = readVkPDynamicStates

instance CanWriteField "pDynamicStates"
           VkPipelineDynamicStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPDynamicStates

instance Show VkPipelineDynamicStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineDynamicStateCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDynamicStateCount = " .
                                  showsPrec d (vkDynamicStateCount x) .
                                    showString ", " .
                                      showString "vkPDynamicStates = " .
                                        showsPrec d (vkPDynamicStates x) . showChar '}'
