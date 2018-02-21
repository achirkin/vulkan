#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkPipelineColorBlendStateCreateInfo
       (VkPipelineColorBlendStateCreateInfo(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Prim
import           GHC.TypeLits
                                                                                   (KnownNat,
                                                                                   natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes
                                                                                   (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks
                                                                                   (VkPipelineColorBlendStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkLogicOp
                                                                                   (VkLogicOp)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineColorBlendAttachmentState
                                                                                   (VkPipelineColorBlendAttachmentState)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineColorBlendStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineColorBlendStateCreateFlags    flags;
--   >     VkBool32               logicOpEnable;
--   >     VkLogicOp              logicOp;
--   >     uint32_t               attachmentCount;
--   >     const VkPipelineColorBlendAttachmentState* pAttachments;
--   >     float                  blendConstants[4];
--   > } VkPipelineColorBlendStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineColorBlendStateCreateInfo.html VkPipelineColorBlendStateCreateInfo registry at www.khronos.org>
data VkPipelineColorBlendStateCreateInfo = VkPipelineColorBlendStateCreateInfo## Addr##
                                                                                ByteArray##

instance Eq VkPipelineColorBlendStateCreateInfo where
        (VkPipelineColorBlendStateCreateInfo## a _) ==
          x@(VkPipelineColorBlendStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineColorBlendStateCreateInfo where
        (VkPipelineColorBlendStateCreateInfo## a _) `compare`
          x@(VkPipelineColorBlendStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineColorBlendStateCreateInfo where
        sizeOf ~_ = #{size VkPipelineColorBlendStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineColorBlendStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineColorBlendStateCreateInfo
         where
        unsafeAddr (VkPipelineColorBlendStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineColorBlendStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineColorBlendStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineColorBlendStateCreateInfo where
        type StructFields VkPipelineColorBlendStateCreateInfo =
             '["sType", "pNext", "flags", "logicOpEnable", "logicOp", -- ' closing tick for hsc2hs
               "attachmentCount", "pAttachments", "blendConstants"]
        type CUnionType VkPipelineColorBlendStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineColorBlendStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineColorBlendStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineColorBlendStateCreateInfo where
        type VkSTypeMType VkPipelineColorBlendStateCreateInfo =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineColorBlendStateCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineColorBlendStateCreateInfo where
        type FieldType "sType" VkPipelineColorBlendStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineColorBlendStateCreateInfo =
             #{offset VkPipelineColorBlendStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, sType}

instance CanReadField "sType" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineColorBlendStateCreateInfo where
        type VkPNextMType VkPipelineColorBlendStateCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineColorBlendStateCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineColorBlendStateCreateInfo where
        type FieldType "pNext" VkPipelineColorBlendStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineColorBlendStateCreateInfo =
             #{offset VkPipelineColorBlendStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, pNext}

instance CanReadField "pNext" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineColorBlendStateCreateInfo where
        type VkFlagsMType VkPipelineColorBlendStateCreateInfo =
             VkPipelineColorBlendStateCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineColorBlendStateCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineColorBlendStateCreateInfo where
        type FieldType "flags" VkPipelineColorBlendStateCreateInfo =
             VkPipelineColorBlendStateCreateFlags
        type FieldOptional "flags" VkPipelineColorBlendStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineColorBlendStateCreateInfo =
             #{offset VkPipelineColorBlendStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, flags}

instance CanReadField "flags" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkLogicOpEnable VkPipelineColorBlendStateCreateInfo where
        type VkLogicOpEnableMType VkPipelineColorBlendStateCreateInfo =
             VkBool32

        {-# NOINLINE vkLogicOpEnable #-}
        vkLogicOpEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable})

        {-# INLINE vkLogicOpEnableByteOffset #-}
        vkLogicOpEnableByteOffset ~_
          = #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}

        {-# INLINE readVkLogicOpEnable #-}
        readVkLogicOpEnable p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}

        {-# INLINE writeVkLogicOpEnable #-}
        writeVkLogicOpEnable p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}

instance {-# OVERLAPPING #-}
         HasField "logicOpEnable" VkPipelineColorBlendStateCreateInfo where
        type FieldType "logicOpEnable" VkPipelineColorBlendStateCreateInfo
             = VkBool32
        type FieldOptional "logicOpEnable"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "logicOpEnable"
               VkPipelineColorBlendStateCreateInfo
             =
             #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}
        type FieldIsArray "logicOpEnable"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}

instance CanReadField "logicOpEnable"
           VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkLogicOpEnable

        {-# INLINE readField #-}
        readField = readVkLogicOpEnable

instance CanWriteField "logicOpEnable"
           VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkLogicOpEnable

instance {-# OVERLAPPING #-}
         HasVkLogicOp VkPipelineColorBlendStateCreateInfo where
        type VkLogicOpMType VkPipelineColorBlendStateCreateInfo = VkLogicOp

        {-# NOINLINE vkLogicOp #-}
        vkLogicOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, logicOp})

        {-# INLINE vkLogicOpByteOffset #-}
        vkLogicOpByteOffset ~_
          = #{offset VkPipelineColorBlendStateCreateInfo, logicOp}

        {-# INLINE readVkLogicOp #-}
        readVkLogicOp p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOp}

        {-# INLINE writeVkLogicOp #-}
        writeVkLogicOp p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOp}

instance {-# OVERLAPPING #-}
         HasField "logicOp" VkPipelineColorBlendStateCreateInfo where
        type FieldType "logicOp" VkPipelineColorBlendStateCreateInfo =
             VkLogicOp
        type FieldOptional "logicOp" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "logicOp" VkPipelineColorBlendStateCreateInfo =
             #{offset VkPipelineColorBlendStateCreateInfo, logicOp}
        type FieldIsArray "logicOp" VkPipelineColorBlendStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, logicOp}

instance CanReadField "logicOp" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkLogicOp

        {-# INLINE readField #-}
        readField = readVkLogicOp

instance CanWriteField "logicOp"
           VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkLogicOp

instance {-# OVERLAPPING #-}
         HasVkAttachmentCount VkPipelineColorBlendStateCreateInfo where
        type VkAttachmentCountMType VkPipelineColorBlendStateCreateInfo =
             Word32

        {-# NOINLINE vkAttachmentCount #-}
        vkAttachmentCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount})

        {-# INLINE vkAttachmentCountByteOffset #-}
        vkAttachmentCountByteOffset ~_
          = #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}

        {-# INLINE readVkAttachmentCount #-}
        readVkAttachmentCount p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}

        {-# INLINE writeVkAttachmentCount #-}
        writeVkAttachmentCount p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         HasField "attachmentCount" VkPipelineColorBlendStateCreateInfo
         where
        type FieldType "attachmentCount"
               VkPipelineColorBlendStateCreateInfo
             = Word32
        type FieldOptional "attachmentCount"
               VkPipelineColorBlendStateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "attachmentCount"
               VkPipelineColorBlendStateCreateInfo
             =
             #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}
        type FieldIsArray "attachmentCount"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}

instance CanReadField "attachmentCount"
           VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkAttachmentCount

        {-# INLINE readField #-}
        readField = readVkAttachmentCount

instance CanWriteField "attachmentCount"
           VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkAttachmentCount

instance {-# OVERLAPPING #-}
         HasVkPAttachments VkPipelineColorBlendStateCreateInfo where
        type VkPAttachmentsMType VkPipelineColorBlendStateCreateInfo =
             Ptr VkPipelineColorBlendAttachmentState

        {-# NOINLINE vkPAttachments #-}
        vkPAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, pAttachments})

        {-# INLINE vkPAttachmentsByteOffset #-}
        vkPAttachmentsByteOffset ~_
          = #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}

        {-# INLINE readVkPAttachments #-}
        readVkPAttachments p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}

        {-# INLINE writeVkPAttachments #-}
        writeVkPAttachments p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         HasField "pAttachments" VkPipelineColorBlendStateCreateInfo where
        type FieldType "pAttachments" VkPipelineColorBlendStateCreateInfo =
             Ptr VkPipelineColorBlendAttachmentState
        type FieldOptional "pAttachments"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAttachments" VkPipelineColorBlendStateCreateInfo
             =
             #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}
        type FieldIsArray "pAttachments"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}

instance CanReadField "pAttachments"
           VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPAttachments

        {-# INLINE readField #-}
        readField = readVkPAttachments

instance CanWriteField "pAttachments"
           VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPAttachments

instance {-# OVERLAPPING #-}
         HasVkBlendConstantsArray VkPipelineColorBlendStateCreateInfo where
        type VkBlendConstantsArrayMType VkPipelineColorBlendStateCreateInfo
             = #{type float}

        {-# NOINLINE vkBlendConstantsArray #-}
        vkBlendConstantsArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: #{type float}) +
                    #{offset VkPipelineColorBlendStateCreateInfo, blendConstants}))

        {-# INLINE vkBlendConstantsArrayByteOffset #-}
        vkBlendConstantsArrayByteOffset ~_
          = #{offset VkPipelineColorBlendStateCreateInfo, blendConstants}

        {-# INLINE readVkBlendConstantsArray #-}
        readVkBlendConstantsArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPipelineColorBlendStateCreateInfo, blendConstants})

        {-# INLINE writeVkBlendConstantsArray #-}
        writeVkBlendConstantsArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: #{type float}) +
                 #{offset VkPipelineColorBlendStateCreateInfo, blendConstants})

instance {-# OVERLAPPING #-}
         HasField "blendConstants" VkPipelineColorBlendStateCreateInfo where
        type FieldType "blendConstants" VkPipelineColorBlendStateCreateInfo
             = #{type float}
        type FieldOptional "blendConstants"
               VkPipelineColorBlendStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "blendConstants"
               VkPipelineColorBlendStateCreateInfo
             =
             #{offset VkPipelineColorBlendStateCreateInfo, blendConstants}
        type FieldIsArray "blendConstants"
               VkPipelineColorBlendStateCreateInfo
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendStateCreateInfo, blendConstants}

instance (KnownNat idx,
          IndexInBounds "blendConstants" idx
            VkPipelineColorBlendStateCreateInfo) =>
         CanReadFieldArray "blendConstants" idx
           VkPipelineColorBlendStateCreateInfo
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "blendConstants" 0
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "blendConstants" 1
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "blendConstants" 2
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "blendConstants" 3
                         VkPipelineColorBlendStateCreateInfo
                       #-}
        type FieldArrayLength "blendConstants"
               VkPipelineColorBlendStateCreateInfo
             = 4

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 4

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkBlendConstantsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkBlendConstantsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance (KnownNat idx,
          IndexInBounds "blendConstants" idx
            VkPipelineColorBlendStateCreateInfo) =>
         CanWriteFieldArray "blendConstants" idx
           VkPipelineColorBlendStateCreateInfo
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "blendConstants" 0
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "blendConstants" 1
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "blendConstants" 2
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "blendConstants" 3
                         VkPipelineColorBlendStateCreateInfo
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray x
          = writeVkBlendConstantsArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance Show VkPipelineColorBlendStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineColorBlendStateCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkLogicOpEnable = " .
                                  showsPrec d (vkLogicOpEnable x) .
                                    showString ", " .
                                      showString "vkLogicOp = " .
                                        showsPrec d (vkLogicOp x) .
                                          showString ", " .
                                            showString "vkAttachmentCount = " .
                                              showsPrec d (vkAttachmentCount x) .
                                                showString ", " .
                                                  showString "vkPAttachments = " .
                                                    showsPrec d (vkPAttachments x) .
                                                      showString ", " .
                                                        showString "vkBlendConstantsArray = [" .
                                                          showsPrec d
                                                            (map (vkBlendConstantsArray x) [1 .. 4])
                                                            . showChar ']' . showChar '}'
