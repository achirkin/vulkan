#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkPipelineColorBlendStateCreateInfo
       (VkPipelineColorBlendStateCreateInfo(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Base
                                                                                   (Addr##,
                                                                                   ByteArray##,
                                                                                   Proxy##,
                                                                                   byteArrayContents##,
                                                                                   plusAddr##,
                                                                                   proxy##)
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineColorBlendStateCreateInfo VkPipelineColorBlendStateCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineColorBlendStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineColorBlendStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineColorBlendStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineColorBlendStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineColorBlendStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineColorBlendStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "logicOpEnable" VkPipelineColorBlendStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "logicOpEnable" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOpEnable}

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

instance {-# OVERLAPPING #-}
         CanReadField "logicOp" VkPipelineColorBlendStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, logicOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOp}

instance {-# OVERLAPPING #-}
         CanWriteField "logicOp" VkPipelineColorBlendStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, logicOp}

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

instance {-# OVERLAPPING #-}
         CanReadField "attachmentCount" VkPipelineColorBlendStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         CanWriteField "attachmentCount" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, attachmentCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pAttachments" VkPipelineColorBlendStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendStateCreateInfo, pAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttachments" VkPipelineColorBlendStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendStateCreateInfo, pAttachments}

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkPipelineColorBlendStateCreateInfo, blendConstants}
                      +
                      sizeOf (undefined :: #{type float}) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkPipelineColorBlendStateCreateInfo, blendConstants}
                 +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkPipelineColorBlendStateCreateInfo, blendConstants}
                 +
                 sizeOf (undefined :: #{type float}) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance Show VkPipelineColorBlendStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineColorBlendStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "logicOpEnable = " .
                                  showsPrec d (getField @"logicOpEnable" x) .
                                    showString ", " .
                                      showString "logicOp = " .
                                        showsPrec d (getField @"logicOp" x) .
                                          showString ", " .
                                            showString "attachmentCount = " .
                                              showsPrec d (getField @"attachmentCount" x) .
                                                showString ", " .
                                                  showString "pAttachments = " .
                                                    showsPrec d (getField @"pAttachments" x) .
                                                      showString ", " .
                                                        (showString "blendConstants = [" .
                                                           showsPrec d
                                                             (let s = sizeOf
                                                                        (undefined ::
                                                                           FieldType
                                                                             "blendConstants"
                                                                             VkPipelineColorBlendStateCreateInfo)
                                                                  o = fieldOffset @"blendConstants"
                                                                        @VkPipelineColorBlendStateCreateInfo
                                                                  f i
                                                                    = peekByteOff (unsafePtr x) i ::
                                                                        IO
                                                                          (FieldType
                                                                             "blendConstants"
                                                                             VkPipelineColorBlendStateCreateInfo)
                                                                in
                                                                unsafeDupablePerformIO . mapM f $
                                                                  map (\ i -> o + i * s)
                                                                    [0 .. 4 - 1])
                                                             . showChar ']')
                                                          . showChar '}'
