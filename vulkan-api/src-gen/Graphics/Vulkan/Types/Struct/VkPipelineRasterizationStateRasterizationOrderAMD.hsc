#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateRasterizationOrderAMD
       (VkPipelineRasterizationStateRasterizationOrderAMD(..)) where
import           Foreign.Storable
                                                                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkRasterizationOrderAMD
                                                                                      (VkRasterizationOrderAMD)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                      (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo
                                                                                      (VkPipelineRasterizationStateCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                      (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineRasterizationStateRasterizationOrderAMD {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkRasterizationOrderAMD          rasterizationOrder;
--   > } VkPipelineRasterizationStateRasterizationOrderAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineRasterizationStateRasterizationOrderAMD.html VkPipelineRasterizationStateRasterizationOrderAMD registry at www.khronos.org>
data VkPipelineRasterizationStateRasterizationOrderAMD = VkPipelineRasterizationStateRasterizationOrderAMD## Addr##
                                                                                                            ByteArray##

instance Eq VkPipelineRasterizationStateRasterizationOrderAMD where
        (VkPipelineRasterizationStateRasterizationOrderAMD## a _) ==
          x@(VkPipelineRasterizationStateRasterizationOrderAMD## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineRasterizationStateRasterizationOrderAMD
         where
        (VkPipelineRasterizationStateRasterizationOrderAMD## a _) `compare`
          x@(VkPipelineRasterizationStateRasterizationOrderAMD## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineRasterizationStateRasterizationOrderAMD
         where
        sizeOf ~_
          = #{size VkPipelineRasterizationStateRasterizationOrderAMD}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineRasterizationStateRasterizationOrderAMD}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        unsafeAddr (VkPipelineRasterizationStateRasterizationOrderAMD## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineRasterizationStateRasterizationOrderAMD## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineRasterizationStateRasterizationOrderAMD##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type StructFields VkPipelineRasterizationStateRasterizationOrderAMD
             = '["sType", "pNext", "rasterizationOrder"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineRasterizationStateRasterizationOrderAMD =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPipelineRasterizationStateRasterizationOrderAMD
             = '[VkPipelineRasterizationStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineRasterizationStateRasterizationOrderAMD where
        type VkSTypeMType VkPipelineRasterizationStateRasterizationOrderAMD
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type FieldType "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             =
             #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}
        type FieldIsArray "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

instance CanReadField "sType"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineRasterizationStateRasterizationOrderAMD where
        type VkPNextMType VkPipelineRasterizationStateRasterizationOrderAMD
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type FieldType "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             =
             #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}
        type FieldIsArray "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

instance CanReadField "pNext"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkRasterizationOrder
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type VkRasterizationOrderMType
               VkPipelineRasterizationStateRasterizationOrderAMD
             = VkRasterizationOrderAMD

        {-# NOINLINE vkRasterizationOrder #-}
        vkRasterizationOrder x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder})

        {-# INLINE vkRasterizationOrderByteOffset #-}
        vkRasterizationOrderByteOffset ~_
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

        {-# INLINE readVkRasterizationOrder #-}
        readVkRasterizationOrder p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

        {-# INLINE writeVkRasterizationOrder #-}
        writeVkRasterizationOrder p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

instance {-# OVERLAPPING #-}
         HasField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type FieldType "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = VkRasterizationOrderAMD
        type FieldOptional "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             =
             #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}
        type FieldIsArray "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

instance CanReadField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE getField #-}
        getField = vkRasterizationOrder

        {-# INLINE readField #-}
        readField = readVkRasterizationOrder

instance CanWriteField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkRasterizationOrder

instance Show VkPipelineRasterizationStateRasterizationOrderAMD
         where
        showsPrec d x
          = showString "VkPipelineRasterizationStateRasterizationOrderAMD {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkRasterizationOrder = " .
                            showsPrec d (vkRasterizationOrder x) . showChar '}'
