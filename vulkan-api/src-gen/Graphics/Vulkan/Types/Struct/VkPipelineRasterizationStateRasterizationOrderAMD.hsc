#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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
import           System.IO.Unsafe
                                                                                      (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineRasterizationStateRasterizationOrderAMD {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkRasterizationOrderAMD          rasterizationOrder;
--   > } VkPipelineRasterizationStateRasterizationOrderAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPipelineRasterizationStateRasterizationOrderAMD.html VkPipelineRasterizationStateRasterizationOrderAMD registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

instance {-# OVERLAPPING #-}
         CanWriteField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

instance Show VkPipelineRasterizationStateRasterizationOrderAMD
         where
        showsPrec d x
          = showString "VkPipelineRasterizationStateRasterizationOrderAMD {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "rasterizationOrder = " .
                            showsPrec d (getField @"rasterizationOrder" x) . showChar '}'
