#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRenderPassMultiviewCreateInfo
       (VkRenderPassMultiviewCreateInfo(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Base                                            (Addr##,
                                                                      ByteArray##,
                                                                      byteArrayContents##,
                                                                      plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType          (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo (VkRenderPassCreateInfo)
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkRenderPassMultiviewCreateInfo {
--   >     VkStructureType        sType;
--   >     const void*            pNext;
--   >     uint32_t               subpassCount;
--   >     const uint32_t*     pViewMasks;
--   >     uint32_t               dependencyCount;
--   >     const int32_t*   pViewOffsets;
--   >     uint32_t               correlationMaskCount;
--   >     const uint32_t* pCorrelationMasks;
--   > } VkRenderPassMultiviewCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkRenderPassMultiviewCreateInfo VkRenderPassMultiviewCreateInfo registry at www.khronos.org>
data VkRenderPassMultiviewCreateInfo = VkRenderPassMultiviewCreateInfo## Addr##
                                                                        ByteArray##

instance Eq VkRenderPassMultiviewCreateInfo where
        (VkRenderPassMultiviewCreateInfo## a _) ==
          x@(VkRenderPassMultiviewCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassMultiviewCreateInfo where
        (VkRenderPassMultiviewCreateInfo## a _) `compare`
          x@(VkRenderPassMultiviewCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassMultiviewCreateInfo where
        sizeOf ~_ = #{size VkRenderPassMultiviewCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassMultiviewCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassMultiviewCreateInfo where
        unsafeAddr (VkRenderPassMultiviewCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassMultiviewCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassMultiviewCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassMultiviewCreateInfo where
        type StructFields VkRenderPassMultiviewCreateInfo =
             '["sType", "pNext", "subpassCount", "pViewMasks", -- ' closing tick for hsc2hs
               "dependencyCount", "pViewOffsets", "correlationMaskCount",
               "pCorrelationMasks"]
        type CUnionType VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassMultiviewCreateInfo =
             '[VkRenderPassCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassMultiviewCreateInfo where
        type FieldType "sType" VkRenderPassMultiviewCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassMultiviewCreateInfo =
             #{offset VkRenderPassMultiviewCreateInfo, sType}
        type FieldIsArray "sType" VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassMultiviewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkRenderPassMultiviewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassMultiviewCreateInfo where
        type FieldType "pNext" VkRenderPassMultiviewCreateInfo = Ptr Void
        type FieldOptional "pNext" VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassMultiviewCreateInfo =
             #{offset VkRenderPassMultiviewCreateInfo, pNext}
        type FieldIsArray "pNext" VkRenderPassMultiviewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassMultiviewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkRenderPassMultiviewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "subpassCount" VkRenderPassMultiviewCreateInfo where
        type FieldType "subpassCount" VkRenderPassMultiviewCreateInfo =
             Word32
        type FieldOptional "subpassCount" VkRenderPassMultiviewCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "subpassCount" VkRenderPassMultiviewCreateInfo =
             #{offset VkRenderPassMultiviewCreateInfo, subpassCount}
        type FieldIsArray "subpassCount" VkRenderPassMultiviewCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         CanReadField "subpassCount" VkRenderPassMultiviewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, subpassCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         CanWriteField "subpassCount" VkRenderPassMultiviewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         HasField "pViewMasks" VkRenderPassMultiviewCreateInfo where
        type FieldType "pViewMasks" VkRenderPassMultiviewCreateInfo =
             Ptr Word32
        type FieldOptional "pViewMasks" VkRenderPassMultiviewCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewMasks" VkRenderPassMultiviewCreateInfo =
             #{offset VkRenderPassMultiviewCreateInfo, pViewMasks}
        type FieldIsArray "pViewMasks" VkRenderPassMultiviewCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, pViewMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pViewMasks" VkRenderPassMultiviewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, pViewMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, pViewMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewMasks" VkRenderPassMultiviewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, pViewMasks}

instance {-# OVERLAPPING #-}
         HasField "dependencyCount" VkRenderPassMultiviewCreateInfo where
        type FieldType "dependencyCount" VkRenderPassMultiviewCreateInfo =
             Word32
        type FieldOptional "dependencyCount"
               VkRenderPassMultiviewCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dependencyCount" VkRenderPassMultiviewCreateInfo
             =
             #{offset VkRenderPassMultiviewCreateInfo, dependencyCount}
        type FieldIsArray "dependencyCount" VkRenderPassMultiviewCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         CanReadField "dependencyCount" VkRenderPassMultiviewCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, dependencyCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         CanWriteField "dependencyCount" VkRenderPassMultiviewCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         HasField "pViewOffsets" VkRenderPassMultiviewCreateInfo where
        type FieldType "pViewOffsets" VkRenderPassMultiviewCreateInfo =
             Ptr Int32
        type FieldOptional "pViewOffsets" VkRenderPassMultiviewCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewOffsets" VkRenderPassMultiviewCreateInfo =
             #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets}
        type FieldIsArray "pViewOffsets" VkRenderPassMultiviewCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets}

instance {-# OVERLAPPING #-}
         CanReadField "pViewOffsets" VkRenderPassMultiviewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewOffsets" VkRenderPassMultiviewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, pViewOffsets}

instance {-# OVERLAPPING #-}
         HasField "correlationMaskCount" VkRenderPassMultiviewCreateInfo
         where
        type FieldType "correlationMaskCount"
               VkRenderPassMultiviewCreateInfo
             = Word32
        type FieldOptional "correlationMaskCount"
               VkRenderPassMultiviewCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "correlationMaskCount"
               VkRenderPassMultiviewCreateInfo
             =
             #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount}
        type FieldIsArray "correlationMaskCount"
               VkRenderPassMultiviewCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount}

instance {-# OVERLAPPING #-}
         CanReadField "correlationMaskCount" VkRenderPassMultiviewCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount}

instance {-# OVERLAPPING #-}
         CanWriteField "correlationMaskCount"
           VkRenderPassMultiviewCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, correlationMaskCount}

instance {-# OVERLAPPING #-}
         HasField "pCorrelationMasks" VkRenderPassMultiviewCreateInfo where
        type FieldType "pCorrelationMasks" VkRenderPassMultiviewCreateInfo
             = Ptr Word32
        type FieldOptional "pCorrelationMasks"
               VkRenderPassMultiviewCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pCorrelationMasks"
               VkRenderPassMultiviewCreateInfo
             =
             #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks}
        type FieldIsArray "pCorrelationMasks"
               VkRenderPassMultiviewCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pCorrelationMasks" VkRenderPassMultiviewCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pCorrelationMasks" VkRenderPassMultiviewCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfo, pCorrelationMasks}

instance Show VkRenderPassMultiviewCreateInfo where
        showsPrec d x
          = showString "VkRenderPassMultiviewCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "subpassCount = " .
                            showsPrec d (getField @"subpassCount" x) .
                              showString ", " .
                                showString "pViewMasks = " .
                                  showsPrec d (getField @"pViewMasks" x) .
                                    showString ", " .
                                      showString "dependencyCount = " .
                                        showsPrec d (getField @"dependencyCount" x) .
                                          showString ", " .
                                            showString "pViewOffsets = " .
                                              showsPrec d (getField @"pViewOffsets" x) .
                                                showString ", " .
                                                  showString "correlationMaskCount = " .
                                                    showsPrec d (getField @"correlationMaskCount" x)
                                                      .
                                                      showString ", " .
                                                        showString "pCorrelationMasks = " .
                                                          showsPrec d
                                                            (getField @"pCorrelationMasks" x)
                                                            . showChar '}'
