#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRenderPassMultiviewCreateInfoKHX
       (VkRenderPassMultiviewCreateInfoKHX(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType          (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo (VkRenderPassCreateInfo)
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkRenderPassMultiviewCreateInfoKHX {
--   >     VkStructureType        sType;
--   >     const void*            pNext;
--   >     uint32_t               subpassCount;
--   >     const uint32_t*     pViewMasks;
--   >     uint32_t               dependencyCount;
--   >     const int32_t*   pViewOffsets;
--   >     uint32_t               correlationMaskCount;
--   >     const uint32_t* pCorrelationMasks;
--   > } VkRenderPassMultiviewCreateInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkRenderPassMultiviewCreateInfoKHX.html VkRenderPassMultiviewCreateInfoKHX registry at www.khronos.org>
data VkRenderPassMultiviewCreateInfoKHX = VkRenderPassMultiviewCreateInfoKHX## Addr##
                                                                              ByteArray##

instance Eq VkRenderPassMultiviewCreateInfoKHX where
        (VkRenderPassMultiviewCreateInfoKHX## a _) ==
          x@(VkRenderPassMultiviewCreateInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassMultiviewCreateInfoKHX where
        (VkRenderPassMultiviewCreateInfoKHX## a _) `compare`
          x@(VkRenderPassMultiviewCreateInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassMultiviewCreateInfoKHX where
        sizeOf ~_ = #{size VkRenderPassMultiviewCreateInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassMultiviewCreateInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassMultiviewCreateInfoKHX where
        unsafeAddr (VkRenderPassMultiviewCreateInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassMultiviewCreateInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassMultiviewCreateInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassMultiviewCreateInfoKHX where
        type StructFields VkRenderPassMultiviewCreateInfoKHX =
             '["sType", "pNext", "subpassCount", "pViewMasks", -- ' closing tick for hsc2hs
               "dependencyCount", "pViewOffsets", "correlationMaskCount",
               "pCorrelationMasks"]
        type CUnionType VkRenderPassMultiviewCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassMultiviewCreateInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassMultiviewCreateInfoKHX =
             '[VkRenderPassCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassMultiviewCreateInfoKHX where
        type FieldType "sType" VkRenderPassMultiviewCreateInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkRenderPassMultiviewCreateInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassMultiviewCreateInfoKHX =
             #{offset VkRenderPassMultiviewCreateInfoKHX, sType}
        type FieldIsArray "sType" VkRenderPassMultiviewCreateInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassMultiviewCreateInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkRenderPassMultiviewCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassMultiviewCreateInfoKHX where
        type FieldType "pNext" VkRenderPassMultiviewCreateInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkRenderPassMultiviewCreateInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassMultiviewCreateInfoKHX =
             #{offset VkRenderPassMultiviewCreateInfoKHX, pNext}
        type FieldIsArray "pNext" VkRenderPassMultiviewCreateInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassMultiviewCreateInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkRenderPassMultiviewCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "subpassCount" VkRenderPassMultiviewCreateInfoKHX where
        type FieldType "subpassCount" VkRenderPassMultiviewCreateInfoKHX =
             Word32
        type FieldOptional "subpassCount"
               VkRenderPassMultiviewCreateInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "subpassCount" VkRenderPassMultiviewCreateInfoKHX
             =
             #{offset VkRenderPassMultiviewCreateInfoKHX, subpassCount}
        type FieldIsArray "subpassCount" VkRenderPassMultiviewCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfoKHX, subpassCount}

instance {-# OVERLAPPING #-}
         CanReadField "subpassCount" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, subpassCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, subpassCount}

instance {-# OVERLAPPING #-}
         CanWriteField "subpassCount" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, subpassCount}

instance {-# OVERLAPPING #-}
         HasField "pViewMasks" VkRenderPassMultiviewCreateInfoKHX where
        type FieldType "pViewMasks" VkRenderPassMultiviewCreateInfoKHX =
             Ptr Word32
        type FieldOptional "pViewMasks" VkRenderPassMultiviewCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewMasks" VkRenderPassMultiviewCreateInfoKHX =
             #{offset VkRenderPassMultiviewCreateInfoKHX, pViewMasks}
        type FieldIsArray "pViewMasks" VkRenderPassMultiviewCreateInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfoKHX, pViewMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pViewMasks" VkRenderPassMultiviewCreateInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, pViewMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pViewMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewMasks" VkRenderPassMultiviewCreateInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pViewMasks}

instance {-# OVERLAPPING #-}
         HasField "dependencyCount" VkRenderPassMultiviewCreateInfoKHX where
        type FieldType "dependencyCount" VkRenderPassMultiviewCreateInfoKHX
             = Word32
        type FieldOptional "dependencyCount"
               VkRenderPassMultiviewCreateInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dependencyCount"
               VkRenderPassMultiviewCreateInfoKHX
             =
             #{offset VkRenderPassMultiviewCreateInfoKHX, dependencyCount}
        type FieldIsArray "dependencyCount"
               VkRenderPassMultiviewCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfoKHX, dependencyCount}

instance {-# OVERLAPPING #-}
         CanReadField "dependencyCount" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, dependencyCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, dependencyCount}

instance {-# OVERLAPPING #-}
         CanWriteField "dependencyCount" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, dependencyCount}

instance {-# OVERLAPPING #-}
         HasField "pViewOffsets" VkRenderPassMultiviewCreateInfoKHX where
        type FieldType "pViewOffsets" VkRenderPassMultiviewCreateInfoKHX =
             Ptr Int32
        type FieldOptional "pViewOffsets"
               VkRenderPassMultiviewCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewOffsets" VkRenderPassMultiviewCreateInfoKHX
             =
             #{offset VkRenderPassMultiviewCreateInfoKHX, pViewOffsets}
        type FieldIsArray "pViewOffsets" VkRenderPassMultiviewCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfoKHX, pViewOffsets}

instance {-# OVERLAPPING #-}
         CanReadField "pViewOffsets" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, pViewOffsets})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pViewOffsets}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewOffsets" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pViewOffsets}

instance {-# OVERLAPPING #-}
         HasField "correlationMaskCount" VkRenderPassMultiviewCreateInfoKHX
         where
        type FieldType "correlationMaskCount"
               VkRenderPassMultiviewCreateInfoKHX
             = Word32
        type FieldOptional "correlationMaskCount"
               VkRenderPassMultiviewCreateInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "correlationMaskCount"
               VkRenderPassMultiviewCreateInfoKHX
             =
             #{offset VkRenderPassMultiviewCreateInfoKHX, correlationMaskCount}
        type FieldIsArray "correlationMaskCount"
               VkRenderPassMultiviewCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfoKHX, correlationMaskCount}

instance {-# OVERLAPPING #-}
         CanReadField "correlationMaskCount"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, correlationMaskCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, correlationMaskCount}

instance {-# OVERLAPPING #-}
         CanWriteField "correlationMaskCount"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, correlationMaskCount}

instance {-# OVERLAPPING #-}
         HasField "pCorrelationMasks" VkRenderPassMultiviewCreateInfoKHX
         where
        type FieldType "pCorrelationMasks"
               VkRenderPassMultiviewCreateInfoKHX
             = Ptr Word32
        type FieldOptional "pCorrelationMasks"
               VkRenderPassMultiviewCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pCorrelationMasks"
               VkRenderPassMultiviewCreateInfoKHX
             =
             #{offset VkRenderPassMultiviewCreateInfoKHX, pCorrelationMasks}
        type FieldIsArray "pCorrelationMasks"
               VkRenderPassMultiviewCreateInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassMultiviewCreateInfoKHX, pCorrelationMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pCorrelationMasks" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, pCorrelationMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pCorrelationMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pCorrelationMasks"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pCorrelationMasks}

instance Show VkRenderPassMultiviewCreateInfoKHX where
        showsPrec d x
          = showString "VkRenderPassMultiviewCreateInfoKHX {" .
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
