#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRenderPassMultiviewCreateInfoKHX
       (VkRenderPassMultiviewCreateInfoKHX(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType          (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo (VkRenderPassCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkRenderPassMultiviewCreateInfoKHX where
        type VkSTypeMType VkRenderPassMultiviewCreateInfoKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkRenderPassMultiviewCreateInfoKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, sType}

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

instance CanReadField "sType" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkRenderPassMultiviewCreateInfoKHX where
        type VkPNextMType VkRenderPassMultiviewCreateInfoKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkRenderPassMultiviewCreateInfoKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pNext}

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

instance CanReadField "pNext" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSubpassCount VkRenderPassMultiviewCreateInfoKHX where
        type VkSubpassCountMType VkRenderPassMultiviewCreateInfoKHX =
             Word32

        {-# NOINLINE vkSubpassCount #-}
        vkSubpassCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, subpassCount})

        {-# INLINE vkSubpassCountByteOffset #-}
        vkSubpassCountByteOffset ~_
          = #{offset VkRenderPassMultiviewCreateInfoKHX, subpassCount}

        {-# INLINE readVkSubpassCount #-}
        readVkSubpassCount p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, subpassCount}

        {-# INLINE writeVkSubpassCount #-}
        writeVkSubpassCount p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, subpassCount}

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

instance CanReadField "subpassCount"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkSubpassCount

        {-# INLINE readField #-}
        readField = readVkSubpassCount

instance CanWriteField "subpassCount"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubpassCount

instance {-# OVERLAPPING #-}
         HasVkPViewMasks VkRenderPassMultiviewCreateInfoKHX where
        type VkPViewMasksMType VkRenderPassMultiviewCreateInfoKHX =
             Ptr Word32

        {-# NOINLINE vkPViewMasks #-}
        vkPViewMasks x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, pViewMasks})

        {-# INLINE vkPViewMasksByteOffset #-}
        vkPViewMasksByteOffset ~_
          = #{offset VkRenderPassMultiviewCreateInfoKHX, pViewMasks}

        {-# INLINE readVkPViewMasks #-}
        readVkPViewMasks p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pViewMasks}

        {-# INLINE writeVkPViewMasks #-}
        writeVkPViewMasks p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pViewMasks}

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

instance CanReadField "pViewMasks"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPViewMasks

        {-# INLINE readField #-}
        readField = readVkPViewMasks

instance CanWriteField "pViewMasks"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPViewMasks

instance {-# OVERLAPPING #-}
         HasVkDependencyCount VkRenderPassMultiviewCreateInfoKHX where
        type VkDependencyCountMType VkRenderPassMultiviewCreateInfoKHX =
             Word32

        {-# NOINLINE vkDependencyCount #-}
        vkDependencyCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, dependencyCount})

        {-# INLINE vkDependencyCountByteOffset #-}
        vkDependencyCountByteOffset ~_
          = #{offset VkRenderPassMultiviewCreateInfoKHX, dependencyCount}

        {-# INLINE readVkDependencyCount #-}
        readVkDependencyCount p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, dependencyCount}

        {-# INLINE writeVkDependencyCount #-}
        writeVkDependencyCount p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, dependencyCount}

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

instance CanReadField "dependencyCount"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkDependencyCount

        {-# INLINE readField #-}
        readField = readVkDependencyCount

instance CanWriteField "dependencyCount"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDependencyCount

instance {-# OVERLAPPING #-}
         HasVkPViewOffsets VkRenderPassMultiviewCreateInfoKHX where
        type VkPViewOffsetsMType VkRenderPassMultiviewCreateInfoKHX =
             Ptr Int32

        {-# NOINLINE vkPViewOffsets #-}
        vkPViewOffsets x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, pViewOffsets})

        {-# INLINE vkPViewOffsetsByteOffset #-}
        vkPViewOffsetsByteOffset ~_
          = #{offset VkRenderPassMultiviewCreateInfoKHX, pViewOffsets}

        {-# INLINE readVkPViewOffsets #-}
        readVkPViewOffsets p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pViewOffsets}

        {-# INLINE writeVkPViewOffsets #-}
        writeVkPViewOffsets p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pViewOffsets}

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

instance CanReadField "pViewOffsets"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPViewOffsets

        {-# INLINE readField #-}
        readField = readVkPViewOffsets

instance CanWriteField "pViewOffsets"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPViewOffsets

instance {-# OVERLAPPING #-}
         HasVkCorrelationMaskCount VkRenderPassMultiviewCreateInfoKHX where
        type VkCorrelationMaskCountMType VkRenderPassMultiviewCreateInfoKHX
             = Word32

        {-# NOINLINE vkCorrelationMaskCount #-}
        vkCorrelationMaskCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, correlationMaskCount})

        {-# INLINE vkCorrelationMaskCountByteOffset #-}
        vkCorrelationMaskCountByteOffset ~_
          = #{offset VkRenderPassMultiviewCreateInfoKHX, correlationMaskCount}

        {-# INLINE readVkCorrelationMaskCount #-}
        readVkCorrelationMaskCount p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, correlationMaskCount}

        {-# INLINE writeVkCorrelationMaskCount #-}
        writeVkCorrelationMaskCount p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, correlationMaskCount}

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

instance CanReadField "correlationMaskCount"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkCorrelationMaskCount

        {-# INLINE readField #-}
        readField = readVkCorrelationMaskCount

instance CanWriteField "correlationMaskCount"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkCorrelationMaskCount

instance {-# OVERLAPPING #-}
         HasVkPCorrelationMasks VkRenderPassMultiviewCreateInfoKHX where
        type VkPCorrelationMasksMType VkRenderPassMultiviewCreateInfoKHX =
             Ptr Word32

        {-# NOINLINE vkPCorrelationMasks #-}
        vkPCorrelationMasks x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassMultiviewCreateInfoKHX, pCorrelationMasks})

        {-# INLINE vkPCorrelationMasksByteOffset #-}
        vkPCorrelationMasksByteOffset ~_
          = #{offset VkRenderPassMultiviewCreateInfoKHX, pCorrelationMasks}

        {-# INLINE readVkPCorrelationMasks #-}
        readVkPCorrelationMasks p
          = peekByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pCorrelationMasks}

        {-# INLINE writeVkPCorrelationMasks #-}
        writeVkPCorrelationMasks p
          = pokeByteOff p #{offset VkRenderPassMultiviewCreateInfoKHX, pCorrelationMasks}

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

instance CanReadField "pCorrelationMasks"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE getField #-}
        getField = vkPCorrelationMasks

        {-# INLINE readField #-}
        readField = readVkPCorrelationMasks

instance CanWriteField "pCorrelationMasks"
           VkRenderPassMultiviewCreateInfoKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPCorrelationMasks

instance Show VkRenderPassMultiviewCreateInfoKHX where
        showsPrec d x
          = showString "VkRenderPassMultiviewCreateInfoKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSubpassCount = " .
                            showsPrec d (vkSubpassCount x) .
                              showString ", " .
                                showString "vkPViewMasks = " .
                                  showsPrec d (vkPViewMasks x) .
                                    showString ", " .
                                      showString "vkDependencyCount = " .
                                        showsPrec d (vkDependencyCount x) .
                                          showString ", " .
                                            showString "vkPViewOffsets = " .
                                              showsPrec d (vkPViewOffsets x) .
                                                showString ", " .
                                                  showString "vkCorrelationMaskCount = " .
                                                    showsPrec d (vkCorrelationMaskCount x) .
                                                      showString ", " .
                                                        showString "vkPCorrelationMasks = " .
                                                          showsPrec d (vkPCorrelationMasks x) .
                                                            showChar '}'
