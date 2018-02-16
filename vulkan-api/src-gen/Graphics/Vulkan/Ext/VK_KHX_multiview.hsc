#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_KHX_multiview
       (-- * Vulkan extension: @VK_KHX_multiview@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @54@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkRenderPassMultiviewCreateInfoKHX(..),
        VkPhysicalDeviceMultiviewFeaturesKHX(..),
        VkPhysicalDeviceMultiviewPropertiesKHX(..),
        VK_KHX_MULTIVIEW_SPEC_VERSION,
        pattern VK_KHX_MULTIVIEW_SPEC_VERSION,
        VK_KHX_MULTIVIEW_EXTENSION_NAME,
        pattern VK_KHX_MULTIVIEW_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHX,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHX,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHX,
        pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHX)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Common           (VkBool32,
                                                   VkDependencyFlagBits (..),
                                                   VkStructureType,
                                                   VkStructureType (..), Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkRenderPassMultiviewCreateInfoKHX.html VkRenderPassMultiviewCreateInfoKHX registry at www.khronos.org>
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

-- | > typedef struct VkPhysicalDeviceMultiviewFeaturesKHX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         multiview;
--   >     VkBool32                         multiviewGeometryShader;
--   >     VkBool32                         multiviewTessellationShader;
--   > } VkPhysicalDeviceMultiviewFeaturesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceMultiviewFeaturesKHX.html VkPhysicalDeviceMultiviewFeaturesKHX registry at www.khronos.org>
data VkPhysicalDeviceMultiviewFeaturesKHX = VkPhysicalDeviceMultiviewFeaturesKHX## Addr##
                                                                                  ByteArray##

instance Eq VkPhysicalDeviceMultiviewFeaturesKHX where
        (VkPhysicalDeviceMultiviewFeaturesKHX## a _) ==
          x@(VkPhysicalDeviceMultiviewFeaturesKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMultiviewFeaturesKHX where
        (VkPhysicalDeviceMultiviewFeaturesKHX## a _) `compare`
          x@(VkPhysicalDeviceMultiviewFeaturesKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMultiviewFeaturesKHX where
        sizeOf ~_
          = #{size VkPhysicalDeviceMultiviewFeaturesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewFeaturesKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMultiviewFeaturesKHX
         where
        unsafeAddr (VkPhysicalDeviceMultiviewFeaturesKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMultiviewFeaturesKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMultiviewFeaturesKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMultiviewFeaturesKHX where
        type StructFields VkPhysicalDeviceMultiviewFeaturesKHX =
             '["sType", "pNext", "multiview", "multiviewGeometryShader", -- ' closing tick for hsc2hs
               "multiviewTessellationShader"]

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceMultiviewFeaturesKHX where
        type VkSTypeMType VkPhysicalDeviceMultiviewFeaturesKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMultiviewFeaturesKHX where
        type FieldType "sType" VkPhysicalDeviceMultiviewFeaturesKHX =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMultiviewFeaturesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceMultiviewFeaturesKHX =
             #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, sType}

instance CanReadField "sType" VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceMultiviewFeaturesKHX where
        type VkPNextMType VkPhysicalDeviceMultiviewFeaturesKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMultiviewFeaturesKHX where
        type FieldType "pNext" VkPhysicalDeviceMultiviewFeaturesKHX =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMultiviewFeaturesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceMultiviewFeaturesKHX =
             #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, pNext}

instance CanReadField "pNext" VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMultiview VkPhysicalDeviceMultiviewFeaturesKHX where
        type VkMultiviewMType VkPhysicalDeviceMultiviewFeaturesKHX =
             VkBool32

        {-# NOINLINE vkMultiview #-}
        vkMultiview x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview})

        {-# INLINE vkMultiviewByteOffset #-}
        vkMultiviewByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

        {-# INLINE readVkMultiview #-}
        readVkMultiview p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

        {-# INLINE writeVkMultiview #-}
        writeVkMultiview p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

instance {-# OVERLAPPING #-}
         HasField "multiview" VkPhysicalDeviceMultiviewFeaturesKHX where
        type FieldType "multiview" VkPhysicalDeviceMultiviewFeaturesKHX =
             VkBool32
        type FieldOptional "multiview" VkPhysicalDeviceMultiviewFeaturesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "multiview" VkPhysicalDeviceMultiviewFeaturesKHX =
             #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiview}

instance CanReadField "multiview"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE getField #-}
        getField = vkMultiview

        {-# INLINE readField #-}
        readField = readVkMultiview

instance CanWriteField "multiview"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMultiview

instance {-# OVERLAPPING #-}
         HasVkMultiviewGeometryShader VkPhysicalDeviceMultiviewFeaturesKHX
         where
        type VkMultiviewGeometryShaderMType
               VkPhysicalDeviceMultiviewFeaturesKHX
             = VkBool32

        {-# NOINLINE vkMultiviewGeometryShader #-}
        vkMultiviewGeometryShader x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader})

        {-# INLINE vkMultiviewGeometryShaderByteOffset #-}
        vkMultiviewGeometryShaderByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

        {-# INLINE readVkMultiviewGeometryShader #-}
        readVkMultiviewGeometryShader p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

        {-# INLINE writeVkMultiviewGeometryShader #-}
        writeVkMultiviewGeometryShader p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

instance {-# OVERLAPPING #-}
         HasField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        type FieldType "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             = VkBool32
        type FieldOptional "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "multiviewGeometryShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             =
             #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewGeometryShader}

instance CanReadField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE getField #-}
        getField = vkMultiviewGeometryShader

        {-# INLINE readField #-}
        readField = readVkMultiviewGeometryShader

instance CanWriteField "multiviewGeometryShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMultiviewGeometryShader

instance {-# OVERLAPPING #-}
         HasVkMultiviewTessellationShader
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        type VkMultiviewTessellationShaderMType
               VkPhysicalDeviceMultiviewFeaturesKHX
             = VkBool32

        {-# NOINLINE vkMultiviewTessellationShader #-}
        vkMultiviewTessellationShader x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader})

        {-# INLINE vkMultiviewTessellationShaderByteOffset #-}
        vkMultiviewTessellationShaderByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

        {-# INLINE readVkMultiviewTessellationShader #-}
        readVkMultiviewTessellationShader p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

        {-# INLINE writeVkMultiviewTessellationShader #-}
        writeVkMultiviewTessellationShader p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

instance {-# OVERLAPPING #-}
         HasField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        type FieldType "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             = VkBool32
        type FieldOptional "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "multiviewTessellationShader"
               VkPhysicalDeviceMultiviewFeaturesKHX
             =
             #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewFeaturesKHX, multiviewTessellationShader}

instance CanReadField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE getField #-}
        getField = vkMultiviewTessellationShader

        {-# INLINE readField #-}
        readField = readVkMultiviewTessellationShader

instance CanWriteField "multiviewTessellationShader"
           VkPhysicalDeviceMultiviewFeaturesKHX
         where
        {-# INLINE writeField #-}
        writeField = writeVkMultiviewTessellationShader

instance Show VkPhysicalDeviceMultiviewFeaturesKHX where
        showsPrec d x
          = showString "VkPhysicalDeviceMultiviewFeaturesKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMultiview = " .
                            showsPrec d (vkMultiview x) .
                              showString ", " .
                                showString "vkMultiviewGeometryShader = " .
                                  showsPrec d (vkMultiviewGeometryShader x) .
                                    showString ", " .
                                      showString "vkMultiviewTessellationShader = " .
                                        showsPrec d (vkMultiviewTessellationShader x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceMultiviewPropertiesKHX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         maxMultiviewViewCount;
--   >     uint32_t                         maxMultiviewInstanceIndex;
--   > } VkPhysicalDeviceMultiviewPropertiesKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceMultiviewPropertiesKHX.html VkPhysicalDeviceMultiviewPropertiesKHX registry at www.khronos.org>
data VkPhysicalDeviceMultiviewPropertiesKHX = VkPhysicalDeviceMultiviewPropertiesKHX## Addr##
                                                                                      ByteArray##

instance Eq VkPhysicalDeviceMultiviewPropertiesKHX where
        (VkPhysicalDeviceMultiviewPropertiesKHX## a _) ==
          x@(VkPhysicalDeviceMultiviewPropertiesKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMultiviewPropertiesKHX where
        (VkPhysicalDeviceMultiviewPropertiesKHX## a _) `compare`
          x@(VkPhysicalDeviceMultiviewPropertiesKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMultiviewPropertiesKHX where
        sizeOf ~_
          = #{size VkPhysicalDeviceMultiviewPropertiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewPropertiesKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceMultiviewPropertiesKHX
         where
        unsafeAddr (VkPhysicalDeviceMultiviewPropertiesKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceMultiviewPropertiesKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMultiviewPropertiesKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceMultiviewPropertiesKHX where
        type StructFields VkPhysicalDeviceMultiviewPropertiesKHX =
             '["sType", "pNext", "maxMultiviewViewCount", -- ' closing tick for hsc2hs
               "maxMultiviewInstanceIndex"]

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceMultiviewPropertiesKHX where
        type VkSTypeMType VkPhysicalDeviceMultiviewPropertiesKHX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMultiviewPropertiesKHX where
        type FieldType "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceMultiviewPropertiesKHX =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, sType}

instance CanReadField "sType"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceMultiviewPropertiesKHX where
        type VkPNextMType VkPhysicalDeviceMultiviewPropertiesKHX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMultiviewPropertiesKHX where
        type FieldType "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceMultiviewPropertiesKHX =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkMaxMultiviewViewCount VkPhysicalDeviceMultiviewPropertiesKHX
         where
        type VkMaxMultiviewViewCountMType
               VkPhysicalDeviceMultiviewPropertiesKHX
             = Word32

        {-# NOINLINE vkMaxMultiviewViewCount #-}
        vkMaxMultiviewViewCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount})

        {-# INLINE vkMaxMultiviewViewCountByteOffset #-}
        vkMaxMultiviewViewCountByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

        {-# INLINE readVkMaxMultiviewViewCount #-}
        readVkMaxMultiviewViewCount p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

        {-# INLINE writeVkMaxMultiviewViewCount #-}
        writeVkMaxMultiviewViewCount p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

instance {-# OVERLAPPING #-}
         HasField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        type FieldType "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = Word32
        type FieldOptional "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMultiviewViewCount"
               VkPhysicalDeviceMultiviewPropertiesKHX
             =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewViewCount}

instance CanReadField "maxMultiviewViewCount"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkMaxMultiviewViewCount

        {-# INLINE readField #-}
        readField = readVkMaxMultiviewViewCount

instance {-# OVERLAPPING #-}
         HasVkMaxMultiviewInstanceIndex
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        type VkMaxMultiviewInstanceIndexMType
               VkPhysicalDeviceMultiviewPropertiesKHX
             = Word32

        {-# NOINLINE vkMaxMultiviewInstanceIndex #-}
        vkMaxMultiviewInstanceIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex})

        {-# INLINE vkMaxMultiviewInstanceIndexByteOffset #-}
        vkMaxMultiviewInstanceIndexByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

        {-# INLINE readVkMaxMultiviewInstanceIndex #-}
        readVkMaxMultiviewInstanceIndex p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

        {-# INLINE writeVkMaxMultiviewInstanceIndex #-}
        writeVkMaxMultiviewInstanceIndex p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

instance {-# OVERLAPPING #-}
         HasField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        type FieldType "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = Word32
        type FieldOptional "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxMultiviewInstanceIndex"
               VkPhysicalDeviceMultiviewPropertiesKHX
             =
             #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPropertiesKHX, maxMultiviewInstanceIndex}

instance CanReadField "maxMultiviewInstanceIndex"
           VkPhysicalDeviceMultiviewPropertiesKHX
         where
        {-# INLINE getField #-}
        getField = vkMaxMultiviewInstanceIndex

        {-# INLINE readField #-}
        readField = readVkMaxMultiviewInstanceIndex

instance Show VkPhysicalDeviceMultiviewPropertiesKHX where
        showsPrec d x
          = showString "VkPhysicalDeviceMultiviewPropertiesKHX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMaxMultiviewViewCount = " .
                            showsPrec d (vkMaxMultiviewViewCount x) .
                              showString ", " .
                                showString "vkMaxMultiviewInstanceIndex = " .
                                  showsPrec d (vkMaxMultiviewInstanceIndex x) . showChar '}'

pattern VK_KHX_MULTIVIEW_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHX_MULTIVIEW_SPEC_VERSION = 1

type VK_KHX_MULTIVIEW_SPEC_VERSION = 1

pattern VK_KHX_MULTIVIEW_EXTENSION_NAME :: CString

pattern VK_KHX_MULTIVIEW_EXTENSION_NAME <-
        (is_VK_KHX_MULTIVIEW_EXTENSION_NAME -> True)
  where VK_KHX_MULTIVIEW_EXTENSION_NAME
          = _VK_KHX_MULTIVIEW_EXTENSION_NAME

{-# INLINE _VK_KHX_MULTIVIEW_EXTENSION_NAME #-}

_VK_KHX_MULTIVIEW_EXTENSION_NAME :: CString
_VK_KHX_MULTIVIEW_EXTENSION_NAME = Ptr "VK_KHX_multiview\NUL"##

{-# INLINE is_VK_KHX_MULTIVIEW_EXTENSION_NAME #-}

is_VK_KHX_MULTIVIEW_EXTENSION_NAME :: CString -> Bool
is_VK_KHX_MULTIVIEW_EXTENSION_NAME
  = eqCStrings _VK_KHX_MULTIVIEW_EXTENSION_NAME

type VK_KHX_MULTIVIEW_EXTENSION_NAME = "VK_KHX_multiview"

pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHX =
        VkStructureType 1000053000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHX ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHX =
        VkStructureType 1000053001

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHX
        = VkStructureType 1000053002

-- | bitpos = @1@
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHX :: VkDependencyFlagBits

pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHX = VkDependencyFlagBits 2
