#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE UnboxedTuples   #-}
{-# LANGUAGE ViewPatterns    #-}
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
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
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
data VkRenderPassMultiviewCreateInfoKHX = VkRenderPassMultiviewCreateInfoKHX## ByteArray##

instance Eq VkRenderPassMultiviewCreateInfoKHX where
        (VkRenderPassMultiviewCreateInfoKHX## a) ==
          (VkRenderPassMultiviewCreateInfoKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassMultiviewCreateInfoKHX where
        (VkRenderPassMultiviewCreateInfoKHX## a) `compare`
          (VkRenderPassMultiviewCreateInfoKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkRenderPassMultiviewCreateInfoKHX where
        sizeOf ~_ = #{size VkRenderPassMultiviewCreateInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassMultiviewCreateInfoKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkRenderPassMultiviewCreateInfoKHX),
            I## a <- alignment (undefined :: VkRenderPassMultiviewCreateInfoKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkRenderPassMultiviewCreateInfoKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkRenderPassMultiviewCreateInfoKHX## ba)
          | I## n <- sizeOf (undefined :: VkRenderPassMultiviewCreateInfoKHX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkRenderPassMultiviewCreateInfoKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkRenderPassMultiviewCreateInfoKHX),
            I## a <- alignment (undefined :: VkRenderPassMultiviewCreateInfoKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkRenderPassMultiviewCreateInfoKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkRenderPassMultiviewCreateInfoKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkRenderPassMultiviewCreateInfoKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkRenderPassMultiviewCreateInfoKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkRenderPassMultiviewCreateInfoKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkRenderPassMultiviewCreateInfoKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkPhysicalDeviceMultiviewFeaturesKHX = VkPhysicalDeviceMultiviewFeaturesKHX## ByteArray##

instance Eq VkPhysicalDeviceMultiviewFeaturesKHX where
        (VkPhysicalDeviceMultiviewFeaturesKHX## a) ==
          (VkPhysicalDeviceMultiviewFeaturesKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMultiviewFeaturesKHX where
        (VkPhysicalDeviceMultiviewFeaturesKHX## a) `compare`
          (VkPhysicalDeviceMultiviewFeaturesKHX## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMultiviewFeaturesKHX where
        sizeOf ~_
          = #{size VkPhysicalDeviceMultiviewFeaturesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewFeaturesKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceMultiviewFeaturesKHX),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceMultiviewFeaturesKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceMultiviewFeaturesKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceMultiviewFeaturesKHX## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceMultiviewFeaturesKHX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceMultiviewFeaturesKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceMultiviewFeaturesKHX),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceMultiviewFeaturesKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceMultiviewFeaturesKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceMultiviewFeaturesKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceMultiviewFeaturesKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceMultiviewFeaturesKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceMultiviewFeaturesKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceMultiviewFeaturesKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
data VkPhysicalDeviceMultiviewPropertiesKHX = VkPhysicalDeviceMultiviewPropertiesKHX## ByteArray##

instance Eq VkPhysicalDeviceMultiviewPropertiesKHX where
        (VkPhysicalDeviceMultiviewPropertiesKHX## a) ==
          (VkPhysicalDeviceMultiviewPropertiesKHX## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMultiviewPropertiesKHX where
        (VkPhysicalDeviceMultiviewPropertiesKHX## a) `compare`
          (VkPhysicalDeviceMultiviewPropertiesKHX## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMultiviewPropertiesKHX where
        sizeOf ~_
          = #{size VkPhysicalDeviceMultiviewPropertiesKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewPropertiesKHX}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceMultiviewPropertiesKHX),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceMultiviewPropertiesKHX)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceMultiviewPropertiesKHX##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceMultiviewPropertiesKHX## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceMultiviewPropertiesKHX)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceMultiviewPropertiesKHX where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceMultiviewPropertiesKHX),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceMultiviewPropertiesKHX)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceMultiviewPropertiesKHX##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceMultiviewPropertiesKHX## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceMultiviewPropertiesKHX##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceMultiviewPropertiesKHX## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceMultiviewPropertiesKHX## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceMultiviewPropertiesKHX## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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

_VK_KHX_MULTIVIEW_EXTENSION_NAME :: CString

{-# INLINE _VK_KHX_MULTIVIEW_EXTENSION_NAME #-}
_VK_KHX_MULTIVIEW_EXTENSION_NAME = Ptr "VK_KHX_multiview\NUL"##

is_VK_KHX_MULTIVIEW_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHX_MULTIVIEW_EXTENSION_NAME #-}
is_VK_KHX_MULTIVIEW_EXTENSION_NAME
  = (_VK_KHX_MULTIVIEW_EXTENSION_NAME ==)

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
