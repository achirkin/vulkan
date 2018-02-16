#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_display
       (-- * Vulkan extension: @VK_KHR_display@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo,Norbert Nopper @FslNopper@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @3@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkDisplayPropertiesKHR(..), VkDisplayModeParametersKHR(..),
        VkDisplayModePropertiesKHR(..), VkDisplayModeCreateInfoKHR(..),
        VkDisplayPlaneCapabilitiesKHR(..), VkDisplayPlanePropertiesKHR(..),
        VkDisplaySurfaceCreateInfoKHR(..),
        vkGetPhysicalDeviceDisplayPropertiesKHR,
        vkGetPhysicalDeviceDisplayPlanePropertiesKHR,
        vkGetDisplayPlaneSupportedDisplaysKHR,
        vkGetDisplayModePropertiesKHR, vkCreateDisplayModeKHR,
        vkGetDisplayPlaneCapabilitiesKHR, vkCreateDisplayPlaneSurfaceKHR,
        VK_KHR_DISPLAY_SPEC_VERSION, pattern VK_KHR_DISPLAY_SPEC_VERSION,
        VK_KHR_DISPLAY_EXTENSION_NAME,
        pattern VK_KHR_DISPLAY_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR,
        pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR,
        pattern VK_OBJECT_TYPE_DISPLAY_KHR,
        pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..),
                                                   VkExtent2D, VkOffset2D)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayPropertiesKHR {
--   >     VkDisplayKHR                     display;
--   >     const char*                      displayName;
--   >     VkExtent2D                       physicalDimensions;
--   >     VkExtent2D                       physicalResolution;
--   >     VkSurfaceTransformFlagsKHR       supportedTransforms;
--   >     VkBool32                         planeReorderPossible;
--   >     VkBool32                         persistentContent;
--   > } VkDisplayPropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayPropertiesKHR.html VkDisplayPropertiesKHR registry at www.khronos.org>
data VkDisplayPropertiesKHR = VkDisplayPropertiesKHR## Addr##
                                                      ByteArray##

instance Eq VkDisplayPropertiesKHR where
        (VkDisplayPropertiesKHR## a _) == x@(VkDisplayPropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPropertiesKHR where
        (VkDisplayPropertiesKHR## a _) `compare`
          x@(VkDisplayPropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPropertiesKHR where
        sizeOf ~_ = #{size VkDisplayPropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPropertiesKHR where
        unsafeAddr (VkDisplayPropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPropertiesKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPropertiesKHR where
        type StructFields VkDisplayPropertiesKHR =
             '["display", "displayName", "physicalDimensions", -- ' closing tick for hsc2hs
               "physicalResolution", "supportedTransforms",
               "planeReorderPossible", "persistentContent"]

instance {-# OVERLAPPING #-} HasVkDisplay VkDisplayPropertiesKHR
         where
        type VkDisplayMType VkDisplayPropertiesKHR = VkDisplayKHR

        {-# NOINLINE vkDisplay #-}
        vkDisplay x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, display})

        {-# INLINE vkDisplayByteOffset #-}
        vkDisplayByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, display}

        {-# INLINE readVkDisplay #-}
        readVkDisplay p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, display}

        {-# INLINE writeVkDisplay #-}
        writeVkDisplay p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, display}

instance {-# OVERLAPPING #-}
         HasField "display" VkDisplayPropertiesKHR where
        type FieldType "display" VkDisplayPropertiesKHR = VkDisplayKHR
        type FieldOptional "display" VkDisplayPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "display" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, display}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPropertiesKHR, display}

instance CanReadField "display" VkDisplayPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkDisplay

        {-# INLINE readField #-}
        readField = readVkDisplay

instance {-# OVERLAPPING #-}
         HasVkDisplayName VkDisplayPropertiesKHR where
        type VkDisplayNameMType VkDisplayPropertiesKHR = CString

        {-# NOINLINE vkDisplayName #-}
        vkDisplayName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, displayName})

        {-# INLINE vkDisplayNameByteOffset #-}
        vkDisplayNameByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, displayName}

        {-# INLINE readVkDisplayName #-}
        readVkDisplayName p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, displayName}

        {-# INLINE writeVkDisplayName #-}
        writeVkDisplayName p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, displayName}

instance {-# OVERLAPPING #-}
         HasField "displayName" VkDisplayPropertiesKHR where
        type FieldType "displayName" VkDisplayPropertiesKHR = CString
        type FieldOptional "displayName" VkDisplayPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "displayName" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, displayName}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, displayName}

instance CanReadField "displayName" VkDisplayPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkDisplayName

        {-# INLINE readField #-}
        readField = readVkDisplayName

instance {-# OVERLAPPING #-}
         HasVkPhysicalDimensions VkDisplayPropertiesKHR where
        type VkPhysicalDimensionsMType VkDisplayPropertiesKHR = VkExtent2D

        {-# NOINLINE vkPhysicalDimensions #-}
        vkPhysicalDimensions x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, physicalDimensions})

        {-# INLINE vkPhysicalDimensionsByteOffset #-}
        vkPhysicalDimensionsByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, physicalDimensions}

        {-# INLINE readVkPhysicalDimensions #-}
        readVkPhysicalDimensions p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, physicalDimensions}

        {-# INLINE writeVkPhysicalDimensions #-}
        writeVkPhysicalDimensions p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, physicalDimensions}

instance {-# OVERLAPPING #-}
         HasField "physicalDimensions" VkDisplayPropertiesKHR where
        type FieldType "physicalDimensions" VkDisplayPropertiesKHR =
             VkExtent2D
        type FieldOptional "physicalDimensions" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "physicalDimensions" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, physicalDimensions}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, physicalDimensions}

instance CanReadField "physicalDimensions" VkDisplayPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPhysicalDimensions

        {-# INLINE readField #-}
        readField = readVkPhysicalDimensions

instance {-# OVERLAPPING #-}
         HasVkPhysicalResolution VkDisplayPropertiesKHR where
        type VkPhysicalResolutionMType VkDisplayPropertiesKHR = VkExtent2D

        {-# NOINLINE vkPhysicalResolution #-}
        vkPhysicalResolution x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, physicalResolution})

        {-# INLINE vkPhysicalResolutionByteOffset #-}
        vkPhysicalResolutionByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, physicalResolution}

        {-# INLINE readVkPhysicalResolution #-}
        readVkPhysicalResolution p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, physicalResolution}

        {-# INLINE writeVkPhysicalResolution #-}
        writeVkPhysicalResolution p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, physicalResolution}

instance {-# OVERLAPPING #-}
         HasField "physicalResolution" VkDisplayPropertiesKHR where
        type FieldType "physicalResolution" VkDisplayPropertiesKHR =
             VkExtent2D
        type FieldOptional "physicalResolution" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "physicalResolution" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, physicalResolution}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, physicalResolution}

instance CanReadField "physicalResolution" VkDisplayPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPhysicalResolution

        {-# INLINE readField #-}
        readField = readVkPhysicalResolution

instance {-# OVERLAPPING #-}
         HasVkSupportedTransforms VkDisplayPropertiesKHR where
        type VkSupportedTransformsMType VkDisplayPropertiesKHR =
             VkSurfaceTransformFlagsKHR

        {-# NOINLINE vkSupportedTransforms #-}
        vkSupportedTransforms x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, supportedTransforms})

        {-# INLINE vkSupportedTransformsByteOffset #-}
        vkSupportedTransformsByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, supportedTransforms}

        {-# INLINE readVkSupportedTransforms #-}
        readVkSupportedTransforms p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, supportedTransforms}

        {-# INLINE writeVkSupportedTransforms #-}
        writeVkSupportedTransforms p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         HasField "supportedTransforms" VkDisplayPropertiesKHR where
        type FieldType "supportedTransforms" VkDisplayPropertiesKHR =
             VkSurfaceTransformFlagsKHR
        type FieldOptional "supportedTransforms" VkDisplayPropertiesKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedTransforms" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, supportedTransforms}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, supportedTransforms}

instance CanReadField "supportedTransforms" VkDisplayPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSupportedTransforms

        {-# INLINE readField #-}
        readField = readVkSupportedTransforms

instance {-# OVERLAPPING #-}
         HasVkPlaneReorderPossible VkDisplayPropertiesKHR where
        type VkPlaneReorderPossibleMType VkDisplayPropertiesKHR = VkBool32

        {-# NOINLINE vkPlaneReorderPossible #-}
        vkPlaneReorderPossible x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, planeReorderPossible})

        {-# INLINE vkPlaneReorderPossibleByteOffset #-}
        vkPlaneReorderPossibleByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, planeReorderPossible}

        {-# INLINE readVkPlaneReorderPossible #-}
        readVkPlaneReorderPossible p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, planeReorderPossible}

        {-# INLINE writeVkPlaneReorderPossible #-}
        writeVkPlaneReorderPossible p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, planeReorderPossible}

instance {-# OVERLAPPING #-}
         HasField "planeReorderPossible" VkDisplayPropertiesKHR where
        type FieldType "planeReorderPossible" VkDisplayPropertiesKHR =
             VkBool32
        type FieldOptional "planeReorderPossible" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "planeReorderPossible" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, planeReorderPossible}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, planeReorderPossible}

instance CanReadField "planeReorderPossible" VkDisplayPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPlaneReorderPossible

        {-# INLINE readField #-}
        readField = readVkPlaneReorderPossible

instance {-# OVERLAPPING #-}
         HasVkPersistentContent VkDisplayPropertiesKHR where
        type VkPersistentContentMType VkDisplayPropertiesKHR = VkBool32

        {-# NOINLINE vkPersistentContent #-}
        vkPersistentContent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPropertiesKHR, persistentContent})

        {-# INLINE vkPersistentContentByteOffset #-}
        vkPersistentContentByteOffset ~_
          = #{offset VkDisplayPropertiesKHR, persistentContent}

        {-# INLINE readVkPersistentContent #-}
        readVkPersistentContent p
          = peekByteOff p #{offset VkDisplayPropertiesKHR, persistentContent}

        {-# INLINE writeVkPersistentContent #-}
        writeVkPersistentContent p
          = pokeByteOff p #{offset VkDisplayPropertiesKHR, persistentContent}

instance {-# OVERLAPPING #-}
         HasField "persistentContent" VkDisplayPropertiesKHR where
        type FieldType "persistentContent" VkDisplayPropertiesKHR =
             VkBool32
        type FieldOptional "persistentContent" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "persistentContent" VkDisplayPropertiesKHR =
             #{offset VkDisplayPropertiesKHR, persistentContent}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPropertiesKHR, persistentContent}

instance CanReadField "persistentContent" VkDisplayPropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPersistentContent

        {-# INLINE readField #-}
        readField = readVkPersistentContent

instance Show VkDisplayPropertiesKHR where
        showsPrec d x
          = showString "VkDisplayPropertiesKHR {" .
              showString "vkDisplay = " .
                showsPrec d (vkDisplay x) .
                  showString ", " .
                    showString "vkDisplayName = " .
                      showsPrec d (vkDisplayName x) .
                        showString ", " .
                          showString "vkPhysicalDimensions = " .
                            showsPrec d (vkPhysicalDimensions x) .
                              showString ", " .
                                showString "vkPhysicalResolution = " .
                                  showsPrec d (vkPhysicalResolution x) .
                                    showString ", " .
                                      showString "vkSupportedTransforms = " .
                                        showsPrec d (vkSupportedTransforms x) .
                                          showString ", " .
                                            showString "vkPlaneReorderPossible = " .
                                              showsPrec d (vkPlaneReorderPossible x) .
                                                showString ", " .
                                                  showString "vkPersistentContent = " .
                                                    showsPrec d (vkPersistentContent x) .
                                                      showChar '}'

-- | > typedef struct VkDisplayModeParametersKHR {
--   >     VkExtent2D                       visibleRegion;
--   >     uint32_t                         refreshRate;
--   > } VkDisplayModeParametersKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayModeParametersKHR.html VkDisplayModeParametersKHR registry at www.khronos.org>
data VkDisplayModeParametersKHR = VkDisplayModeParametersKHR## Addr##
                                                              ByteArray##

instance Eq VkDisplayModeParametersKHR where
        (VkDisplayModeParametersKHR## a _) ==
          x@(VkDisplayModeParametersKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayModeParametersKHR where
        (VkDisplayModeParametersKHR## a _) `compare`
          x@(VkDisplayModeParametersKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayModeParametersKHR where
        sizeOf ~_ = #{size VkDisplayModeParametersKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayModeParametersKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayModeParametersKHR where
        unsafeAddr (VkDisplayModeParametersKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayModeParametersKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayModeParametersKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayModeParametersKHR where
        type StructFields VkDisplayModeParametersKHR =
             '["visibleRegion", "refreshRate"] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkVisibleRegion VkDisplayModeParametersKHR where
        type VkVisibleRegionMType VkDisplayModeParametersKHR = VkExtent2D

        {-# NOINLINE vkVisibleRegion #-}
        vkVisibleRegion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeParametersKHR, visibleRegion})

        {-# INLINE vkVisibleRegionByteOffset #-}
        vkVisibleRegionByteOffset ~_
          = #{offset VkDisplayModeParametersKHR, visibleRegion}

        {-# INLINE readVkVisibleRegion #-}
        readVkVisibleRegion p
          = peekByteOff p #{offset VkDisplayModeParametersKHR, visibleRegion}

        {-# INLINE writeVkVisibleRegion #-}
        writeVkVisibleRegion p
          = pokeByteOff p #{offset VkDisplayModeParametersKHR, visibleRegion}

instance {-# OVERLAPPING #-}
         HasField "visibleRegion" VkDisplayModeParametersKHR where
        type FieldType "visibleRegion" VkDisplayModeParametersKHR =
             VkExtent2D
        type FieldOptional "visibleRegion" VkDisplayModeParametersKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "visibleRegion" VkDisplayModeParametersKHR =
             #{offset VkDisplayModeParametersKHR, visibleRegion}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeParametersKHR, visibleRegion}

instance CanReadField "visibleRegion" VkDisplayModeParametersKHR
         where
        {-# INLINE getField #-}
        getField = vkVisibleRegion

        {-# INLINE readField #-}
        readField = readVkVisibleRegion

instance CanWriteField "visibleRegion" VkDisplayModeParametersKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkVisibleRegion

instance {-# OVERLAPPING #-}
         HasVkRefreshRate VkDisplayModeParametersKHR where
        type VkRefreshRateMType VkDisplayModeParametersKHR = Word32

        {-# NOINLINE vkRefreshRate #-}
        vkRefreshRate x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeParametersKHR, refreshRate})

        {-# INLINE vkRefreshRateByteOffset #-}
        vkRefreshRateByteOffset ~_
          = #{offset VkDisplayModeParametersKHR, refreshRate}

        {-# INLINE readVkRefreshRate #-}
        readVkRefreshRate p
          = peekByteOff p #{offset VkDisplayModeParametersKHR, refreshRate}

        {-# INLINE writeVkRefreshRate #-}
        writeVkRefreshRate p
          = pokeByteOff p #{offset VkDisplayModeParametersKHR, refreshRate}

instance {-# OVERLAPPING #-}
         HasField "refreshRate" VkDisplayModeParametersKHR where
        type FieldType "refreshRate" VkDisplayModeParametersKHR = Word32
        type FieldOptional "refreshRate" VkDisplayModeParametersKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "refreshRate" VkDisplayModeParametersKHR =
             #{offset VkDisplayModeParametersKHR, refreshRate}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeParametersKHR, refreshRate}

instance CanReadField "refreshRate" VkDisplayModeParametersKHR
         where
        {-# INLINE getField #-}
        getField = vkRefreshRate

        {-# INLINE readField #-}
        readField = readVkRefreshRate

instance CanWriteField "refreshRate" VkDisplayModeParametersKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkRefreshRate

instance Show VkDisplayModeParametersKHR where
        showsPrec d x
          = showString "VkDisplayModeParametersKHR {" .
              showString "vkVisibleRegion = " .
                showsPrec d (vkVisibleRegion x) .
                  showString ", " .
                    showString "vkRefreshRate = " .
                      showsPrec d (vkRefreshRate x) . showChar '}'

-- | > typedef struct VkDisplayModePropertiesKHR {
--   >     VkDisplayModeKHR                 displayMode;
--   >     VkDisplayModeParametersKHR       parameters;
--   > } VkDisplayModePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayModePropertiesKHR.html VkDisplayModePropertiesKHR registry at www.khronos.org>
data VkDisplayModePropertiesKHR = VkDisplayModePropertiesKHR## Addr##
                                                              ByteArray##

instance Eq VkDisplayModePropertiesKHR where
        (VkDisplayModePropertiesKHR## a _) ==
          x@(VkDisplayModePropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayModePropertiesKHR where
        (VkDisplayModePropertiesKHR## a _) `compare`
          x@(VkDisplayModePropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayModePropertiesKHR where
        sizeOf ~_ = #{size VkDisplayModePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayModePropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayModePropertiesKHR where
        unsafeAddr (VkDisplayModePropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayModePropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayModePropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayModePropertiesKHR where
        type StructFields VkDisplayModePropertiesKHR =
             '["displayMode", "parameters"] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkDisplayMode VkDisplayModePropertiesKHR where
        type VkDisplayModeMType VkDisplayModePropertiesKHR =
             VkDisplayModeKHR

        {-# NOINLINE vkDisplayMode #-}
        vkDisplayMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModePropertiesKHR, displayMode})

        {-# INLINE vkDisplayModeByteOffset #-}
        vkDisplayModeByteOffset ~_
          = #{offset VkDisplayModePropertiesKHR, displayMode}

        {-# INLINE readVkDisplayMode #-}
        readVkDisplayMode p
          = peekByteOff p #{offset VkDisplayModePropertiesKHR, displayMode}

        {-# INLINE writeVkDisplayMode #-}
        writeVkDisplayMode p
          = pokeByteOff p #{offset VkDisplayModePropertiesKHR, displayMode}

instance {-# OVERLAPPING #-}
         HasField "displayMode" VkDisplayModePropertiesKHR where
        type FieldType "displayMode" VkDisplayModePropertiesKHR =
             VkDisplayModeKHR
        type FieldOptional "displayMode" VkDisplayModePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "displayMode" VkDisplayModePropertiesKHR =
             #{offset VkDisplayModePropertiesKHR, displayMode}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModePropertiesKHR, displayMode}

instance CanReadField "displayMode" VkDisplayModePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkDisplayMode

        {-# INLINE readField #-}
        readField = readVkDisplayMode

instance {-# OVERLAPPING #-}
         HasVkParameters VkDisplayModePropertiesKHR where
        type VkParametersMType VkDisplayModePropertiesKHR =
             VkDisplayModeParametersKHR

        {-# NOINLINE vkParameters #-}
        vkParameters x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModePropertiesKHR, parameters})

        {-# INLINE vkParametersByteOffset #-}
        vkParametersByteOffset ~_
          = #{offset VkDisplayModePropertiesKHR, parameters}

        {-# INLINE readVkParameters #-}
        readVkParameters p
          = peekByteOff p #{offset VkDisplayModePropertiesKHR, parameters}

        {-# INLINE writeVkParameters #-}
        writeVkParameters p
          = pokeByteOff p #{offset VkDisplayModePropertiesKHR, parameters}

instance {-# OVERLAPPING #-}
         HasField "parameters" VkDisplayModePropertiesKHR where
        type FieldType "parameters" VkDisplayModePropertiesKHR =
             VkDisplayModeParametersKHR
        type FieldOptional "parameters" VkDisplayModePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "parameters" VkDisplayModePropertiesKHR =
             #{offset VkDisplayModePropertiesKHR, parameters}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModePropertiesKHR, parameters}

instance CanReadField "parameters" VkDisplayModePropertiesKHR where
        {-# INLINE getField #-}
        getField = vkParameters

        {-# INLINE readField #-}
        readField = readVkParameters

instance Show VkDisplayModePropertiesKHR where
        showsPrec d x
          = showString "VkDisplayModePropertiesKHR {" .
              showString "vkDisplayMode = " .
                showsPrec d (vkDisplayMode x) .
                  showString ", " .
                    showString "vkParameters = " .
                      showsPrec d (vkParameters x) . showChar '}'

-- | > typedef struct VkDisplayModeCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplayModeCreateFlagsKHR      flags;
--   >     VkDisplayModeParametersKHR       parameters;
--   > } VkDisplayModeCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayModeCreateInfoKHR.html VkDisplayModeCreateInfoKHR registry at www.khronos.org>
data VkDisplayModeCreateInfoKHR = VkDisplayModeCreateInfoKHR## Addr##
                                                              ByteArray##

instance Eq VkDisplayModeCreateInfoKHR where
        (VkDisplayModeCreateInfoKHR## a _) ==
          x@(VkDisplayModeCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayModeCreateInfoKHR where
        (VkDisplayModeCreateInfoKHR## a _) `compare`
          x@(VkDisplayModeCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayModeCreateInfoKHR where
        sizeOf ~_ = #{size VkDisplayModeCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayModeCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayModeCreateInfoKHR where
        unsafeAddr (VkDisplayModeCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayModeCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayModeCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayModeCreateInfoKHR where
        type StructFields VkDisplayModeCreateInfoKHR =
             '["sType", "pNext", "flags", "parameters"] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkDisplayModeCreateInfoKHR
         where
        type VkSTypeMType VkDisplayModeCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplayModeCreateInfoKHR where
        type FieldType "sType" VkDisplayModeCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayModeCreateInfoKHR =
             #{offset VkDisplayModeCreateInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeCreateInfoKHR, sType}

instance CanReadField "sType" VkDisplayModeCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDisplayModeCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDisplayModeCreateInfoKHR
         where
        type VkPNextMType VkDisplayModeCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplayModeCreateInfoKHR where
        type FieldType "pNext" VkDisplayModeCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayModeCreateInfoKHR =
             #{offset VkDisplayModeCreateInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeCreateInfoKHR, pNext}

instance CanReadField "pNext" VkDisplayModeCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDisplayModeCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkDisplayModeCreateInfoKHR
         where
        type VkFlagsMType VkDisplayModeCreateInfoKHR =
             VkDisplayModeCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDisplayModeCreateInfoKHR where
        type FieldType "flags" VkDisplayModeCreateInfoKHR =
             VkDisplayModeCreateFlagsKHR
        type FieldOptional "flags" VkDisplayModeCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDisplayModeCreateInfoKHR =
             #{offset VkDisplayModeCreateInfoKHR, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeCreateInfoKHR, flags}

instance CanReadField "flags" VkDisplayModeCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkDisplayModeCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkParameters VkDisplayModeCreateInfoKHR where
        type VkParametersMType VkDisplayModeCreateInfoKHR =
             VkDisplayModeParametersKHR

        {-# NOINLINE vkParameters #-}
        vkParameters x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayModeCreateInfoKHR, parameters})

        {-# INLINE vkParametersByteOffset #-}
        vkParametersByteOffset ~_
          = #{offset VkDisplayModeCreateInfoKHR, parameters}

        {-# INLINE readVkParameters #-}
        readVkParameters p
          = peekByteOff p #{offset VkDisplayModeCreateInfoKHR, parameters}

        {-# INLINE writeVkParameters #-}
        writeVkParameters p
          = pokeByteOff p #{offset VkDisplayModeCreateInfoKHR, parameters}

instance {-# OVERLAPPING #-}
         HasField "parameters" VkDisplayModeCreateInfoKHR where
        type FieldType "parameters" VkDisplayModeCreateInfoKHR =
             VkDisplayModeParametersKHR
        type FieldOptional "parameters" VkDisplayModeCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "parameters" VkDisplayModeCreateInfoKHR =
             #{offset VkDisplayModeCreateInfoKHR, parameters}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayModeCreateInfoKHR, parameters}

instance CanReadField "parameters" VkDisplayModeCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkParameters

        {-# INLINE readField #-}
        readField = readVkParameters

instance CanWriteField "parameters" VkDisplayModeCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkParameters

instance Show VkDisplayModeCreateInfoKHR where
        showsPrec d x
          = showString "VkDisplayModeCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkParameters = " .
                                  showsPrec d (vkParameters x) . showChar '}'

-- | > typedef struct VkDisplayPlaneCapabilitiesKHR {
--   >     VkDisplayPlaneAlphaFlagsKHR      supportedAlpha;
--   >     VkOffset2D                       minSrcPosition;
--   >     VkOffset2D                       maxSrcPosition;
--   >     VkExtent2D                       minSrcExtent;
--   >     VkExtent2D                       maxSrcExtent;
--   >     VkOffset2D                       minDstPosition;
--   >     VkOffset2D                       maxDstPosition;
--   >     VkExtent2D                       minDstExtent;
--   >     VkExtent2D                       maxDstExtent;
--   > } VkDisplayPlaneCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayPlaneCapabilitiesKHR.html VkDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
data VkDisplayPlaneCapabilitiesKHR = VkDisplayPlaneCapabilitiesKHR## Addr##
                                                                    ByteArray##

instance Eq VkDisplayPlaneCapabilitiesKHR where
        (VkDisplayPlaneCapabilitiesKHR## a _) ==
          x@(VkDisplayPlaneCapabilitiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPlaneCapabilitiesKHR where
        (VkDisplayPlaneCapabilitiesKHR## a _) `compare`
          x@(VkDisplayPlaneCapabilitiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPlaneCapabilitiesKHR where
        sizeOf ~_ = #{size VkDisplayPlaneCapabilitiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDisplayPlaneCapabilitiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPlaneCapabilitiesKHR where
        unsafeAddr (VkDisplayPlaneCapabilitiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPlaneCapabilitiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPlaneCapabilitiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPlaneCapabilitiesKHR where
        type StructFields VkDisplayPlaneCapabilitiesKHR =
             '["supportedAlpha", "minSrcPosition", "maxSrcPosition", -- ' closing tick for hsc2hs
               "minSrcExtent", "maxSrcExtent", "minDstPosition", "maxDstPosition",
               "minDstExtent", "maxDstExtent"]

instance {-# OVERLAPPING #-}
         HasVkSupportedAlpha VkDisplayPlaneCapabilitiesKHR where
        type VkSupportedAlphaMType VkDisplayPlaneCapabilitiesKHR =
             VkDisplayPlaneAlphaFlagsKHR

        {-# NOINLINE vkSupportedAlpha #-}
        vkSupportedAlpha x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha})

        {-# INLINE vkSupportedAlphaByteOffset #-}
        vkSupportedAlphaByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

        {-# INLINE readVkSupportedAlpha #-}
        readVkSupportedAlpha p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

        {-# INLINE writeVkSupportedAlpha #-}
        writeVkSupportedAlpha p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

instance {-# OVERLAPPING #-}
         HasField "supportedAlpha" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             VkDisplayPlaneAlphaFlagsKHR
        type FieldOptional "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedAlpha" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, supportedAlpha}

instance CanReadField "supportedAlpha"
           VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSupportedAlpha

        {-# INLINE readField #-}
        readField = readVkSupportedAlpha

instance {-# OVERLAPPING #-}
         HasVkMinSrcPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMinSrcPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMinSrcPosition #-}
        vkMinSrcPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition})

        {-# INLINE vkMinSrcPositionByteOffset #-}
        vkMinSrcPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

        {-# INLINE readVkMinSrcPosition #-}
        readVkMinSrcPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

        {-# INLINE writeVkMinSrcPosition #-}
        writeVkMinSrcPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

instance {-# OVERLAPPING #-}
         HasField "minSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcPosition}

instance CanReadField "minSrcPosition"
           VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMinSrcPosition

        {-# INLINE readField #-}
        readField = readVkMinSrcPosition

instance {-# OVERLAPPING #-}
         HasVkMaxSrcPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMaxSrcPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMaxSrcPosition #-}
        vkMaxSrcPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition})

        {-# INLINE vkMaxSrcPositionByteOffset #-}
        vkMaxSrcPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

        {-# INLINE readVkMaxSrcPosition #-}
        readVkMaxSrcPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

        {-# INLINE writeVkMaxSrcPosition #-}
        writeVkMaxSrcPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

instance {-# OVERLAPPING #-}
         HasField "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSrcPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcPosition}

instance CanReadField "maxSrcPosition"
           VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxSrcPosition

        {-# INLINE readField #-}
        readField = readVkMaxSrcPosition

instance {-# OVERLAPPING #-}
         HasVkMinSrcExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMinSrcExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMinSrcExtent #-}
        vkMinSrcExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent})

        {-# INLINE vkMinSrcExtentByteOffset #-}
        vkMinSrcExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

        {-# INLINE readVkMinSrcExtent #-}
        readVkMinSrcExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

        {-# INLINE writeVkMinSrcExtent #-}
        writeVkMinSrcExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

instance {-# OVERLAPPING #-}
         HasField "minSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minSrcExtent}

instance CanReadField "minSrcExtent" VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMinSrcExtent

        {-# INLINE readField #-}
        readField = readVkMinSrcExtent

instance {-# OVERLAPPING #-}
         HasVkMaxSrcExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMaxSrcExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMaxSrcExtent #-}
        vkMaxSrcExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent})

        {-# INLINE vkMaxSrcExtentByteOffset #-}
        vkMaxSrcExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

        {-# INLINE readVkMaxSrcExtent #-}
        readVkMaxSrcExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

        {-# INLINE writeVkMaxSrcExtent #-}
        writeVkMaxSrcExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

instance {-# OVERLAPPING #-}
         HasField "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxSrcExtent}

instance CanReadField "maxSrcExtent" VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxSrcExtent

        {-# INLINE readField #-}
        readField = readVkMaxSrcExtent

instance {-# OVERLAPPING #-}
         HasVkMinDstPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMinDstPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMinDstPosition #-}
        vkMinDstPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition})

        {-# INLINE vkMinDstPositionByteOffset #-}
        vkMinDstPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

        {-# INLINE readVkMinDstPosition #-}
        readVkMinDstPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

        {-# INLINE writeVkMinDstPosition #-}
        writeVkMinDstPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

instance {-# OVERLAPPING #-}
         HasField "minDstPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minDstPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstPosition}

instance CanReadField "minDstPosition"
           VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMinDstPosition

        {-# INLINE readField #-}
        readField = readVkMinDstPosition

instance {-# OVERLAPPING #-}
         HasVkMaxDstPosition VkDisplayPlaneCapabilitiesKHR where
        type VkMaxDstPositionMType VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D

        {-# NOINLINE vkMaxDstPosition #-}
        vkMaxDstPosition x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition})

        {-# INLINE vkMaxDstPositionByteOffset #-}
        vkMaxDstPositionByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

        {-# INLINE readVkMaxDstPosition #-}
        readVkMaxDstPosition p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

        {-# INLINE writeVkMaxDstPosition #-}
        writeVkMaxDstPosition p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

instance {-# OVERLAPPING #-}
         HasField "maxDstPosition" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             VkOffset2D
        type FieldOptional "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDstPosition" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstPosition}

instance CanReadField "maxDstPosition"
           VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxDstPosition

        {-# INLINE readField #-}
        readField = readVkMaxDstPosition

instance {-# OVERLAPPING #-}
         HasVkMinDstExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMinDstExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMinDstExtent #-}
        vkMinDstExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent})

        {-# INLINE vkMinDstExtentByteOffset #-}
        vkMinDstExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

        {-# INLINE readVkMinDstExtent #-}
        readVkMinDstExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

        {-# INLINE writeVkMinDstExtent #-}
        writeVkMinDstExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

instance {-# OVERLAPPING #-}
         HasField "minDstExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minDstExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, minDstExtent}

instance CanReadField "minDstExtent" VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMinDstExtent

        {-# INLINE readField #-}
        readField = readVkMinDstExtent

instance {-# OVERLAPPING #-}
         HasVkMaxDstExtent VkDisplayPlaneCapabilitiesKHR where
        type VkMaxDstExtentMType VkDisplayPlaneCapabilitiesKHR = VkExtent2D

        {-# NOINLINE vkMaxDstExtent #-}
        vkMaxDstExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent})

        {-# INLINE vkMaxDstExtentByteOffset #-}
        vkMaxDstExtentByteOffset ~_
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

        {-# INLINE readVkMaxDstExtent #-}
        readVkMaxDstExtent p
          = peekByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

        {-# INLINE writeVkMaxDstExtent #-}
        writeVkMaxDstExtent p
          = pokeByteOff p #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

instance {-# OVERLAPPING #-}
         HasField "maxDstExtent" VkDisplayPlaneCapabilitiesKHR where
        type FieldType "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDstExtent" VkDisplayPlaneCapabilitiesKHR =
             #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlaneCapabilitiesKHR, maxDstExtent}

instance CanReadField "maxDstExtent" VkDisplayPlaneCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkMaxDstExtent

        {-# INLINE readField #-}
        readField = readVkMaxDstExtent

instance Show VkDisplayPlaneCapabilitiesKHR where
        showsPrec d x
          = showString "VkDisplayPlaneCapabilitiesKHR {" .
              showString "vkSupportedAlpha = " .
                showsPrec d (vkSupportedAlpha x) .
                  showString ", " .
                    showString "vkMinSrcPosition = " .
                      showsPrec d (vkMinSrcPosition x) .
                        showString ", " .
                          showString "vkMaxSrcPosition = " .
                            showsPrec d (vkMaxSrcPosition x) .
                              showString ", " .
                                showString "vkMinSrcExtent = " .
                                  showsPrec d (vkMinSrcExtent x) .
                                    showString ", " .
                                      showString "vkMaxSrcExtent = " .
                                        showsPrec d (vkMaxSrcExtent x) .
                                          showString ", " .
                                            showString "vkMinDstPosition = " .
                                              showsPrec d (vkMinDstPosition x) .
                                                showString ", " .
                                                  showString "vkMaxDstPosition = " .
                                                    showsPrec d (vkMaxDstPosition x) .
                                                      showString ", " .
                                                        showString "vkMinDstExtent = " .
                                                          showsPrec d (vkMinDstExtent x) .
                                                            showString ", " .
                                                              showString "vkMaxDstExtent = " .
                                                                showsPrec d (vkMaxDstExtent x) .
                                                                  showChar '}'

-- | > typedef struct VkDisplayPlanePropertiesKHR {
--   >     VkDisplayKHR                     currentDisplay;
--   >     uint32_t                         currentStackIndex;
--   > } VkDisplayPlanePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayPlanePropertiesKHR.html VkDisplayPlanePropertiesKHR registry at www.khronos.org>
data VkDisplayPlanePropertiesKHR = VkDisplayPlanePropertiesKHR## Addr##
                                                                ByteArray##

instance Eq VkDisplayPlanePropertiesKHR where
        (VkDisplayPlanePropertiesKHR## a _) ==
          x@(VkDisplayPlanePropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPlanePropertiesKHR where
        (VkDisplayPlanePropertiesKHR## a _) `compare`
          x@(VkDisplayPlanePropertiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPlanePropertiesKHR where
        sizeOf ~_ = #{size VkDisplayPlanePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPlanePropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPlanePropertiesKHR where
        unsafeAddr (VkDisplayPlanePropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPlanePropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPlanePropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPlanePropertiesKHR where
        type StructFields VkDisplayPlanePropertiesKHR =
             '["currentDisplay", "currentStackIndex"] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkCurrentDisplay VkDisplayPlanePropertiesKHR where
        type VkCurrentDisplayMType VkDisplayPlanePropertiesKHR =
             VkDisplayKHR

        {-# NOINLINE vkCurrentDisplay #-}
        vkCurrentDisplay x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlanePropertiesKHR, currentDisplay})

        {-# INLINE vkCurrentDisplayByteOffset #-}
        vkCurrentDisplayByteOffset ~_
          = #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

        {-# INLINE readVkCurrentDisplay #-}
        readVkCurrentDisplay p
          = peekByteOff p #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

        {-# INLINE writeVkCurrentDisplay #-}
        writeVkCurrentDisplay p
          = pokeByteOff p #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

instance {-# OVERLAPPING #-}
         HasField "currentDisplay" VkDisplayPlanePropertiesKHR where
        type FieldType "currentDisplay" VkDisplayPlanePropertiesKHR =
             VkDisplayKHR
        type FieldOptional "currentDisplay" VkDisplayPlanePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "currentDisplay" VkDisplayPlanePropertiesKHR =
             #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlanePropertiesKHR, currentDisplay}

instance CanReadField "currentDisplay" VkDisplayPlanePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCurrentDisplay

        {-# INLINE readField #-}
        readField = readVkCurrentDisplay

instance {-# OVERLAPPING #-}
         HasVkCurrentStackIndex VkDisplayPlanePropertiesKHR where
        type VkCurrentStackIndexMType VkDisplayPlanePropertiesKHR = Word32

        {-# NOINLINE vkCurrentStackIndex #-}
        vkCurrentStackIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPlanePropertiesKHR, currentStackIndex})

        {-# INLINE vkCurrentStackIndexByteOffset #-}
        vkCurrentStackIndexByteOffset ~_
          = #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

        {-# INLINE readVkCurrentStackIndex #-}
        readVkCurrentStackIndex p
          = peekByteOff p #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

        {-# INLINE writeVkCurrentStackIndex #-}
        writeVkCurrentStackIndex p
          = pokeByteOff p #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

instance {-# OVERLAPPING #-}
         HasField "currentStackIndex" VkDisplayPlanePropertiesKHR where
        type FieldType "currentStackIndex" VkDisplayPlanePropertiesKHR =
             Word32
        type FieldOptional "currentStackIndex" VkDisplayPlanePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "currentStackIndex" VkDisplayPlanePropertiesKHR =
             #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPlanePropertiesKHR, currentStackIndex}

instance CanReadField "currentStackIndex"
           VkDisplayPlanePropertiesKHR
         where
        {-# INLINE getField #-}
        getField = vkCurrentStackIndex

        {-# INLINE readField #-}
        readField = readVkCurrentStackIndex

instance Show VkDisplayPlanePropertiesKHR where
        showsPrec d x
          = showString "VkDisplayPlanePropertiesKHR {" .
              showString "vkCurrentDisplay = " .
                showsPrec d (vkCurrentDisplay x) .
                  showString ", " .
                    showString "vkCurrentStackIndex = " .
                      showsPrec d (vkCurrentStackIndex x) . showChar '}'

-- | > typedef struct VkDisplaySurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDisplaySurfaceCreateFlagsKHR   flags;
--   >     VkDisplayModeKHR                 displayMode;
--   >     uint32_t                         planeIndex;
--   >     uint32_t                         planeStackIndex;
--   >     VkSurfaceTransformFlagBitsKHR    transform;
--   >     float                            globalAlpha;
--   >     VkDisplayPlaneAlphaFlagBitsKHR   alphaMode;
--   >     VkExtent2D                       imageExtent;
--   > } VkDisplaySurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplaySurfaceCreateInfoKHR.html VkDisplaySurfaceCreateInfoKHR registry at www.khronos.org>
data VkDisplaySurfaceCreateInfoKHR = VkDisplaySurfaceCreateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkDisplaySurfaceCreateInfoKHR where
        (VkDisplaySurfaceCreateInfoKHR## a _) ==
          x@(VkDisplaySurfaceCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplaySurfaceCreateInfoKHR where
        (VkDisplaySurfaceCreateInfoKHR## a _) `compare`
          x@(VkDisplaySurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplaySurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkDisplaySurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDisplaySurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplaySurfaceCreateInfoKHR where
        unsafeAddr (VkDisplaySurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplaySurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplaySurfaceCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplaySurfaceCreateInfoKHR where
        type StructFields VkDisplaySurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "displayMode", "planeIndex", -- ' closing tick for hsc2hs
               "planeStackIndex", "transform", "globalAlpha", "alphaMode",
               "imageExtent"]

instance {-# OVERLAPPING #-}
         HasVkSType VkDisplaySurfaceCreateInfoKHR where
        type VkSTypeMType VkDisplaySurfaceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "sType" VkDisplaySurfaceCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, sType}

instance CanReadField "sType" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDisplaySurfaceCreateInfoKHR where
        type VkPNextMType VkDisplaySurfaceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "pNext" VkDisplaySurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

instance CanReadField "pNext" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkDisplaySurfaceCreateInfoKHR where
        type VkFlagsMType VkDisplaySurfaceCreateInfoKHR =
             VkDisplaySurfaceCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "flags" VkDisplaySurfaceCreateInfoKHR =
             VkDisplaySurfaceCreateFlagsKHR
        type FieldOptional "flags" VkDisplaySurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, flags}

instance CanReadField "flags" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkDisplayMode VkDisplaySurfaceCreateInfoKHR where
        type VkDisplayModeMType VkDisplaySurfaceCreateInfoKHR =
             VkDisplayModeKHR

        {-# NOINLINE vkDisplayMode #-}
        vkDisplayMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, displayMode})

        {-# INLINE vkDisplayModeByteOffset #-}
        vkDisplayModeByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

        {-# INLINE readVkDisplayMode #-}
        readVkDisplayMode p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

        {-# INLINE writeVkDisplayMode #-}
        writeVkDisplayMode p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

instance {-# OVERLAPPING #-}
         HasField "displayMode" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "displayMode" VkDisplaySurfaceCreateInfoKHR =
             VkDisplayModeKHR
        type FieldOptional "displayMode" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "displayMode" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

instance CanReadField "displayMode" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkDisplayMode

        {-# INLINE readField #-}
        readField = readVkDisplayMode

instance CanWriteField "displayMode" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDisplayMode

instance {-# OVERLAPPING #-}
         HasVkPlaneIndex VkDisplaySurfaceCreateInfoKHR where
        type VkPlaneIndexMType VkDisplaySurfaceCreateInfoKHR = Word32

        {-# NOINLINE vkPlaneIndex #-}
        vkPlaneIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex})

        {-# INLINE vkPlaneIndexByteOffset #-}
        vkPlaneIndexByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

        {-# INLINE readVkPlaneIndex #-}
        readVkPlaneIndex p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

        {-# INLINE writeVkPlaneIndex #-}
        writeVkPlaneIndex p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

instance {-# OVERLAPPING #-}
         HasField "planeIndex" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "planeIndex" VkDisplaySurfaceCreateInfoKHR = Word32
        type FieldOptional "planeIndex" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "planeIndex" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

instance CanReadField "planeIndex" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPlaneIndex

        {-# INLINE readField #-}
        readField = readVkPlaneIndex

instance CanWriteField "planeIndex" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPlaneIndex

instance {-# OVERLAPPING #-}
         HasVkPlaneStackIndex VkDisplaySurfaceCreateInfoKHR where
        type VkPlaneStackIndexMType VkDisplaySurfaceCreateInfoKHR = Word32

        {-# NOINLINE vkPlaneStackIndex #-}
        vkPlaneStackIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex})

        {-# INLINE vkPlaneStackIndexByteOffset #-}
        vkPlaneStackIndexByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

        {-# INLINE readVkPlaneStackIndex #-}
        readVkPlaneStackIndex p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

        {-# INLINE writeVkPlaneStackIndex #-}
        writeVkPlaneStackIndex p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

instance {-# OVERLAPPING #-}
         HasField "planeStackIndex" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "planeStackIndex" VkDisplaySurfaceCreateInfoKHR =
             Word32
        type FieldOptional "planeStackIndex" VkDisplaySurfaceCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "planeStackIndex" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

instance CanReadField "planeStackIndex"
           VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPlaneStackIndex

        {-# INLINE readField #-}
        readField = readVkPlaneStackIndex

instance CanWriteField "planeStackIndex"
           VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPlaneStackIndex

instance {-# OVERLAPPING #-}
         HasVkTransform VkDisplaySurfaceCreateInfoKHR where
        type VkTransformMType VkDisplaySurfaceCreateInfoKHR =
             VkSurfaceTransformFlagBitsKHR

        {-# NOINLINE vkTransform #-}
        vkTransform x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, transform})

        {-# INLINE vkTransformByteOffset #-}
        vkTransformByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, transform}

        {-# INLINE readVkTransform #-}
        readVkTransform p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, transform}

        {-# INLINE writeVkTransform #-}
        writeVkTransform p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, transform}

instance {-# OVERLAPPING #-}
         HasField "transform" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "transform" VkDisplaySurfaceCreateInfoKHR =
             VkSurfaceTransformFlagBitsKHR
        type FieldOptional "transform" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "transform" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, transform}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, transform}

instance CanReadField "transform" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkTransform

        {-# INLINE readField #-}
        readField = readVkTransform

instance CanWriteField "transform" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkTransform

instance {-# OVERLAPPING #-}
         HasVkGlobalAlpha VkDisplaySurfaceCreateInfoKHR where
        type VkGlobalAlphaMType VkDisplaySurfaceCreateInfoKHR =
             #{type float}

        {-# NOINLINE vkGlobalAlpha #-}
        vkGlobalAlpha x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha})

        {-# INLINE vkGlobalAlphaByteOffset #-}
        vkGlobalAlphaByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

        {-# INLINE readVkGlobalAlpha #-}
        readVkGlobalAlpha p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

        {-# INLINE writeVkGlobalAlpha #-}
        writeVkGlobalAlpha p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

instance {-# OVERLAPPING #-}
         HasField "globalAlpha" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "globalAlpha" VkDisplaySurfaceCreateInfoKHR =
             #{type float}
        type FieldOptional "globalAlpha" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "globalAlpha" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

instance CanReadField "globalAlpha" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkGlobalAlpha

        {-# INLINE readField #-}
        readField = readVkGlobalAlpha

instance CanWriteField "globalAlpha" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkGlobalAlpha

instance {-# OVERLAPPING #-}
         HasVkAlphaMode VkDisplaySurfaceCreateInfoKHR where
        type VkAlphaModeMType VkDisplaySurfaceCreateInfoKHR =
             VkDisplayPlaneAlphaFlagBitsKHR

        {-# NOINLINE vkAlphaMode #-}
        vkAlphaMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode})

        {-# INLINE vkAlphaModeByteOffset #-}
        vkAlphaModeByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

        {-# INLINE readVkAlphaMode #-}
        readVkAlphaMode p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

        {-# INLINE writeVkAlphaMode #-}
        writeVkAlphaMode p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

instance {-# OVERLAPPING #-}
         HasField "alphaMode" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "alphaMode" VkDisplaySurfaceCreateInfoKHR =
             VkDisplayPlaneAlphaFlagBitsKHR
        type FieldOptional "alphaMode" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "alphaMode" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

instance CanReadField "alphaMode" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkAlphaMode

        {-# INLINE readField #-}
        readField = readVkAlphaMode

instance CanWriteField "alphaMode" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkAlphaMode

instance {-# OVERLAPPING #-}
         HasVkImageExtent VkDisplaySurfaceCreateInfoKHR where
        type VkImageExtentMType VkDisplaySurfaceCreateInfoKHR = VkExtent2D

        {-# NOINLINE vkImageExtent #-}
        vkImageExtent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent})

        {-# INLINE vkImageExtentByteOffset #-}
        vkImageExtentByteOffset ~_
          = #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

        {-# INLINE readVkImageExtent #-}
        readVkImageExtent p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

        {-# INLINE writeVkImageExtent #-}
        writeVkImageExtent p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

instance {-# OVERLAPPING #-}
         HasField "imageExtent" VkDisplaySurfaceCreateInfoKHR where
        type FieldType "imageExtent" VkDisplaySurfaceCreateInfoKHR =
             VkExtent2D
        type FieldOptional "imageExtent" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "imageExtent" VkDisplaySurfaceCreateInfoKHR =
             #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

instance CanReadField "imageExtent" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkImageExtent

        {-# INLINE readField #-}
        readField = readVkImageExtent

instance CanWriteField "imageExtent" VkDisplaySurfaceCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkImageExtent

instance Show VkDisplaySurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkDisplaySurfaceCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkDisplayMode = " .
                                  showsPrec d (vkDisplayMode x) .
                                    showString ", " .
                                      showString "vkPlaneIndex = " .
                                        showsPrec d (vkPlaneIndex x) .
                                          showString ", " .
                                            showString "vkPlaneStackIndex = " .
                                              showsPrec d (vkPlaneStackIndex x) .
                                                showString ", " .
                                                  showString "vkTransform = " .
                                                    showsPrec d (vkTransform x) .
                                                      showString ", " .
                                                        showString "vkGlobalAlpha = " .
                                                          showsPrec d (vkGlobalAlpha x) .
                                                            showString ", " .
                                                              showString "vkAlphaMode = " .
                                                                showsPrec d (vkAlphaMode x) .
                                                                  showString ", " .
                                                                    showString "vkImageExtent = " .
                                                                      showsPrec d (vkImageExtent x)
                                                                        . showChar '}'

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceDisplayPropertiesKHR.html vkGetPhysicalDeviceDisplayPropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceDisplayPropertiesKHR"
               vkGetPhysicalDeviceDisplayPropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkDisplayPropertiesKHR -- ^ pProperties
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetPhysicalDeviceDisplayPlanePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayPlanePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceDisplayPlanePropertiesKHR.html vkGetPhysicalDeviceDisplayPlanePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
               vkGetPhysicalDeviceDisplayPlanePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pPropertyCount
                            -> Ptr VkDisplayPlanePropertiesKHR -- ^ pProperties
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneSupportedDisplaysKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t planeIndex
--   >     , uint32_t* pDisplayCount
--   >     , VkDisplayKHR* pDisplays
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDisplayPlaneSupportedDisplaysKHR.html vkGetDisplayPlaneSupportedDisplaysKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayPlaneSupportedDisplaysKHR"
               vkGetDisplayPlaneSupportedDisplaysKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ planeIndex
                        -> Ptr Word32 -- ^ pDisplayCount
                                      -> Ptr VkDisplayKHR -- ^ pDisplays
                                                          -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayModePropertiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , uint32_t* pPropertyCount
--   >     , VkDisplayModePropertiesKHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDisplayModePropertiesKHR.html vkGetDisplayModePropertiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayModePropertiesKHR"
               vkGetDisplayModePropertiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkDisplayModePropertiesKHR -- ^ pProperties
                                                                -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INITIALIZATION_FAILED'.
--
--   > VkResult vkCreateDisplayModeKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayKHR display
--   >     , const VkDisplayModeCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDisplayModeKHR* pMode
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDisplayModeKHR.html vkCreateDisplayModeKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDisplayModeKHR"
               vkCreateDisplayModeKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayKHR -- ^ display
                              ->
                   Ptr VkDisplayModeCreateInfoKHR -- ^ pCreateInfo
                                                  ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkDisplayModeKHR -- ^ pMode
                                                                       -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkGetDisplayPlaneCapabilitiesKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkDisplayModeKHR mode
--   >     , uint32_t planeIndex
--   >     , VkDisplayPlaneCapabilitiesKHR* pCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetDisplayPlaneCapabilitiesKHR.html vkGetDisplayPlaneCapabilitiesKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetDisplayPlaneCapabilitiesKHR"
               vkGetDisplayPlaneCapabilitiesKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 VkDisplayModeKHR -- ^ mode
                                  ->
                   Word32 -- ^ planeIndex
                          -> Ptr VkDisplayPlaneCapabilitiesKHR -- ^ pCapabilities
                                                               -> IO VkResult

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateDisplayPlaneSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkDisplaySurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDisplayPlaneSurfaceKHR.html vkCreateDisplayPlaneSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDisplayPlaneSurfaceKHR"
               vkCreateDisplayPlaneSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkDisplaySurfaceCreateInfoKHR -- ^ pCreateInfo
                                                   ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

pattern VK_KHR_DISPLAY_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_DISPLAY_SPEC_VERSION = 21

type VK_KHR_DISPLAY_SPEC_VERSION = 21

pattern VK_KHR_DISPLAY_EXTENSION_NAME :: CString

pattern VK_KHR_DISPLAY_EXTENSION_NAME <-
        (is_VK_KHR_DISPLAY_EXTENSION_NAME -> True)
  where VK_KHR_DISPLAY_EXTENSION_NAME
          = _VK_KHR_DISPLAY_EXTENSION_NAME

{-# INLINE _VK_KHR_DISPLAY_EXTENSION_NAME #-}

_VK_KHR_DISPLAY_EXTENSION_NAME :: CString
_VK_KHR_DISPLAY_EXTENSION_NAME = Ptr "VK_KHR_display\NUL"##

{-# INLINE is_VK_KHR_DISPLAY_EXTENSION_NAME #-}

is_VK_KHR_DISPLAY_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DISPLAY_EXTENSION_NAME
  = eqCStrings _VK_KHR_DISPLAY_EXTENSION_NAME

type VK_KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"

pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR =
        VkStructureType 1000002000

pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000002001

-- | VkDisplayKHR
pattern VK_OBJECT_TYPE_DISPLAY_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_DISPLAY_KHR = VkObjectType 1000002000

-- | VkDisplayModeKHR
pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR :: VkObjectType

pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR = VkObjectType 1000002001
