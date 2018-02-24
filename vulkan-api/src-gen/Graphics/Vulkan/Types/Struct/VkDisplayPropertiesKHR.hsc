#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplayPropertiesKHR
       (VkDisplayPropertiesKHR(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                       (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR (VkSurfaceTransformFlagsKHR)
import           Graphics.Vulkan.Types.Handles                         (VkDisplayKHR)
import           Graphics.Vulkan.Types.Struct.VkExtent2D               (VkExtent2D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDisplayPropertiesKHR.html VkDisplayPropertiesKHR registry at www.khronos.org>
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
        type CUnionType VkDisplayPropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPropertiesKHR = '[] -- ' closing tick for hsc2hs

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
        type FieldIsArray "display" VkDisplayPropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPropertiesKHR, display}

instance CanReadField "display" VkDisplayPropertiesKHR where
        {-# INLINE getField #-}
        getField = vkDisplay

        {-# INLINE readField #-}
        readField = readVkDisplay

instance CanWriteField "display" VkDisplayPropertiesKHR where
        {-# INLINE writeField #-}
        writeField = writeVkDisplay

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
        type FieldIsArray "displayName" VkDisplayPropertiesKHR = 'False -- ' closing tick for hsc2hs

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

instance CanWriteField "displayName" VkDisplayPropertiesKHR where
        {-# INLINE writeField #-}
        writeField = writeVkDisplayName

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
        type FieldIsArray "physicalDimensions" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs

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

instance CanWriteField "physicalDimensions" VkDisplayPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPhysicalDimensions

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
        type FieldIsArray "physicalResolution" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs

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

instance CanWriteField "physicalResolution" VkDisplayPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPhysicalResolution

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
        type FieldIsArray "supportedTransforms" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs

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

instance CanWriteField "supportedTransforms" VkDisplayPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSupportedTransforms

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
        type FieldIsArray "planeReorderPossible" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs

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

instance CanWriteField "planeReorderPossible"
           VkDisplayPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPlaneReorderPossible

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
        type FieldIsArray "persistentContent" VkDisplayPropertiesKHR =
             'False -- ' closing tick for hsc2hs

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

instance CanWriteField "persistentContent" VkDisplayPropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPersistentContent

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
