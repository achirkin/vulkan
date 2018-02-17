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
module Graphics.Vulkan.Ext.VK_KHR_get_surface_capabilities2
       (-- * Vulkan extension: @VK_KHR_get_surface_capabilities2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @120@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkPhysicalDeviceSurfaceInfo2KHR(..),
        VkSurfaceCapabilities2KHR(..), VkSurfaceFormat2KHR(..),
        vkGetPhysicalDeviceSurfaceCapabilities2KHR,
        vkGetPhysicalDeviceSurfaceFormats2KHR,
        VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION,
        pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION,
        VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME,
        pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkSurfaceCapabilitiesKHR,
                                                   VkSurfaceFormatKHR)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSurfaceInfo2KHR {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkSurfaceKHR surface;
--   > } VkPhysicalDeviceSurfaceInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceSurfaceInfo2KHR.html VkPhysicalDeviceSurfaceInfo2KHR registry at www.khronos.org>
data VkPhysicalDeviceSurfaceInfo2KHR = VkPhysicalDeviceSurfaceInfo2KHR## Addr##
                                                                        ByteArray##

instance Eq VkPhysicalDeviceSurfaceInfo2KHR where
        (VkPhysicalDeviceSurfaceInfo2KHR## a _) ==
          x@(VkPhysicalDeviceSurfaceInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSurfaceInfo2KHR where
        (VkPhysicalDeviceSurfaceInfo2KHR## a _) `compare`
          x@(VkPhysicalDeviceSurfaceInfo2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSurfaceInfo2KHR where
        sizeOf ~_ = #{size VkPhysicalDeviceSurfaceInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSurfaceInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceSurfaceInfo2KHR where
        unsafeAddr (VkPhysicalDeviceSurfaceInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceSurfaceInfo2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceSurfaceInfo2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceSurfaceInfo2KHR where
        type StructFields VkPhysicalDeviceSurfaceInfo2KHR =
             '["sType", "pNext", "surface"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceSurfaceInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceSurfaceInfo2KHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceSurfaceInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceSurfaceInfo2KHR where
        type VkSTypeMType VkPhysicalDeviceSurfaceInfo2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceSurfaceInfo2KHR where
        type FieldType "sType" VkPhysicalDeviceSurfaceInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceSurfaceInfo2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceSurfaceInfo2KHR =
             #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSurfaceInfo2KHR, sType}

instance CanReadField "sType" VkPhysicalDeviceSurfaceInfo2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPhysicalDeviceSurfaceInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceSurfaceInfo2KHR where
        type VkPNextMType VkPhysicalDeviceSurfaceInfo2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceSurfaceInfo2KHR where
        type FieldType "pNext" VkPhysicalDeviceSurfaceInfo2KHR = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceSurfaceInfo2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceSurfaceInfo2KHR =
             #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSurfaceInfo2KHR, pNext}

instance CanReadField "pNext" VkPhysicalDeviceSurfaceInfo2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPhysicalDeviceSurfaceInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSurface VkPhysicalDeviceSurfaceInfo2KHR where
        type VkSurfaceMType VkPhysicalDeviceSurfaceInfo2KHR = VkSurfaceKHR

        {-# NOINLINE vkSurface #-}
        vkSurface x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface})

        {-# INLINE vkSurfaceByteOffset #-}
        vkSurfaceByteOffset ~_
          = #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface}

        {-# INLINE readVkSurface #-}
        readVkSurface p
          = peekByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface}

        {-# INLINE writeVkSurface #-}
        writeVkSurface p
          = pokeByteOff p #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface}

instance {-# OVERLAPPING #-}
         HasField "surface" VkPhysicalDeviceSurfaceInfo2KHR where
        type FieldType "surface" VkPhysicalDeviceSurfaceInfo2KHR =
             VkSurfaceKHR
        type FieldOptional "surface" VkPhysicalDeviceSurfaceInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "surface" VkPhysicalDeviceSurfaceInfo2KHR =
             #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSurfaceInfo2KHR, surface}

instance CanReadField "surface" VkPhysicalDeviceSurfaceInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSurface

        {-# INLINE readField #-}
        readField = readVkSurface

instance CanWriteField "surface" VkPhysicalDeviceSurfaceInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSurface

instance Show VkPhysicalDeviceSurfaceInfo2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceSurfaceInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSurface = " .
                            showsPrec d (vkSurface x) . showChar '}'

-- | > typedef struct VkSurfaceCapabilities2KHR {
--   >     VkStructureType sType;
--   >     void*   pNext;
--   >     VkSurfaceCapabilitiesKHR surfaceCapabilities;
--   > } VkSurfaceCapabilities2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSurfaceCapabilities2KHR.html VkSurfaceCapabilities2KHR registry at www.khronos.org>
data VkSurfaceCapabilities2KHR = VkSurfaceCapabilities2KHR## Addr##
                                                            ByteArray##

instance Eq VkSurfaceCapabilities2KHR where
        (VkSurfaceCapabilities2KHR## a _) ==
          x@(VkSurfaceCapabilities2KHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceCapabilities2KHR where
        (VkSurfaceCapabilities2KHR## a _) `compare`
          x@(VkSurfaceCapabilities2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceCapabilities2KHR where
        sizeOf ~_ = #{size VkSurfaceCapabilities2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceCapabilities2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceCapabilities2KHR where
        unsafeAddr (VkSurfaceCapabilities2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceCapabilities2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceCapabilities2KHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceCapabilities2KHR where
        type StructFields VkSurfaceCapabilities2KHR =
             '["sType", "pNext", "surfaceCapabilities"] -- ' closing tick for hsc2hs
        type CUnionType VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceCapabilities2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceCapabilities2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkSurfaceCapabilities2KHR
         where
        type VkSTypeMType VkSurfaceCapabilities2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSurfaceCapabilities2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSurfaceCapabilities2KHR where
        type FieldType "sType" VkSurfaceCapabilities2KHR = VkStructureType
        type FieldOptional "sType" VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSurfaceCapabilities2KHR =
             #{offset VkSurfaceCapabilities2KHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2KHR, sType}

instance CanReadField "sType" VkSurfaceCapabilities2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSurfaceCapabilities2KHR
         where
        type VkPNextMType VkSurfaceCapabilities2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSurfaceCapabilities2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSurfaceCapabilities2KHR where
        type FieldType "pNext" VkSurfaceCapabilities2KHR = Ptr Void
        type FieldOptional "pNext" VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSurfaceCapabilities2KHR =
             #{offset VkSurfaceCapabilities2KHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2KHR, pNext}

instance CanReadField "pNext" VkSurfaceCapabilities2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkSurfaceCapabilities VkSurfaceCapabilities2KHR where
        type VkSurfaceCapabilitiesMType VkSurfaceCapabilities2KHR =
             VkSurfaceCapabilitiesKHR

        {-# NOINLINE vkSurfaceCapabilities #-}
        vkSurfaceCapabilities x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities})

        {-# INLINE vkSurfaceCapabilitiesByteOffset #-}
        vkSurfaceCapabilitiesByteOffset ~_
          = #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

        {-# INLINE readVkSurfaceCapabilities #-}
        readVkSurfaceCapabilities p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

        {-# INLINE writeVkSurfaceCapabilities #-}
        writeVkSurfaceCapabilities p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

instance {-# OVERLAPPING #-}
         HasField "surfaceCapabilities" VkSurfaceCapabilities2KHR where
        type FieldType "surfaceCapabilities" VkSurfaceCapabilities2KHR =
             VkSurfaceCapabilitiesKHR
        type FieldOptional "surfaceCapabilities" VkSurfaceCapabilities2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "surfaceCapabilities" VkSurfaceCapabilities2KHR =
             #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

instance CanReadField "surfaceCapabilities"
           VkSurfaceCapabilities2KHR
         where
        {-# INLINE getField #-}
        getField = vkSurfaceCapabilities

        {-# INLINE readField #-}
        readField = readVkSurfaceCapabilities

instance Show VkSurfaceCapabilities2KHR where
        showsPrec d x
          = showString "VkSurfaceCapabilities2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSurfaceCapabilities = " .
                            showsPrec d (vkSurfaceCapabilities x) . showChar '}'

-- | > typedef struct VkSurfaceFormat2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkSurfaceFormatKHR surfaceFormat;
--   > } VkSurfaceFormat2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSurfaceFormat2KHR.html VkSurfaceFormat2KHR registry at www.khronos.org>
data VkSurfaceFormat2KHR = VkSurfaceFormat2KHR## Addr## ByteArray##

instance Eq VkSurfaceFormat2KHR where
        (VkSurfaceFormat2KHR## a _) == x@(VkSurfaceFormat2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceFormat2KHR where
        (VkSurfaceFormat2KHR## a _) `compare` x@(VkSurfaceFormat2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceFormat2KHR where
        sizeOf ~_ = #{size VkSurfaceFormat2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceFormat2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceFormat2KHR where
        unsafeAddr (VkSurfaceFormat2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceFormat2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceFormat2KHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceFormat2KHR where
        type StructFields VkSurfaceFormat2KHR =
             '["sType", "pNext", "surfaceFormat"] -- ' closing tick for hsc2hs
        type CUnionType VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceFormat2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceFormat2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkSurfaceFormat2KHR where
        type VkSTypeMType VkSurfaceFormat2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSurfaceFormat2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkSurfaceFormat2KHR
         where
        type FieldType "sType" VkSurfaceFormat2KHR = VkStructureType
        type FieldOptional "sType" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSurfaceFormat2KHR =
             #{offset VkSurfaceFormat2KHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSurfaceFormat2KHR, sType}

instance CanReadField "sType" VkSurfaceFormat2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSurfaceFormat2KHR where
        type VkPNextMType VkSurfaceFormat2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSurfaceFormat2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkSurfaceFormat2KHR
         where
        type FieldType "pNext" VkSurfaceFormat2KHR = Ptr Void
        type FieldOptional "pNext" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSurfaceFormat2KHR =
             #{offset VkSurfaceFormat2KHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSurfaceFormat2KHR, pNext}

instance CanReadField "pNext" VkSurfaceFormat2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-} HasVkSurfaceFormat VkSurfaceFormat2KHR
         where
        type VkSurfaceFormatMType VkSurfaceFormat2KHR = VkSurfaceFormatKHR

        {-# NOINLINE vkSurfaceFormat #-}
        vkSurfaceFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceFormat2KHR, surfaceFormat})

        {-# INLINE vkSurfaceFormatByteOffset #-}
        vkSurfaceFormatByteOffset ~_
          = #{offset VkSurfaceFormat2KHR, surfaceFormat}

        {-# INLINE readVkSurfaceFormat #-}
        readVkSurfaceFormat p
          = peekByteOff p #{offset VkSurfaceFormat2KHR, surfaceFormat}

        {-# INLINE writeVkSurfaceFormat #-}
        writeVkSurfaceFormat p
          = pokeByteOff p #{offset VkSurfaceFormat2KHR, surfaceFormat}

instance {-# OVERLAPPING #-}
         HasField "surfaceFormat" VkSurfaceFormat2KHR where
        type FieldType "surfaceFormat" VkSurfaceFormat2KHR =
             VkSurfaceFormatKHR
        type FieldOptional "surfaceFormat" VkSurfaceFormat2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "surfaceFormat" VkSurfaceFormat2KHR =
             #{offset VkSurfaceFormat2KHR, surfaceFormat}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceFormat2KHR, surfaceFormat}

instance CanReadField "surfaceFormat" VkSurfaceFormat2KHR where
        {-# INLINE getField #-}
        getField = vkSurfaceFormat

        {-# INLINE readField #-}
        readField = readVkSurfaceFormat

instance Show VkSurfaceFormat2KHR where
        showsPrec d x
          = showString "VkSurfaceFormat2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSurfaceFormat = " .
                            showsPrec d (vkSurfaceFormat x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceCapabilities2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , VkSurfaceCapabilities2KHR* pSurfaceCapabilities
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceCapabilities2KHR.html vkGetPhysicalDeviceSurfaceCapabilities2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSurfaceCapabilities2KHR"
               vkGetPhysicalDeviceSurfaceCapabilities2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                                     ->
                   Ptr VkSurfaceCapabilities2KHR -- ^ pSurfaceCapabilities
                                                 -> IO VkResult

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetPhysicalDeviceSurfaceFormats2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSurfaceInfo2KHR* pSurfaceInfo
--   >     , uint32_t* pSurfaceFormatCount
--   >     , VkSurfaceFormat2KHR* pSurfaceFormats
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSurfaceFormats2KHR.html vkGetPhysicalDeviceSurfaceFormats2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceSurfaceFormats2KHR"
               vkGetPhysicalDeviceSurfaceFormats2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSurfaceInfo2KHR -- ^ pSurfaceInfo
                                                     ->
                   Ptr Word32 -- ^ pSurfaceFormatCount
                              -> Ptr VkSurfaceFormat2KHR -- ^ pSurfaceFormats
                                                         -> IO VkResult

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1

type VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: CString

pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME <-
        (is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME -> True)
  where VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
          = _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME

{-# INLINE _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME #-}

_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: CString
_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  = Ptr "VK_KHR_get_surface_capabilities2\NUL"##

{-# INLINE is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME #-}

is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME ::
                                                    CString -> Bool
is_VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  = eqCStrings _VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME

type VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME =
     "VK_KHR_get_surface_capabilities2"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR =
        VkStructureType 1000119000

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR =
        VkStructureType 1000119001

pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR :: VkStructureType

pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR =
        VkStructureType 1000119002
