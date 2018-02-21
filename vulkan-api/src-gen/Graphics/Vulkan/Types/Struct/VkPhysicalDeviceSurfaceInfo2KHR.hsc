#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSurfaceInfo2KHR
       (VkPhysicalDeviceSurfaceInfo2KHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkSurfaceKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSurfaceInfo2KHR {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkSurfaceKHR surface;
--   > } VkPhysicalDeviceSurfaceInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceSurfaceInfo2KHR.html VkPhysicalDeviceSurfaceInfo2KHR registry at www.khronos.org>
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
        type FieldIsArray "sType" VkPhysicalDeviceSurfaceInfo2KHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pNext" VkPhysicalDeviceSurfaceInfo2KHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "surface" VkPhysicalDeviceSurfaceInfo2KHR =
             'False -- ' closing tick for hsc2hs

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
