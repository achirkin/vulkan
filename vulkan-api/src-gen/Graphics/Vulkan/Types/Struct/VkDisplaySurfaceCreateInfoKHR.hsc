#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplaySurfaceCreateInfoKHR
       (VkDisplaySurfaceCreateInfoKHR(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                         (VkDisplaySurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkDisplayPlaneAlphaFlagsKHR (VkDisplayPlaneAlphaFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR  (VkSurfaceTransformFlagBitsKHR)
import           Graphics.Vulkan.Types.Handles                          (VkDisplayModeKHR)
import           Graphics.Vulkan.Types.Struct.VkExtent2D                (VkExtent2D)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDisplaySurfaceCreateInfoKHR.html VkDisplaySurfaceCreateInfoKHR registry at www.khronos.org>
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
        type CUnionType VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplaySurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

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
        type FieldIsArray "sType" VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pNext" VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "flags" VkDisplaySurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "displayMode" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "planeIndex" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "planeStackIndex" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "transform" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "globalAlpha" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "alphaMode" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "imageExtent" VkDisplaySurfaceCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

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
