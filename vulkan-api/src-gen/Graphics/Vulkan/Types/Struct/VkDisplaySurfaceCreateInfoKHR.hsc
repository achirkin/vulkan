#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDisplaySurfaceCreateInfoKHR
       (VkDisplaySurfaceCreateInfoKHR(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Base                                               (Addr##, ByteArray##,
                                                                         byteArrayContents##,
                                                                         plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                         (VkDisplaySurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkDisplayPlaneAlphaFlagsKHR (VkDisplayPlaneAlphaFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR  (VkSurfaceTransformFlagBitsKHR)
import           Graphics.Vulkan.Types.Handles                          (VkDisplayModeKHR)
import           Graphics.Vulkan.Types.Struct.VkExtent2D                (VkExtent2D)
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDisplaySurfaceCreateInfoKHR VkDisplaySurfaceCreateInfoKHR registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "displayMode" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, displayMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

instance {-# OVERLAPPING #-}
         CanWriteField "displayMode" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, displayMode}

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

instance {-# OVERLAPPING #-}
         CanReadField "planeIndex" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "planeIndex" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeIndex}

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

instance {-# OVERLAPPING #-}
         CanReadField "planeStackIndex" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "planeStackIndex" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, planeStackIndex}

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

instance {-# OVERLAPPING #-}
         CanReadField "transform" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, transform})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, transform}

instance {-# OVERLAPPING #-}
         CanWriteField "transform" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, transform}

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

instance {-# OVERLAPPING #-}
         CanReadField "globalAlpha" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

instance {-# OVERLAPPING #-}
         CanWriteField "globalAlpha" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, globalAlpha}

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

instance {-# OVERLAPPING #-}
         CanReadField "alphaMode" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

instance {-# OVERLAPPING #-}
         CanWriteField "alphaMode" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, alphaMode}

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

instance {-# OVERLAPPING #-}
         CanReadField "imageExtent" VkDisplaySurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "imageExtent" VkDisplaySurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDisplaySurfaceCreateInfoKHR, imageExtent}

instance Show VkDisplaySurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkDisplaySurfaceCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "displayMode = " .
                                  showsPrec d (getField @"displayMode" x) .
                                    showString ", " .
                                      showString "planeIndex = " .
                                        showsPrec d (getField @"planeIndex" x) .
                                          showString ", " .
                                            showString "planeStackIndex = " .
                                              showsPrec d (getField @"planeStackIndex" x) .
                                                showString ", " .
                                                  showString "transform = " .
                                                    showsPrec d (getField @"transform" x) .
                                                      showString ", " .
                                                        showString "globalAlpha = " .
                                                          showsPrec d (getField @"globalAlpha" x) .
                                                            showString ", " .
                                                              showString "alphaMode = " .
                                                                showsPrec d
                                                                  (getField @"alphaMode" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "imageExtent = " .
                                                                      showsPrec d
                                                                        (getField @"imageExtent" x)
                                                                        . showChar '}'
