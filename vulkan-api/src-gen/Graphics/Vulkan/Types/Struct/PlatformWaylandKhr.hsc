#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformWaylandKhr
       (VkWaylandSurfaceCreateInfoKHR(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkWaylandSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Include            (WlDisplay, WlSurface)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkWaylandSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkWaylandSurfaceCreateFlagsKHR   flags;
--   >     struct wl_display*               display;
--   >     struct wl_surface*               surface;
--   > } VkWaylandSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkWaylandSurfaceCreateInfoKHR VkWaylandSurfaceCreateInfoKHR registry at www.khronos.org>
data VkWaylandSurfaceCreateInfoKHR = VkWaylandSurfaceCreateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkWaylandSurfaceCreateInfoKHR where
        (VkWaylandSurfaceCreateInfoKHR## a _) ==
          x@(VkWaylandSurfaceCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkWaylandSurfaceCreateInfoKHR where
        (VkWaylandSurfaceCreateInfoKHR## a _) `compare`
          x@(VkWaylandSurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkWaylandSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkWaylandSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkWaylandSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkWaylandSurfaceCreateInfoKHR where
        unsafeAddr (VkWaylandSurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkWaylandSurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkWaylandSurfaceCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkWaylandSurfaceCreateInfoKHR where
        type StructFields VkWaylandSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "display", "surface"] -- ' closing tick for hsc2hs
        type CUnionType VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkWaylandSurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkWaylandSurfaceCreateInfoKHR where
        type FieldType "sType" VkWaylandSurfaceCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWaylandSurfaceCreateInfoKHR =
             #{offset VkWaylandSurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWaylandSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkWaylandSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWaylandSurfaceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkWaylandSurfaceCreateInfoKHR where
        type FieldType "pNext" VkWaylandSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWaylandSurfaceCreateInfoKHR =
             #{offset VkWaylandSurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWaylandSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkWaylandSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWaylandSurfaceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkWaylandSurfaceCreateInfoKHR where
        type FieldType "flags" VkWaylandSurfaceCreateInfoKHR =
             VkWaylandSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkWaylandSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkWaylandSurfaceCreateInfoKHR =
             #{offset VkWaylandSurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWaylandSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkWaylandSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWaylandSurfaceCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "display" VkWaylandSurfaceCreateInfoKHR where
        type FieldType "display" VkWaylandSurfaceCreateInfoKHR =
             Ptr WlDisplay
        type FieldOptional "display" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "display" VkWaylandSurfaceCreateInfoKHR =
             #{offset VkWaylandSurfaceCreateInfoKHR, display}
        type FieldIsArray "display" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWaylandSurfaceCreateInfoKHR, display}

instance {-# OVERLAPPING #-}
         CanReadField "display" VkWaylandSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWaylandSurfaceCreateInfoKHR, display})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, display}

instance {-# OVERLAPPING #-}
         CanWriteField "display" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, display}

instance {-# OVERLAPPING #-}
         HasField "surface" VkWaylandSurfaceCreateInfoKHR where
        type FieldType "surface" VkWaylandSurfaceCreateInfoKHR =
             Ptr WlSurface
        type FieldOptional "surface" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "surface" VkWaylandSurfaceCreateInfoKHR =
             #{offset VkWaylandSurfaceCreateInfoKHR, surface}
        type FieldIsArray "surface" VkWaylandSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWaylandSurfaceCreateInfoKHR, surface}

instance {-# OVERLAPPING #-}
         CanReadField "surface" VkWaylandSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWaylandSurfaceCreateInfoKHR, surface})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, surface}

instance {-# OVERLAPPING #-}
         CanWriteField "surface" VkWaylandSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWaylandSurfaceCreateInfoKHR, surface}

instance Show VkWaylandSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkWaylandSurfaceCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "display = " .
                                  showsPrec d (getField @"display" x) .
                                    showString ", " .
                                      showString "surface = " .
                                        showsPrec d (getField @"surface" x) . showChar '}'
