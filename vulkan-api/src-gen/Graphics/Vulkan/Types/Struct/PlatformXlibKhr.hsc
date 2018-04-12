#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformXlibKhr
       (VkXlibSurfaceCreateInfoKHR(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkXlibSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Include            (Display, Window)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkXlibSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkXlibSurfaceCreateFlagsKHR   flags;
--   >     Display*                         dpy;
--   >     Window                           window;
--   > } VkXlibSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkXlibSurfaceCreateInfoKHR VkXlibSurfaceCreateInfoKHR registry at www.khronos.org>
data VkXlibSurfaceCreateInfoKHR = VkXlibSurfaceCreateInfoKHR## Addr##
                                                              ByteArray##

instance Eq VkXlibSurfaceCreateInfoKHR where
        (VkXlibSurfaceCreateInfoKHR## a _) ==
          x@(VkXlibSurfaceCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkXlibSurfaceCreateInfoKHR where
        (VkXlibSurfaceCreateInfoKHR## a _) `compare`
          x@(VkXlibSurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkXlibSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkXlibSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkXlibSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkXlibSurfaceCreateInfoKHR where
        unsafeAddr (VkXlibSurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkXlibSurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkXlibSurfaceCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkXlibSurfaceCreateInfoKHR where
        type StructFields VkXlibSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "dpy", "window"] -- ' closing tick for hsc2hs
        type CUnionType VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkXlibSurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkXlibSurfaceCreateInfoKHR where
        type FieldType "sType" VkXlibSurfaceCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkXlibSurfaceCreateInfoKHR =
             #{offset VkXlibSurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXlibSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkXlibSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXlibSurfaceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkXlibSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkXlibSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkXlibSurfaceCreateInfoKHR where
        type FieldType "pNext" VkXlibSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkXlibSurfaceCreateInfoKHR =
             #{offset VkXlibSurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXlibSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkXlibSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXlibSurfaceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkXlibSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkXlibSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkXlibSurfaceCreateInfoKHR where
        type FieldType "flags" VkXlibSurfaceCreateInfoKHR =
             VkXlibSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkXlibSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkXlibSurfaceCreateInfoKHR =
             #{offset VkXlibSurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXlibSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkXlibSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXlibSurfaceCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkXlibSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkXlibSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "dpy" VkXlibSurfaceCreateInfoKHR where
        type FieldType "dpy" VkXlibSurfaceCreateInfoKHR = Ptr Display
        type FieldOptional "dpy" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dpy" VkXlibSurfaceCreateInfoKHR =
             #{offset VkXlibSurfaceCreateInfoKHR, dpy}
        type FieldIsArray "dpy" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkXlibSurfaceCreateInfoKHR, dpy}

instance {-# OVERLAPPING #-}
         CanReadField "dpy" VkXlibSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXlibSurfaceCreateInfoKHR, dpy})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkXlibSurfaceCreateInfoKHR, dpy}

instance {-# OVERLAPPING #-}
         CanWriteField "dpy" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkXlibSurfaceCreateInfoKHR, dpy}

instance {-# OVERLAPPING #-}
         HasField "window" VkXlibSurfaceCreateInfoKHR where
        type FieldType "window" VkXlibSurfaceCreateInfoKHR = Window
        type FieldOptional "window" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "window" VkXlibSurfaceCreateInfoKHR =
             #{offset VkXlibSurfaceCreateInfoKHR, window}
        type FieldIsArray "window" VkXlibSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXlibSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         CanReadField "window" VkXlibSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXlibSurfaceCreateInfoKHR, window})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkXlibSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         CanWriteField "window" VkXlibSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkXlibSurfaceCreateInfoKHR, window}

instance Show VkXlibSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkXlibSurfaceCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "dpy = " .
                                  showsPrec d (getField @"dpy" x) .
                                    showString ", " .
                                      showString "window = " .
                                        showsPrec d (getField @"window" x) . showChar '}'
