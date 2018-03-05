#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkWin32SurfaceCreateInfoKHR
       (VkWin32SurfaceCreateInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkWin32SurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Include              (HINSTANCE, HWND)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkWin32SurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkWin32SurfaceCreateFlagsKHR   flags;
--   >     HINSTANCE                        hinstance;
--   >     HWND                             hwnd;
--   > } VkWin32SurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkWin32SurfaceCreateInfoKHR.html VkWin32SurfaceCreateInfoKHR registry at www.khronos.org>
data VkWin32SurfaceCreateInfoKHR = VkWin32SurfaceCreateInfoKHR## Addr##
                                                                ByteArray##

instance Eq VkWin32SurfaceCreateInfoKHR where
        (VkWin32SurfaceCreateInfoKHR## a _) ==
          x@(VkWin32SurfaceCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkWin32SurfaceCreateInfoKHR where
        (VkWin32SurfaceCreateInfoKHR## a _) `compare`
          x@(VkWin32SurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkWin32SurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkWin32SurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkWin32SurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkWin32SurfaceCreateInfoKHR where
        unsafeAddr (VkWin32SurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkWin32SurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkWin32SurfaceCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkWin32SurfaceCreateInfoKHR where
        type StructFields VkWin32SurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "hinstance", "hwnd"] -- ' closing tick for hsc2hs
        type CUnionType VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkWin32SurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkWin32SurfaceCreateInfoKHR where
        type FieldType "sType" VkWin32SurfaceCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkWin32SurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkWin32SurfaceCreateInfoKHR where
        type FieldType "pNext" VkWin32SurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkWin32SurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkWin32SurfaceCreateInfoKHR where
        type FieldType "flags" VkWin32SurfaceCreateInfoKHR =
             VkWin32SurfaceCreateFlagsKHR
        type FieldOptional "flags" VkWin32SurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkWin32SurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "hinstance" VkWin32SurfaceCreateInfoKHR where
        type FieldType "hinstance" VkWin32SurfaceCreateInfoKHR = HINSTANCE
        type FieldOptional "hinstance" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "hinstance" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, hinstance}
        type FieldIsArray "hinstance" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

instance {-# OVERLAPPING #-}
         CanReadField "hinstance" VkWin32SurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, hinstance})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

instance {-# OVERLAPPING #-}
         CanWriteField "hinstance" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hinstance}

instance {-# OVERLAPPING #-}
         HasField "hwnd" VkWin32SurfaceCreateInfoKHR where
        type FieldType "hwnd" VkWin32SurfaceCreateInfoKHR = HWND
        type FieldOptional "hwnd" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "hwnd" VkWin32SurfaceCreateInfoKHR =
             #{offset VkWin32SurfaceCreateInfoKHR, hwnd}
        type FieldIsArray "hwnd" VkWin32SurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

instance {-# OVERLAPPING #-}
         CanReadField "hwnd" VkWin32SurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32SurfaceCreateInfoKHR, hwnd})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

instance {-# OVERLAPPING #-}
         CanWriteField "hwnd" VkWin32SurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32SurfaceCreateInfoKHR, hwnd}

instance Show VkWin32SurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkWin32SurfaceCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "hinstance = " .
                                  showsPrec d (getField @"hinstance" x) .
                                    showString ", " .
                                      showString "hwnd = " .
                                        showsPrec d (getField @"hwnd" x) . showChar '}'
