#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAndroidSurfaceCreateInfoKHR
       (VkAndroidSurfaceCreateInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkAndroidSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Defines              (ANativeWindow)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkAndroidSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                    pNext;
--   >     VkAndroidSurfaceCreateFlagsKHR flags;
--   >     struct ANativeWindow*    window;
--   > } VkAndroidSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkAndroidSurfaceCreateInfoKHR.html VkAndroidSurfaceCreateInfoKHR registry at www.khronos.org>
data VkAndroidSurfaceCreateInfoKHR = VkAndroidSurfaceCreateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkAndroidSurfaceCreateInfoKHR where
        (VkAndroidSurfaceCreateInfoKHR## a _) ==
          x@(VkAndroidSurfaceCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAndroidSurfaceCreateInfoKHR where
        (VkAndroidSurfaceCreateInfoKHR## a _) `compare`
          x@(VkAndroidSurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAndroidSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkAndroidSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAndroidSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAndroidSurfaceCreateInfoKHR where
        unsafeAddr (VkAndroidSurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAndroidSurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAndroidSurfaceCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAndroidSurfaceCreateInfoKHR where
        type StructFields VkAndroidSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "window"] -- ' closing tick for hsc2hs
        type CUnionType VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAndroidSurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "sType" VkAndroidSurfaceCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkAndroidSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "pNext" VkAndroidSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkAndroidSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "flags" VkAndroidSurfaceCreateInfoKHR =
             VkAndroidSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkAndroidSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkAndroidSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "window" VkAndroidSurfaceCreateInfoKHR where
        type FieldType "window" VkAndroidSurfaceCreateInfoKHR =
             Ptr ANativeWindow
        type FieldOptional "window" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "window" VkAndroidSurfaceCreateInfoKHR =
             #{offset VkAndroidSurfaceCreateInfoKHR, window}
        type FieldIsArray "window" VkAndroidSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         CanReadField "window" VkAndroidSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidSurfaceCreateInfoKHR, window})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         CanWriteField "window" VkAndroidSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidSurfaceCreateInfoKHR, window}

instance Show VkAndroidSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkAndroidSurfaceCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "window = " .
                                  showsPrec d (getField @"window" x) . showChar '}'
