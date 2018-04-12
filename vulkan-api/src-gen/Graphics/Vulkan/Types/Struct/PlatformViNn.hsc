#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformViNn
       (VkViSurfaceCreateInfoNN(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkViSurfaceCreateFlagsNN)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkViSurfaceCreateInfoNN {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkViSurfaceCreateFlagsNN   flags;
--   >     void*                            window;
--   > } VkViSurfaceCreateInfoNN;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkViSurfaceCreateInfoNN VkViSurfaceCreateInfoNN registry at www.khronos.org>
data VkViSurfaceCreateInfoNN = VkViSurfaceCreateInfoNN## Addr##
                                                        ByteArray##

instance Eq VkViSurfaceCreateInfoNN where
        (VkViSurfaceCreateInfoNN## a _) == x@(VkViSurfaceCreateInfoNN## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkViSurfaceCreateInfoNN where
        (VkViSurfaceCreateInfoNN## a _) `compare`
          x@(VkViSurfaceCreateInfoNN## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkViSurfaceCreateInfoNN where
        sizeOf ~_ = #{size VkViSurfaceCreateInfoNN}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViSurfaceCreateInfoNN}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkViSurfaceCreateInfoNN where
        unsafeAddr (VkViSurfaceCreateInfoNN## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkViSurfaceCreateInfoNN## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkViSurfaceCreateInfoNN## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkViSurfaceCreateInfoNN where
        type StructFields VkViSurfaceCreateInfoNN =
             '["sType", "pNext", "flags", "window"] -- ' closing tick for hsc2hs
        type CUnionType VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type StructExtends VkViSurfaceCreateInfoNN = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkViSurfaceCreateInfoNN where
        type FieldType "sType" VkViSurfaceCreateInfoNN = VkStructureType
        type FieldOptional "sType" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, sType}
        type FieldIsArray "sType" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkViSurfaceCreateInfoNN where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkViSurfaceCreateInfoNN where
        type FieldType "pNext" VkViSurfaceCreateInfoNN = Ptr Void
        type FieldOptional "pNext" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, pNext}
        type FieldIsArray "pNext" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkViSurfaceCreateInfoNN where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkViSurfaceCreateInfoNN where
        type FieldType "flags" VkViSurfaceCreateInfoNN =
             VkViSurfaceCreateFlagsNN
        type FieldOptional "flags" VkViSurfaceCreateInfoNN = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, flags}
        type FieldIsArray "flags" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkViSurfaceCreateInfoNN where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, flags}

instance {-# OVERLAPPING #-}
         HasField "window" VkViSurfaceCreateInfoNN where
        type FieldType "window" VkViSurfaceCreateInfoNN = Ptr Void
        type FieldOptional "window" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs
        type FieldOffset "window" VkViSurfaceCreateInfoNN =
             #{offset VkViSurfaceCreateInfoNN, window}
        type FieldIsArray "window" VkViSurfaceCreateInfoNN = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViSurfaceCreateInfoNN, window}

instance {-# OVERLAPPING #-}
         CanReadField "window" VkViSurfaceCreateInfoNN where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViSurfaceCreateInfoNN, window})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkViSurfaceCreateInfoNN, window}

instance {-# OVERLAPPING #-}
         CanWriteField "window" VkViSurfaceCreateInfoNN where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkViSurfaceCreateInfoNN, window}

instance Show VkViSurfaceCreateInfoNN where
        showsPrec d x
          = showString "VkViSurfaceCreateInfoNN {" .
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
