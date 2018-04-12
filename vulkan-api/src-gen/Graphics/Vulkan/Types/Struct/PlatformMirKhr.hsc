#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformMirKhr
       (VkMirSurfaceCreateInfoKHR(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkMirSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Include            (MirConnection,
                                                           MirSurface)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkMirSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMirSurfaceCreateFlagsKHR   flags;
--   >     MirConnection*                   connection;
--   >     MirSurface*                      mirSurface;
--   > } VkMirSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMirSurfaceCreateInfoKHR VkMirSurfaceCreateInfoKHR registry at www.khronos.org>
data VkMirSurfaceCreateInfoKHR = VkMirSurfaceCreateInfoKHR## Addr##
                                                            ByteArray##

instance Eq VkMirSurfaceCreateInfoKHR where
        (VkMirSurfaceCreateInfoKHR## a _) ==
          x@(VkMirSurfaceCreateInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMirSurfaceCreateInfoKHR where
        (VkMirSurfaceCreateInfoKHR## a _) `compare`
          x@(VkMirSurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMirSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkMirSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMirSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMirSurfaceCreateInfoKHR where
        unsafeAddr (VkMirSurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMirSurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMirSurfaceCreateInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMirSurfaceCreateInfoKHR where
        type StructFields VkMirSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "connection", "mirSurface"] -- ' closing tick for hsc2hs
        type CUnionType VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMirSurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMirSurfaceCreateInfoKHR where
        type FieldType "sType" VkMirSurfaceCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMirSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMirSurfaceCreateInfoKHR where
        type FieldType "pNext" VkMirSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMirSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkMirSurfaceCreateInfoKHR where
        type FieldType "flags" VkMirSurfaceCreateInfoKHR =
             VkMirSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkMirSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkMirSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "connection" VkMirSurfaceCreateInfoKHR where
        type FieldType "connection" VkMirSurfaceCreateInfoKHR =
             Ptr MirConnection
        type FieldOptional "connection" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "connection" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, connection}
        type FieldIsArray "connection" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, connection}

instance {-# OVERLAPPING #-}
         CanReadField "connection" VkMirSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, connection})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, connection}

instance {-# OVERLAPPING #-}
         CanWriteField "connection" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, connection}

instance {-# OVERLAPPING #-}
         HasField "mirSurface" VkMirSurfaceCreateInfoKHR where
        type FieldType "mirSurface" VkMirSurfaceCreateInfoKHR =
             Ptr MirSurface
        type FieldOptional "mirSurface" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mirSurface" VkMirSurfaceCreateInfoKHR =
             #{offset VkMirSurfaceCreateInfoKHR, mirSurface}
        type FieldIsArray "mirSurface" VkMirSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

instance {-# OVERLAPPING #-}
         CanReadField "mirSurface" VkMirSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMirSurfaceCreateInfoKHR, mirSurface})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

instance {-# OVERLAPPING #-}
         CanWriteField "mirSurface" VkMirSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMirSurfaceCreateInfoKHR, mirSurface}

instance Show VkMirSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkMirSurfaceCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "connection = " .
                                  showsPrec d (getField @"connection" x) .
                                    showString ", " .
                                      showString "mirSurface = " .
                                        showsPrec d (getField @"mirSurface" x) . showChar '}'
