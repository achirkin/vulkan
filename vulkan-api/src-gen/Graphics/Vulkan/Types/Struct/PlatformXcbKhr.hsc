#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.PlatformXcbKhr
       (VkXcbSurfaceCreateInfoKHR(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkXcbSurfaceCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Include            (XcbConnectionT,
                                                           XcbWindowT)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkXcbSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkXcbSurfaceCreateFlagsKHR   flags;
--   >     xcb_connection_t*                connection;
--   >     xcb_window_t                     window;
--   > } VkXcbSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkXcbSurfaceCreateInfoKHR VkXcbSurfaceCreateInfoKHR registry at www.khronos.org>
data VkXcbSurfaceCreateInfoKHR = VkXcbSurfaceCreateInfoKHR## Addr##
                                                            ByteArray##

instance Eq VkXcbSurfaceCreateInfoKHR where
        (VkXcbSurfaceCreateInfoKHR## a _) ==
          x@(VkXcbSurfaceCreateInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkXcbSurfaceCreateInfoKHR where
        (VkXcbSurfaceCreateInfoKHR## a _) `compare`
          x@(VkXcbSurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkXcbSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkXcbSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkXcbSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkXcbSurfaceCreateInfoKHR where
        unsafeAddr (VkXcbSurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkXcbSurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkXcbSurfaceCreateInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkXcbSurfaceCreateInfoKHR where
        type StructFields VkXcbSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "connection", "window"] -- ' closing tick for hsc2hs
        type CUnionType VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkXcbSurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkXcbSurfaceCreateInfoKHR where
        type FieldType "sType" VkXcbSurfaceCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkXcbSurfaceCreateInfoKHR =
             #{offset VkXcbSurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXcbSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkXcbSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkXcbSurfaceCreateInfoKHR where
        type FieldType "pNext" VkXcbSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkXcbSurfaceCreateInfoKHR =
             #{offset VkXcbSurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXcbSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkXcbSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkXcbSurfaceCreateInfoKHR where
        type FieldType "flags" VkXcbSurfaceCreateInfoKHR =
             VkXcbSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkXcbSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkXcbSurfaceCreateInfoKHR =
             #{offset VkXcbSurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXcbSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkXcbSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "connection" VkXcbSurfaceCreateInfoKHR where
        type FieldType "connection" VkXcbSurfaceCreateInfoKHR =
             Ptr XcbConnectionT
        type FieldOptional "connection" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "connection" VkXcbSurfaceCreateInfoKHR =
             #{offset VkXcbSurfaceCreateInfoKHR, connection}
        type FieldIsArray "connection" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXcbSurfaceCreateInfoKHR, connection}

instance {-# OVERLAPPING #-}
         CanReadField "connection" VkXcbSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, connection})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, connection}

instance {-# OVERLAPPING #-}
         CanWriteField "connection" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, connection}

instance {-# OVERLAPPING #-}
         HasField "window" VkXcbSurfaceCreateInfoKHR where
        type FieldType "window" VkXcbSurfaceCreateInfoKHR = XcbWindowT
        type FieldOptional "window" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "window" VkXcbSurfaceCreateInfoKHR =
             #{offset VkXcbSurfaceCreateInfoKHR, window}
        type FieldIsArray "window" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXcbSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         CanReadField "window" VkXcbSurfaceCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, window})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         CanWriteField "window" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, window}

instance Show VkXcbSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkXcbSurfaceCreateInfoKHR {" .
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
                                      showString "window = " .
                                        showsPrec d (getField @"window" x) . showChar '}'
