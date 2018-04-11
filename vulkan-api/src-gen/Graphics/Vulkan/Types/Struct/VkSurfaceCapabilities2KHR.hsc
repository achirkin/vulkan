#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSurfaceCapabilities2KHR
       (VkSurfaceCapabilities2KHR(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSurfaceCapabilitiesKHR (VkSurfaceCapabilitiesKHR)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkSurfaceCapabilities2KHR {
--   >     VkStructureType sType;
--   >     void*   pNext;
--   >     VkSurfaceCapabilitiesKHR surfaceCapabilities;
--   > } VkSurfaceCapabilities2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceCapabilities2KHR VkSurfaceCapabilities2KHR registry at www.khronos.org>
data VkSurfaceCapabilities2KHR = VkSurfaceCapabilities2KHR## Addr##
                                                            ByteArray##

instance Eq VkSurfaceCapabilities2KHR where
        (VkSurfaceCapabilities2KHR## a _) ==
          x@(VkSurfaceCapabilities2KHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceCapabilities2KHR where
        (VkSurfaceCapabilities2KHR## a _) `compare`
          x@(VkSurfaceCapabilities2KHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceCapabilities2KHR where
        sizeOf ~_ = #{size VkSurfaceCapabilities2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceCapabilities2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceCapabilities2KHR where
        unsafeAddr (VkSurfaceCapabilities2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceCapabilities2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceCapabilities2KHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceCapabilities2KHR where
        type StructFields VkSurfaceCapabilities2KHR =
             '["sType", "pNext", "surfaceCapabilities"] -- ' closing tick for hsc2hs
        type CUnionType VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceCapabilities2KHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceCapabilities2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSurfaceCapabilities2KHR where
        type FieldType "sType" VkSurfaceCapabilities2KHR = VkStructureType
        type FieldOptional "sType" VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSurfaceCapabilities2KHR =
             #{offset VkSurfaceCapabilities2KHR, sType}
        type FieldIsArray "sType" VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSurfaceCapabilities2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSurfaceCapabilities2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSurfaceCapabilities2KHR where
        type FieldType "pNext" VkSurfaceCapabilities2KHR = Ptr Void
        type FieldOptional "pNext" VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSurfaceCapabilities2KHR =
             #{offset VkSurfaceCapabilities2KHR, pNext}
        type FieldIsArray "pNext" VkSurfaceCapabilities2KHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSurfaceCapabilities2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSurfaceCapabilities2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "surfaceCapabilities" VkSurfaceCapabilities2KHR where
        type FieldType "surfaceCapabilities" VkSurfaceCapabilities2KHR =
             VkSurfaceCapabilitiesKHR
        type FieldOptional "surfaceCapabilities" VkSurfaceCapabilities2KHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "surfaceCapabilities" VkSurfaceCapabilities2KHR =
             #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}
        type FieldIsArray "surfaceCapabilities" VkSurfaceCapabilities2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

instance {-# OVERLAPPING #-}
         CanReadField "surfaceCapabilities" VkSurfaceCapabilities2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

instance {-# OVERLAPPING #-}
         CanWriteField "surfaceCapabilities" VkSurfaceCapabilities2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

instance Show VkSurfaceCapabilities2KHR where
        showsPrec d x
          = showString "VkSurfaceCapabilities2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "surfaceCapabilities = " .
                            showsPrec d (getField @"surfaceCapabilities" x) . showChar '}'
