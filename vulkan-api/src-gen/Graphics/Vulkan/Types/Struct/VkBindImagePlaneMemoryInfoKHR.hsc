#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindImagePlaneMemoryInfoKHR
       (VkBindImagePlaneMemoryInfoKHR(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags         (VkImageAspectFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBindImageMemoryInfoKHR (VkBindImageMemoryInfoKHR)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkBindImagePlaneMemoryInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImageAspectFlagBits            planeAspect;
--   > } VkBindImagePlaneMemoryInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBindImagePlaneMemoryInfoKHR.html VkBindImagePlaneMemoryInfoKHR registry at www.khronos.org>
data VkBindImagePlaneMemoryInfoKHR = VkBindImagePlaneMemoryInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkBindImagePlaneMemoryInfoKHR where
        (VkBindImagePlaneMemoryInfoKHR## a _) ==
          x@(VkBindImagePlaneMemoryInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindImagePlaneMemoryInfoKHR where
        (VkBindImagePlaneMemoryInfoKHR## a _) `compare`
          x@(VkBindImagePlaneMemoryInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindImagePlaneMemoryInfoKHR where
        sizeOf ~_ = #{size VkBindImagePlaneMemoryInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindImagePlaneMemoryInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindImagePlaneMemoryInfoKHR where
        unsafeAddr (VkBindImagePlaneMemoryInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindImagePlaneMemoryInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindImagePlaneMemoryInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindImagePlaneMemoryInfoKHR where
        type StructFields VkBindImagePlaneMemoryInfoKHR =
             '["sType", "pNext", "planeAspect"] -- ' closing tick for hsc2hs
        type CUnionType VkBindImagePlaneMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindImagePlaneMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindImagePlaneMemoryInfoKHR =
             '[VkBindImageMemoryInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindImagePlaneMemoryInfoKHR where
        type FieldType "sType" VkBindImagePlaneMemoryInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkBindImagePlaneMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindImagePlaneMemoryInfoKHR =
             #{offset VkBindImagePlaneMemoryInfoKHR, sType}
        type FieldIsArray "sType" VkBindImagePlaneMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImagePlaneMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindImagePlaneMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindImagePlaneMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindImagePlaneMemoryInfoKHR where
        type FieldType "pNext" VkBindImagePlaneMemoryInfoKHR = Ptr Void
        type FieldOptional "pNext" VkBindImagePlaneMemoryInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindImagePlaneMemoryInfoKHR =
             #{offset VkBindImagePlaneMemoryInfoKHR, pNext}
        type FieldIsArray "pNext" VkBindImagePlaneMemoryInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImagePlaneMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindImagePlaneMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindImagePlaneMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "planeAspect" VkBindImagePlaneMemoryInfoKHR where
        type FieldType "planeAspect" VkBindImagePlaneMemoryInfoKHR =
             VkImageAspectFlagBits
        type FieldOptional "planeAspect" VkBindImagePlaneMemoryInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "planeAspect" VkBindImagePlaneMemoryInfoKHR =
             #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect}
        type FieldIsArray "planeAspect" VkBindImagePlaneMemoryInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect}

instance {-# OVERLAPPING #-}
         CanReadField "planeAspect" VkBindImagePlaneMemoryInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect}

instance {-# OVERLAPPING #-}
         CanWriteField "planeAspect" VkBindImagePlaneMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect}

instance Show VkBindImagePlaneMemoryInfoKHR where
        showsPrec d x
          = showString "VkBindImagePlaneMemoryInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "planeAspect = " .
                            showsPrec d (getField @"planeAspect" x) . showChar '}'
