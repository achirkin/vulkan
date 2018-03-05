#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImagePlaneMemoryRequirementsInfoKHR
       (VkImagePlaneMemoryRequirementsInfoKHR(..)) where
import           Foreign.Storable                                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageAspectFlags                  (VkImageAspectFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType                     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageMemoryRequirementsInfo2KHR (VkImageMemoryRequirementsInfo2KHR)
import           System.IO.Unsafe                                               (unsafeDupablePerformIO)

-- | > typedef struct VkImagePlaneMemoryRequirementsInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImageAspectFlagBits            planeAspect;
--   > } VkImagePlaneMemoryRequirementsInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImagePlaneMemoryRequirementsInfoKHR.html VkImagePlaneMemoryRequirementsInfoKHR registry at www.khronos.org>
data VkImagePlaneMemoryRequirementsInfoKHR = VkImagePlaneMemoryRequirementsInfoKHR## Addr##
                                                                                    ByteArray##

instance Eq VkImagePlaneMemoryRequirementsInfoKHR where
        (VkImagePlaneMemoryRequirementsInfoKHR## a _) ==
          x@(VkImagePlaneMemoryRequirementsInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImagePlaneMemoryRequirementsInfoKHR where
        (VkImagePlaneMemoryRequirementsInfoKHR## a _) `compare`
          x@(VkImagePlaneMemoryRequirementsInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImagePlaneMemoryRequirementsInfoKHR where
        sizeOf ~_
          = #{size VkImagePlaneMemoryRequirementsInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImagePlaneMemoryRequirementsInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImagePlaneMemoryRequirementsInfoKHR
         where
        unsafeAddr (VkImagePlaneMemoryRequirementsInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImagePlaneMemoryRequirementsInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImagePlaneMemoryRequirementsInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImagePlaneMemoryRequirementsInfoKHR where
        type StructFields VkImagePlaneMemoryRequirementsInfoKHR =
             '["sType", "pNext", "planeAspect"] -- ' closing tick for hsc2hs
        type CUnionType VkImagePlaneMemoryRequirementsInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImagePlaneMemoryRequirementsInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImagePlaneMemoryRequirementsInfoKHR =
             '[VkImageMemoryRequirementsInfo2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImagePlaneMemoryRequirementsInfoKHR where
        type FieldType "sType" VkImagePlaneMemoryRequirementsInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImagePlaneMemoryRequirementsInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImagePlaneMemoryRequirementsInfoKHR =
             #{offset VkImagePlaneMemoryRequirementsInfoKHR, sType}
        type FieldIsArray "sType" VkImagePlaneMemoryRequirementsInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImagePlaneMemoryRequirementsInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImagePlaneMemoryRequirementsInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImagePlaneMemoryRequirementsInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImagePlaneMemoryRequirementsInfoKHR where
        type FieldType "pNext" VkImagePlaneMemoryRequirementsInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkImagePlaneMemoryRequirementsInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImagePlaneMemoryRequirementsInfoKHR =
             #{offset VkImagePlaneMemoryRequirementsInfoKHR, pNext}
        type FieldIsArray "pNext" VkImagePlaneMemoryRequirementsInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImagePlaneMemoryRequirementsInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImagePlaneMemoryRequirementsInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImagePlaneMemoryRequirementsInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "planeAspect" VkImagePlaneMemoryRequirementsInfoKHR where
        type FieldType "planeAspect" VkImagePlaneMemoryRequirementsInfoKHR
             = VkImageAspectFlagBits
        type FieldOptional "planeAspect"
               VkImagePlaneMemoryRequirementsInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "planeAspect"
               VkImagePlaneMemoryRequirementsInfoKHR
             =
             #{offset VkImagePlaneMemoryRequirementsInfoKHR, planeAspect}
        type FieldIsArray "planeAspect"
               VkImagePlaneMemoryRequirementsInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImagePlaneMemoryRequirementsInfoKHR, planeAspect}

instance {-# OVERLAPPING #-}
         CanReadField "planeAspect" VkImagePlaneMemoryRequirementsInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImagePlaneMemoryRequirementsInfoKHR, planeAspect})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, planeAspect}

instance {-# OVERLAPPING #-}
         CanWriteField "planeAspect" VkImagePlaneMemoryRequirementsInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImagePlaneMemoryRequirementsInfoKHR, planeAspect}

instance Show VkImagePlaneMemoryRequirementsInfoKHR where
        showsPrec d x
          = showString "VkImagePlaneMemoryRequirementsInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "planeAspect = " .
                            showsPrec d (getField @"planeAspect" x) . showChar '}'
