#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkBindImagePlaneMemoryInfoKHR where
        type VkSTypeMType VkBindImagePlaneMemoryInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkBindImagePlaneMemoryInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, sType}

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

instance CanReadField "sType" VkBindImagePlaneMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkBindImagePlaneMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkBindImagePlaneMemoryInfoKHR where
        type VkPNextMType VkBindImagePlaneMemoryInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkBindImagePlaneMemoryInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, pNext}

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

instance CanReadField "pNext" VkBindImagePlaneMemoryInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkBindImagePlaneMemoryInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkPlaneAspect VkBindImagePlaneMemoryInfoKHR where
        type VkPlaneAspectMType VkBindImagePlaneMemoryInfoKHR =
             VkImageAspectFlagBits

        {-# NOINLINE vkPlaneAspect #-}
        vkPlaneAspect x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect})

        {-# INLINE vkPlaneAspectByteOffset #-}
        vkPlaneAspectByteOffset ~_
          = #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect}

        {-# INLINE readVkPlaneAspect #-}
        readVkPlaneAspect p
          = peekByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect}

        {-# INLINE writeVkPlaneAspect #-}
        writeVkPlaneAspect p
          = pokeByteOff p #{offset VkBindImagePlaneMemoryInfoKHR, planeAspect}

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

instance CanReadField "planeAspect" VkBindImagePlaneMemoryInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPlaneAspect

        {-# INLINE readField #-}
        readField = readVkPlaneAspect

instance CanWriteField "planeAspect" VkBindImagePlaneMemoryInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPlaneAspect

instance Show VkBindImagePlaneMemoryInfoKHR where
        showsPrec d x
          = showString "VkBindImagePlaneMemoryInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPlaneAspect = " .
                            showsPrec d (vkPlaneAspect x) . showChar '}'
