#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSurfaceCapabilities2KHR
       (VkSurfaceCapabilities2KHR(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSurfaceCapabilitiesKHR (VkSurfaceCapabilitiesKHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkSurfaceCapabilities2KHR {
--   >     VkStructureType sType;
--   >     void*   pNext;
--   >     VkSurfaceCapabilitiesKHR surfaceCapabilities;
--   > } VkSurfaceCapabilities2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSurfaceCapabilities2KHR.html VkSurfaceCapabilities2KHR registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkSurfaceCapabilities2KHR
         where
        type VkSTypeMType VkSurfaceCapabilities2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSurfaceCapabilities2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, sType}

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

instance CanReadField "sType" VkSurfaceCapabilities2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSurfaceCapabilities2KHR
         where
        type VkPNextMType VkSurfaceCapabilities2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSurfaceCapabilities2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, pNext}

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

instance CanReadField "pNext" VkSurfaceCapabilities2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkSurfaceCapabilities VkSurfaceCapabilities2KHR where
        type VkSurfaceCapabilitiesMType VkSurfaceCapabilities2KHR =
             VkSurfaceCapabilitiesKHR

        {-# NOINLINE vkSurfaceCapabilities #-}
        vkSurfaceCapabilities x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities})

        {-# INLINE vkSurfaceCapabilitiesByteOffset #-}
        vkSurfaceCapabilitiesByteOffset ~_
          = #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

        {-# INLINE readVkSurfaceCapabilities #-}
        readVkSurfaceCapabilities p
          = peekByteOff p #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

        {-# INLINE writeVkSurfaceCapabilities #-}
        writeVkSurfaceCapabilities p
          = pokeByteOff p #{offset VkSurfaceCapabilities2KHR, surfaceCapabilities}

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

instance CanReadField "surfaceCapabilities"
           VkSurfaceCapabilities2KHR
         where
        {-# INLINE getField #-}
        getField = vkSurfaceCapabilities

        {-# INLINE readField #-}
        readField = readVkSurfaceCapabilities

instance Show VkSurfaceCapabilities2KHR where
        showsPrec d x
          = showString "VkSurfaceCapabilities2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSurfaceCapabilities = " .
                            showsPrec d (vkSurfaceCapabilities x) . showChar '}'
