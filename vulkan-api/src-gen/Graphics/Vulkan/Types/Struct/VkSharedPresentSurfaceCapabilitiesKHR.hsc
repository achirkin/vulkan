#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSharedPresentSurfaceCapabilitiesKHR
       (VkSharedPresentSurfaceCapabilitiesKHR(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags           (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSurfaceCapabilities2KHR (VkSurfaceCapabilities2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkSharedPresentSurfaceCapabilitiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkImageUsageFlags sharedPresentSupportedUsageFlags;
--   > } VkSharedPresentSurfaceCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSharedPresentSurfaceCapabilitiesKHR.html VkSharedPresentSurfaceCapabilitiesKHR registry at www.khronos.org>
data VkSharedPresentSurfaceCapabilitiesKHR = VkSharedPresentSurfaceCapabilitiesKHR## Addr##
                                                                                    ByteArray##

instance Eq VkSharedPresentSurfaceCapabilitiesKHR where
        (VkSharedPresentSurfaceCapabilitiesKHR## a _) ==
          x@(VkSharedPresentSurfaceCapabilitiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSharedPresentSurfaceCapabilitiesKHR where
        (VkSharedPresentSurfaceCapabilitiesKHR## a _) `compare`
          x@(VkSharedPresentSurfaceCapabilitiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSharedPresentSurfaceCapabilitiesKHR where
        sizeOf ~_
          = #{size VkSharedPresentSurfaceCapabilitiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSharedPresentSurfaceCapabilitiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSharedPresentSurfaceCapabilitiesKHR
         where
        unsafeAddr (VkSharedPresentSurfaceCapabilitiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSharedPresentSurfaceCapabilitiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSharedPresentSurfaceCapabilitiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSharedPresentSurfaceCapabilitiesKHR where
        type StructFields VkSharedPresentSurfaceCapabilitiesKHR =
             '["sType", "pNext", "sharedPresentSupportedUsageFlags"] -- ' closing tick for hsc2hs
        type CUnionType VkSharedPresentSurfaceCapabilitiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSharedPresentSurfaceCapabilitiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSharedPresentSurfaceCapabilitiesKHR =
             '[VkSurfaceCapabilities2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkSharedPresentSurfaceCapabilitiesKHR where
        type VkSTypeMType VkSharedPresentSurfaceCapabilitiesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSharedPresentSurfaceCapabilitiesKHR where
        type FieldType "sType" VkSharedPresentSurfaceCapabilitiesKHR =
             VkStructureType
        type FieldOptional "sType" VkSharedPresentSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSharedPresentSurfaceCapabilitiesKHR =
             #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}
        type FieldIsArray "sType" VkSharedPresentSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

instance CanReadField "sType" VkSharedPresentSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSharedPresentSurfaceCapabilitiesKHR where
        type VkPNextMType VkSharedPresentSurfaceCapabilitiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSharedPresentSurfaceCapabilitiesKHR where
        type FieldType "pNext" VkSharedPresentSurfaceCapabilitiesKHR =
             Ptr Void
        type FieldOptional "pNext" VkSharedPresentSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSharedPresentSurfaceCapabilitiesKHR =
             #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}
        type FieldIsArray "pNext" VkSharedPresentSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

instance CanReadField "pNext" VkSharedPresentSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkSharedPresentSupportedUsageFlags
           VkSharedPresentSurfaceCapabilitiesKHR
         where
        type VkSharedPresentSupportedUsageFlagsMType
               VkSharedPresentSurfaceCapabilitiesKHR
             = VkImageUsageFlags

        {-# NOINLINE vkSharedPresentSupportedUsageFlags #-}
        vkSharedPresentSupportedUsageFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags})

        {-# INLINE vkSharedPresentSupportedUsageFlagsByteOffset #-}
        vkSharedPresentSupportedUsageFlagsByteOffset ~_
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

        {-# INLINE readVkSharedPresentSupportedUsageFlags #-}
        readVkSharedPresentSupportedUsageFlags p
          = peekByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

        {-# INLINE writeVkSharedPresentSupportedUsageFlags #-}
        writeVkSharedPresentSupportedUsageFlags p
          = pokeByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

instance {-# OVERLAPPING #-}
         HasField "sharedPresentSupportedUsageFlags"
           VkSharedPresentSurfaceCapabilitiesKHR
         where
        type FieldType "sharedPresentSupportedUsageFlags"
               VkSharedPresentSurfaceCapabilitiesKHR
             = VkImageUsageFlags
        type FieldOptional "sharedPresentSupportedUsageFlags"
               VkSharedPresentSurfaceCapabilitiesKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sharedPresentSupportedUsageFlags"
               VkSharedPresentSurfaceCapabilitiesKHR
             =
             #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}
        type FieldIsArray "sharedPresentSupportedUsageFlags"
               VkSharedPresentSurfaceCapabilitiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

instance CanReadField "sharedPresentSupportedUsageFlags"
           VkSharedPresentSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSharedPresentSupportedUsageFlags

        {-# INLINE readField #-}
        readField = readVkSharedPresentSupportedUsageFlags

instance Show VkSharedPresentSurfaceCapabilitiesKHR where
        showsPrec d x
          = showString "VkSharedPresentSurfaceCapabilitiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSharedPresentSupportedUsageFlags = " .
                            showsPrec d (vkSharedPresentSupportedUsageFlags x) . showChar '}'
