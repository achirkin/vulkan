#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.SharedPresentSurfaceCapabilitiesKHR
       (VkSharedPresentSurfaceCapabilitiesKHR(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.Image         (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Surface     (VkSurfaceCapabilities2KHR)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkSharedPresentSurfaceCapabilitiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkImageUsageFlags sharedPresentSupportedUsageFlags;
--   > } VkSharedPresentSurfaceCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSharedPresentSurfaceCapabilitiesKHR VkSharedPresentSurfaceCapabilitiesKHR registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSharedPresentSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSharedPresentSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSharedPresentSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSharedPresentSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "sharedPresentSupportedUsageFlags"
           VkSharedPresentSurfaceCapabilitiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "sharedPresentSupportedUsageFlags"
           VkSharedPresentSurfaceCapabilitiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

instance Show VkSharedPresentSurfaceCapabilitiesKHR where
        showsPrec d x
          = showString "VkSharedPresentSurfaceCapabilitiesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "sharedPresentSupportedUsageFlags = " .
                            showsPrec d (getField @"sharedPresentSupportedUsageFlags" x) .
                              showChar '}'
