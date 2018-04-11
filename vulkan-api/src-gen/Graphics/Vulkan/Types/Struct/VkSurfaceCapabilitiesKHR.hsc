#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSurfaceCapabilitiesKHR
       (VkSurfaceCapabilitiesKHR(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkCompositeAlphaFlagsKHR   (VkCompositeAlphaFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags          (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkSurfaceTransformFlagsKHR (VkSurfaceTransformFlagBitsKHR,
                                                                        VkSurfaceTransformFlagsKHR)
import           Graphics.Vulkan.Types.Struct.VkExtent2D               (VkExtent2D)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkSurfaceCapabilitiesKHR {
--   >     uint32_t                         minImageCount;
--   >     uint32_t                         maxImageCount;
--   >     VkExtent2D                       currentExtent;
--   >     VkExtent2D                       minImageExtent;
--   >     VkExtent2D                       maxImageExtent;
--   >     uint32_t                         maxImageArrayLayers;
--   >     VkSurfaceTransformFlagsKHR       supportedTransforms;
--   >     VkSurfaceTransformFlagBitsKHR    currentTransform;
--   >     VkCompositeAlphaFlagsKHR         supportedCompositeAlpha;
--   >     VkImageUsageFlags                supportedUsageFlags;
--   > } VkSurfaceCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSurfaceCapabilitiesKHR VkSurfaceCapabilitiesKHR registry at www.khronos.org>
data VkSurfaceCapabilitiesKHR = VkSurfaceCapabilitiesKHR## Addr##
                                                          ByteArray##

instance Eq VkSurfaceCapabilitiesKHR where
        (VkSurfaceCapabilitiesKHR## a _) ==
          x@(VkSurfaceCapabilitiesKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSurfaceCapabilitiesKHR where
        (VkSurfaceCapabilitiesKHR## a _) `compare`
          x@(VkSurfaceCapabilitiesKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSurfaceCapabilitiesKHR where
        sizeOf ~_ = #{size VkSurfaceCapabilitiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSurfaceCapabilitiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSurfaceCapabilitiesKHR where
        unsafeAddr (VkSurfaceCapabilitiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSurfaceCapabilitiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSurfaceCapabilitiesKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSurfaceCapabilitiesKHR where
        type StructFields VkSurfaceCapabilitiesKHR =
             '["minImageCount", "maxImageCount", "currentExtent", -- ' closing tick for hsc2hs
               "minImageExtent", "maxImageExtent", "maxImageArrayLayers",
               "supportedTransforms", "currentTransform",
               "supportedCompositeAlpha", "supportedUsageFlags"]
        type CUnionType VkSurfaceCapabilitiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSurfaceCapabilitiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkSurfaceCapabilitiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "minImageCount" VkSurfaceCapabilitiesKHR where
        type FieldType "minImageCount" VkSurfaceCapabilitiesKHR = Word32
        type FieldOptional "minImageCount" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minImageCount" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, minImageCount}
        type FieldIsArray "minImageCount" VkSurfaceCapabilitiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, minImageCount}

instance {-# OVERLAPPING #-}
         CanReadField "minImageCount" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, minImageCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageCount}

instance {-# OVERLAPPING #-}
         CanWriteField "minImageCount" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageCount}

instance {-# OVERLAPPING #-}
         HasField "maxImageCount" VkSurfaceCapabilitiesKHR where
        type FieldType "maxImageCount" VkSurfaceCapabilitiesKHR = Word32
        type FieldOptional "maxImageCount" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageCount" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, maxImageCount}
        type FieldIsArray "maxImageCount" VkSurfaceCapabilitiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, maxImageCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxImageCount" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, maxImageCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageCount" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageCount}

instance {-# OVERLAPPING #-}
         HasField "currentExtent" VkSurfaceCapabilitiesKHR where
        type FieldType "currentExtent" VkSurfaceCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "currentExtent" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "currentExtent" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, currentExtent}
        type FieldIsArray "currentExtent" VkSurfaceCapabilitiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, currentExtent}

instance {-# OVERLAPPING #-}
         CanReadField "currentExtent" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, currentExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, currentExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "currentExtent" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, currentExtent}

instance {-# OVERLAPPING #-}
         HasField "minImageExtent" VkSurfaceCapabilitiesKHR where
        type FieldType "minImageExtent" VkSurfaceCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "minImageExtent" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "minImageExtent" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, minImageExtent}
        type FieldIsArray "minImageExtent" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, minImageExtent}

instance {-# OVERLAPPING #-}
         CanReadField "minImageExtent" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, minImageExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "minImageExtent" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, minImageExtent}

instance {-# OVERLAPPING #-}
         HasField "maxImageExtent" VkSurfaceCapabilitiesKHR where
        type FieldType "maxImageExtent" VkSurfaceCapabilitiesKHR =
             VkExtent2D
        type FieldOptional "maxImageExtent" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageExtent" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}
        type FieldIsArray "maxImageExtent" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}

instance {-# OVERLAPPING #-}
         CanReadField "maxImageExtent" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, maxImageExtent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageExtent" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageExtent}

instance {-# OVERLAPPING #-}
         HasField "maxImageArrayLayers" VkSurfaceCapabilitiesKHR where
        type FieldType "maxImageArrayLayers" VkSurfaceCapabilitiesKHR =
             Word32
        type FieldOptional "maxImageArrayLayers" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "maxImageArrayLayers" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}
        type FieldIsArray "maxImageArrayLayers" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         CanReadField "maxImageArrayLayers" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         CanWriteField "maxImageArrayLayers" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, maxImageArrayLayers}

instance {-# OVERLAPPING #-}
         HasField "supportedTransforms" VkSurfaceCapabilitiesKHR where
        type FieldType "supportedTransforms" VkSurfaceCapabilitiesKHR =
             VkSurfaceTransformFlagsKHR
        type FieldOptional "supportedTransforms" VkSurfaceCapabilitiesKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedTransforms" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}
        type FieldIsArray "supportedTransforms" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         CanReadField "supportedTransforms" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, supportedTransforms})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedTransforms" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedTransforms}

instance {-# OVERLAPPING #-}
         HasField "currentTransform" VkSurfaceCapabilitiesKHR where
        type FieldType "currentTransform" VkSurfaceCapabilitiesKHR =
             VkSurfaceTransformFlagBitsKHR
        type FieldOptional "currentTransform" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "currentTransform" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, currentTransform}
        type FieldIsArray "currentTransform" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, currentTransform}

instance {-# OVERLAPPING #-}
         CanReadField "currentTransform" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, currentTransform})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, currentTransform}

instance {-# OVERLAPPING #-}
         CanWriteField "currentTransform" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, currentTransform}

instance {-# OVERLAPPING #-}
         HasField "supportedCompositeAlpha" VkSurfaceCapabilitiesKHR where
        type FieldType "supportedCompositeAlpha" VkSurfaceCapabilitiesKHR =
             VkCompositeAlphaFlagsKHR
        type FieldOptional "supportedCompositeAlpha"
               VkSurfaceCapabilitiesKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedCompositeAlpha" VkSurfaceCapabilitiesKHR
             =
             #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}
        type FieldIsArray "supportedCompositeAlpha"
               VkSurfaceCapabilitiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}

instance {-# OVERLAPPING #-}
         CanReadField "supportedCompositeAlpha" VkSurfaceCapabilitiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedCompositeAlpha" VkSurfaceCapabilitiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedCompositeAlpha}

instance {-# OVERLAPPING #-}
         HasField "supportedUsageFlags" VkSurfaceCapabilitiesKHR where
        type FieldType "supportedUsageFlags" VkSurfaceCapabilitiesKHR =
             VkImageUsageFlags
        type FieldOptional "supportedUsageFlags" VkSurfaceCapabilitiesKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "supportedUsageFlags" VkSurfaceCapabilitiesKHR =
             #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}
        type FieldIsArray "supportedUsageFlags" VkSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}

instance {-# OVERLAPPING #-}
         CanReadField "supportedUsageFlags" VkSurfaceCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedUsageFlags" VkSurfaceCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSurfaceCapabilitiesKHR, supportedUsageFlags}

instance Show VkSurfaceCapabilitiesKHR where
        showsPrec d x
          = showString "VkSurfaceCapabilitiesKHR {" .
              showString "minImageCount = " .
                showsPrec d (getField @"minImageCount" x) .
                  showString ", " .
                    showString "maxImageCount = " .
                      showsPrec d (getField @"maxImageCount" x) .
                        showString ", " .
                          showString "currentExtent = " .
                            showsPrec d (getField @"currentExtent" x) .
                              showString ", " .
                                showString "minImageExtent = " .
                                  showsPrec d (getField @"minImageExtent" x) .
                                    showString ", " .
                                      showString "maxImageExtent = " .
                                        showsPrec d (getField @"maxImageExtent" x) .
                                          showString ", " .
                                            showString "maxImageArrayLayers = " .
                                              showsPrec d (getField @"maxImageArrayLayers" x) .
                                                showString ", " .
                                                  showString "supportedTransforms = " .
                                                    showsPrec d (getField @"supportedTransforms" x)
                                                      .
                                                      showString ", " .
                                                        showString "currentTransform = " .
                                                          showsPrec d
                                                            (getField @"currentTransform" x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "supportedCompositeAlpha = "
                                                                .
                                                                showsPrec d
                                                                  (getField
                                                                     @"supportedCompositeAlpha"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "supportedUsageFlags = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"supportedUsageFlags"
                                                                           x)
                                                                        . showChar '}'
