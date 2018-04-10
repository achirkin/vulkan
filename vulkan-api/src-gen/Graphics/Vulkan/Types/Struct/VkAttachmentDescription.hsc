#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAttachmentDescription
       (VkAttachmentDescription(..)) where
import           Foreign.Storable                                        (Storable (..))
import           GHC.Base                                                (Addr##, ByteArray##,
                                                                          byteArrayContents##,
                                                                          plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkAttachmentDescriptionFlags (VkAttachmentDescriptionFlags)
import           Graphics.Vulkan.Types.Enum.VkAttachmentLoadOp           (VkAttachmentLoadOp)
import           Graphics.Vulkan.Types.Enum.VkAttachmentStoreOp          (VkAttachmentStoreOp)
import           Graphics.Vulkan.Types.Enum.VkFormat                     (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageLayout                (VkImageLayout)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags           (VkSampleCountFlagBits)
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

-- | > typedef struct VkAttachmentDescription {
--   >     VkAttachmentDescriptionFlags flags;
--   >     VkFormat               format;
--   >     VkSampleCountFlagBits  samples;
--   >     VkAttachmentLoadOp     loadOp;
--   >     VkAttachmentStoreOp    storeOp;
--   >     VkAttachmentLoadOp     stencilLoadOp;
--   >     VkAttachmentStoreOp    stencilStoreOp;
--   >     VkImageLayout          initialLayout;
--   >     VkImageLayout          finalLayout;
--   > } VkAttachmentDescription;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkAttachmentDescription VkAttachmentDescription registry at www.khronos.org>
data VkAttachmentDescription = VkAttachmentDescription## Addr##
                                                        ByteArray##

instance Eq VkAttachmentDescription where
        (VkAttachmentDescription## a _) == x@(VkAttachmentDescription## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAttachmentDescription where
        (VkAttachmentDescription## a _) `compare`
          x@(VkAttachmentDescription## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAttachmentDescription where
        sizeOf ~_ = #{size VkAttachmentDescription}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkAttachmentDescription}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAttachmentDescription where
        unsafeAddr (VkAttachmentDescription## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAttachmentDescription## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAttachmentDescription## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAttachmentDescription where
        type StructFields VkAttachmentDescription =
             '["flags", "format", "samples", "loadOp", "storeOp", -- ' closing tick for hsc2hs
               "stencilLoadOp", "stencilStoreOp", "initialLayout", "finalLayout"]
        type CUnionType VkAttachmentDescription = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAttachmentDescription = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAttachmentDescription = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "flags" VkAttachmentDescription where
        type FieldType "flags" VkAttachmentDescription =
             VkAttachmentDescriptionFlags
        type FieldOptional "flags" VkAttachmentDescription = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkAttachmentDescription =
             #{offset VkAttachmentDescription, flags}
        type FieldIsArray "flags" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkAttachmentDescription, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkAttachmentDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentDescription, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentDescription, flags}

instance {-# OVERLAPPING #-}
         HasField "format" VkAttachmentDescription where
        type FieldType "format" VkAttachmentDescription = VkFormat
        type FieldOptional "format" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkAttachmentDescription =
             #{offset VkAttachmentDescription, format}
        type FieldIsArray "format" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkAttachmentDescription, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkAttachmentDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentDescription, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentDescription, format}

instance {-# OVERLAPPING #-}
         HasField "samples" VkAttachmentDescription where
        type FieldType "samples" VkAttachmentDescription =
             VkSampleCountFlagBits
        type FieldOptional "samples" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs
        type FieldOffset "samples" VkAttachmentDescription =
             #{offset VkAttachmentDescription, samples}
        type FieldIsArray "samples" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentDescription, samples}

instance {-# OVERLAPPING #-}
         CanReadField "samples" VkAttachmentDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, samples})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentDescription, samples}

instance {-# OVERLAPPING #-}
         CanWriteField "samples" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentDescription, samples}

instance {-# OVERLAPPING #-}
         HasField "loadOp" VkAttachmentDescription where
        type FieldType "loadOp" VkAttachmentDescription =
             VkAttachmentLoadOp
        type FieldOptional "loadOp" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs
        type FieldOffset "loadOp" VkAttachmentDescription =
             #{offset VkAttachmentDescription, loadOp}
        type FieldIsArray "loadOp" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkAttachmentDescription, loadOp}

instance {-# OVERLAPPING #-}
         CanReadField "loadOp" VkAttachmentDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, loadOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentDescription, loadOp}

instance {-# OVERLAPPING #-}
         CanWriteField "loadOp" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentDescription, loadOp}

instance {-# OVERLAPPING #-}
         HasField "storeOp" VkAttachmentDescription where
        type FieldType "storeOp" VkAttachmentDescription =
             VkAttachmentStoreOp
        type FieldOptional "storeOp" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs
        type FieldOffset "storeOp" VkAttachmentDescription =
             #{offset VkAttachmentDescription, storeOp}
        type FieldIsArray "storeOp" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentDescription, storeOp}

instance {-# OVERLAPPING #-}
         CanReadField "storeOp" VkAttachmentDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, storeOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentDescription, storeOp}

instance {-# OVERLAPPING #-}
         CanWriteField "storeOp" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentDescription, storeOp}

instance {-# OVERLAPPING #-}
         HasField "stencilLoadOp" VkAttachmentDescription where
        type FieldType "stencilLoadOp" VkAttachmentDescription =
             VkAttachmentLoadOp
        type FieldOptional "stencilLoadOp" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stencilLoadOp" VkAttachmentDescription =
             #{offset VkAttachmentDescription, stencilLoadOp}
        type FieldIsArray "stencilLoadOp" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentDescription, stencilLoadOp}

instance {-# OVERLAPPING #-}
         CanReadField "stencilLoadOp" VkAttachmentDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, stencilLoadOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentDescription, stencilLoadOp}

instance {-# OVERLAPPING #-}
         CanWriteField "stencilLoadOp" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentDescription, stencilLoadOp}

instance {-# OVERLAPPING #-}
         HasField "stencilStoreOp" VkAttachmentDescription where
        type FieldType "stencilStoreOp" VkAttachmentDescription =
             VkAttachmentStoreOp
        type FieldOptional "stencilStoreOp" VkAttachmentDescription =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "stencilStoreOp" VkAttachmentDescription =
             #{offset VkAttachmentDescription, stencilStoreOp}
        type FieldIsArray "stencilStoreOp" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentDescription, stencilStoreOp}

instance {-# OVERLAPPING #-}
         CanReadField "stencilStoreOp" VkAttachmentDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, stencilStoreOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentDescription, stencilStoreOp}

instance {-# OVERLAPPING #-}
         CanWriteField "stencilStoreOp" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentDescription, stencilStoreOp}

instance {-# OVERLAPPING #-}
         HasField "initialLayout" VkAttachmentDescription where
        type FieldType "initialLayout" VkAttachmentDescription =
             VkImageLayout
        type FieldOptional "initialLayout" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs
        type FieldOffset "initialLayout" VkAttachmentDescription =
             #{offset VkAttachmentDescription, initialLayout}
        type FieldIsArray "initialLayout" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentDescription, initialLayout}

instance {-# OVERLAPPING #-}
         CanReadField "initialLayout" VkAttachmentDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, initialLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentDescription, initialLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "initialLayout" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentDescription, initialLayout}

instance {-# OVERLAPPING #-}
         HasField "finalLayout" VkAttachmentDescription where
        type FieldType "finalLayout" VkAttachmentDescription =
             VkImageLayout
        type FieldOptional "finalLayout" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs
        type FieldOffset "finalLayout" VkAttachmentDescription =
             #{offset VkAttachmentDescription, finalLayout}
        type FieldIsArray "finalLayout" VkAttachmentDescription = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentDescription, finalLayout}

instance {-# OVERLAPPING #-}
         CanReadField "finalLayout" VkAttachmentDescription where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, finalLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentDescription, finalLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "finalLayout" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentDescription, finalLayout}

instance Show VkAttachmentDescription where
        showsPrec d x
          = showString "VkAttachmentDescription {" .
              showString "flags = " .
                showsPrec d (getField @"flags" x) .
                  showString ", " .
                    showString "format = " .
                      showsPrec d (getField @"format" x) .
                        showString ", " .
                          showString "samples = " .
                            showsPrec d (getField @"samples" x) .
                              showString ", " .
                                showString "loadOp = " .
                                  showsPrec d (getField @"loadOp" x) .
                                    showString ", " .
                                      showString "storeOp = " .
                                        showsPrec d (getField @"storeOp" x) .
                                          showString ", " .
                                            showString "stencilLoadOp = " .
                                              showsPrec d (getField @"stencilLoadOp" x) .
                                                showString ", " .
                                                  showString "stencilStoreOp = " .
                                                    showsPrec d (getField @"stencilStoreOp" x) .
                                                      showString ", " .
                                                        showString "initialLayout = " .
                                                          showsPrec d (getField @"initialLayout" x)
                                                            .
                                                            showString ", " .
                                                              showString "finalLayout = " .
                                                                showsPrec d
                                                                  (getField @"finalLayout" x)
                                                                  . showChar '}'
