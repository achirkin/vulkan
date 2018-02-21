#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAttachmentDescription
       (VkAttachmentDescription(..)) where
import           Foreign.Storable                                        (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkAttachmentDescriptionFlags (VkAttachmentDescriptionFlags)
import           Graphics.Vulkan.Types.Enum.VkAttachmentLoadOp           (VkAttachmentLoadOp)
import           Graphics.Vulkan.Types.Enum.VkAttachmentStoreOp          (VkAttachmentStoreOp)
import           Graphics.Vulkan.Types.Enum.VkFormat                     (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageLayout                (VkImageLayout)
import           Graphics.Vulkan.Types.Enum.VkSampleCountFlags           (VkSampleCountFlagBits)
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkAttachmentDescription.html VkAttachmentDescription registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkFlags VkAttachmentDescription
         where
        type VkFlagsMType VkAttachmentDescription =
             VkAttachmentDescriptionFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkAttachmentDescription, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkAttachmentDescription, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkAttachmentDescription, flags}

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

instance CanReadField "flags" VkAttachmentDescription where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkFormat VkAttachmentDescription
         where
        type VkFormatMType VkAttachmentDescription = VkFormat

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkAttachmentDescription, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkAttachmentDescription, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkAttachmentDescription, format}

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

instance CanReadField "format" VkAttachmentDescription where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-} HasVkSamples VkAttachmentDescription
         where
        type VkSamplesMType VkAttachmentDescription = VkSampleCountFlagBits

        {-# NOINLINE vkSamples #-}
        vkSamples x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, samples})

        {-# INLINE vkSamplesByteOffset #-}
        vkSamplesByteOffset ~_
          = #{offset VkAttachmentDescription, samples}

        {-# INLINE readVkSamples #-}
        readVkSamples p
          = peekByteOff p #{offset VkAttachmentDescription, samples}

        {-# INLINE writeVkSamples #-}
        writeVkSamples p
          = pokeByteOff p #{offset VkAttachmentDescription, samples}

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

instance CanReadField "samples" VkAttachmentDescription where
        {-# INLINE getField #-}
        getField = vkSamples

        {-# INLINE readField #-}
        readField = readVkSamples

instance CanWriteField "samples" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField = writeVkSamples

instance {-# OVERLAPPING #-} HasVkLoadOp VkAttachmentDescription
         where
        type VkLoadOpMType VkAttachmentDescription = VkAttachmentLoadOp

        {-# NOINLINE vkLoadOp #-}
        vkLoadOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, loadOp})

        {-# INLINE vkLoadOpByteOffset #-}
        vkLoadOpByteOffset ~_
          = #{offset VkAttachmentDescription, loadOp}

        {-# INLINE readVkLoadOp #-}
        readVkLoadOp p
          = peekByteOff p #{offset VkAttachmentDescription, loadOp}

        {-# INLINE writeVkLoadOp #-}
        writeVkLoadOp p
          = pokeByteOff p #{offset VkAttachmentDescription, loadOp}

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

instance CanReadField "loadOp" VkAttachmentDescription where
        {-# INLINE getField #-}
        getField = vkLoadOp

        {-# INLINE readField #-}
        readField = readVkLoadOp

instance CanWriteField "loadOp" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField = writeVkLoadOp

instance {-# OVERLAPPING #-} HasVkStoreOp VkAttachmentDescription
         where
        type VkStoreOpMType VkAttachmentDescription = VkAttachmentStoreOp

        {-# NOINLINE vkStoreOp #-}
        vkStoreOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, storeOp})

        {-# INLINE vkStoreOpByteOffset #-}
        vkStoreOpByteOffset ~_
          = #{offset VkAttachmentDescription, storeOp}

        {-# INLINE readVkStoreOp #-}
        readVkStoreOp p
          = peekByteOff p #{offset VkAttachmentDescription, storeOp}

        {-# INLINE writeVkStoreOp #-}
        writeVkStoreOp p
          = pokeByteOff p #{offset VkAttachmentDescription, storeOp}

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

instance CanReadField "storeOp" VkAttachmentDescription where
        {-# INLINE getField #-}
        getField = vkStoreOp

        {-# INLINE readField #-}
        readField = readVkStoreOp

instance CanWriteField "storeOp" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField = writeVkStoreOp

instance {-# OVERLAPPING #-}
         HasVkStencilLoadOp VkAttachmentDescription where
        type VkStencilLoadOpMType VkAttachmentDescription =
             VkAttachmentLoadOp

        {-# NOINLINE vkStencilLoadOp #-}
        vkStencilLoadOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, stencilLoadOp})

        {-# INLINE vkStencilLoadOpByteOffset #-}
        vkStencilLoadOpByteOffset ~_
          = #{offset VkAttachmentDescription, stencilLoadOp}

        {-# INLINE readVkStencilLoadOp #-}
        readVkStencilLoadOp p
          = peekByteOff p #{offset VkAttachmentDescription, stencilLoadOp}

        {-# INLINE writeVkStencilLoadOp #-}
        writeVkStencilLoadOp p
          = pokeByteOff p #{offset VkAttachmentDescription, stencilLoadOp}

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

instance CanReadField "stencilLoadOp" VkAttachmentDescription where
        {-# INLINE getField #-}
        getField = vkStencilLoadOp

        {-# INLINE readField #-}
        readField = readVkStencilLoadOp

instance CanWriteField "stencilLoadOp" VkAttachmentDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkStencilLoadOp

instance {-# OVERLAPPING #-}
         HasVkStencilStoreOp VkAttachmentDescription where
        type VkStencilStoreOpMType VkAttachmentDescription =
             VkAttachmentStoreOp

        {-# NOINLINE vkStencilStoreOp #-}
        vkStencilStoreOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, stencilStoreOp})

        {-# INLINE vkStencilStoreOpByteOffset #-}
        vkStencilStoreOpByteOffset ~_
          = #{offset VkAttachmentDescription, stencilStoreOp}

        {-# INLINE readVkStencilStoreOp #-}
        readVkStencilStoreOp p
          = peekByteOff p #{offset VkAttachmentDescription, stencilStoreOp}

        {-# INLINE writeVkStencilStoreOp #-}
        writeVkStencilStoreOp p
          = pokeByteOff p #{offset VkAttachmentDescription, stencilStoreOp}

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

instance CanReadField "stencilStoreOp" VkAttachmentDescription
         where
        {-# INLINE getField #-}
        getField = vkStencilStoreOp

        {-# INLINE readField #-}
        readField = readVkStencilStoreOp

instance CanWriteField "stencilStoreOp" VkAttachmentDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkStencilStoreOp

instance {-# OVERLAPPING #-}
         HasVkInitialLayout VkAttachmentDescription where
        type VkInitialLayoutMType VkAttachmentDescription = VkImageLayout

        {-# NOINLINE vkInitialLayout #-}
        vkInitialLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, initialLayout})

        {-# INLINE vkInitialLayoutByteOffset #-}
        vkInitialLayoutByteOffset ~_
          = #{offset VkAttachmentDescription, initialLayout}

        {-# INLINE readVkInitialLayout #-}
        readVkInitialLayout p
          = peekByteOff p #{offset VkAttachmentDescription, initialLayout}

        {-# INLINE writeVkInitialLayout #-}
        writeVkInitialLayout p
          = pokeByteOff p #{offset VkAttachmentDescription, initialLayout}

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

instance CanReadField "initialLayout" VkAttachmentDescription where
        {-# INLINE getField #-}
        getField = vkInitialLayout

        {-# INLINE readField #-}
        readField = readVkInitialLayout

instance CanWriteField "initialLayout" VkAttachmentDescription
         where
        {-# INLINE writeField #-}
        writeField = writeVkInitialLayout

instance {-# OVERLAPPING #-}
         HasVkFinalLayout VkAttachmentDescription where
        type VkFinalLayoutMType VkAttachmentDescription = VkImageLayout

        {-# NOINLINE vkFinalLayout #-}
        vkFinalLayout x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentDescription, finalLayout})

        {-# INLINE vkFinalLayoutByteOffset #-}
        vkFinalLayoutByteOffset ~_
          = #{offset VkAttachmentDescription, finalLayout}

        {-# INLINE readVkFinalLayout #-}
        readVkFinalLayout p
          = peekByteOff p #{offset VkAttachmentDescription, finalLayout}

        {-# INLINE writeVkFinalLayout #-}
        writeVkFinalLayout p
          = pokeByteOff p #{offset VkAttachmentDescription, finalLayout}

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

instance CanReadField "finalLayout" VkAttachmentDescription where
        {-# INLINE getField #-}
        getField = vkFinalLayout

        {-# INLINE readField #-}
        readField = readVkFinalLayout

instance CanWriteField "finalLayout" VkAttachmentDescription where
        {-# INLINE writeField #-}
        writeField = writeVkFinalLayout

instance Show VkAttachmentDescription where
        showsPrec d x
          = showString "VkAttachmentDescription {" .
              showString "vkFlags = " .
                showsPrec d (vkFlags x) .
                  showString ", " .
                    showString "vkFormat = " .
                      showsPrec d (vkFormat x) .
                        showString ", " .
                          showString "vkSamples = " .
                            showsPrec d (vkSamples x) .
                              showString ", " .
                                showString "vkLoadOp = " .
                                  showsPrec d (vkLoadOp x) .
                                    showString ", " .
                                      showString "vkStoreOp = " .
                                        showsPrec d (vkStoreOp x) .
                                          showString ", " .
                                            showString "vkStencilLoadOp = " .
                                              showsPrec d (vkStencilLoadOp x) .
                                                showString ", " .
                                                  showString "vkStencilStoreOp = " .
                                                    showsPrec d (vkStencilStoreOp x) .
                                                      showString ", " .
                                                        showString "vkInitialLayout = " .
                                                          showsPrec d (vkInitialLayout x) .
                                                            showString ", " .
                                                              showString "vkFinalLayout = " .
                                                                showsPrec d (vkFinalLayout x) .
                                                                  showChar '}'
