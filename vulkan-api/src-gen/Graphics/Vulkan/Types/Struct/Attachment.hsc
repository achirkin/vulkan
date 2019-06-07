#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Attachment
       (VkAttachmentDescription, VkAttachmentDescription', -- ' closing tick for hsc2hs
        VkAttachmentReference, VkAttachmentReference', -- ' closing tick for hsc2hs
        VkAttachmentSampleLocationsEXT, VkAttachmentSampleLocationsEXT') -- ' closing tick for hsc2hs
       where
import Foreign.Storable                            (Storable (..))
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Types.Enum.Attachment       (VkAttachmentDescriptionFlags,
                                                    VkAttachmentLoadOp,
                                                    VkAttachmentStoreOp)
import Graphics.Vulkan.Types.Enum.Format           (VkFormat)
import Graphics.Vulkan.Types.Enum.Image            (VkImageLayout)
import Graphics.Vulkan.Types.Enum.SampleCountFlags (VkSampleCountFlagBits)
import Graphics.Vulkan.Types.Struct.SampleLocation (VkSampleLocationsInfoEXT)
import System.IO.Unsafe                            (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAttachmentDescription VkAttachmentDescription registry at www.khronos.org>
type VkAttachmentDescription =
     VulkanStruct VkAttachmentDescription' -- ' closing tick for hsc2hs

data VkAttachmentDescription' -- ' closing tick for hsc2hs

instance Eq VkAttachmentDescription where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkAttachmentDescription where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

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

-- | > typedef struct VkAttachmentReference {
--   >     uint32_t               attachment;
--   >     VkImageLayout          layout;
--   > } VkAttachmentReference;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAttachmentReference VkAttachmentReference registry at www.khronos.org>
type VkAttachmentReference = VulkanStruct VkAttachmentReference' -- ' closing tick for hsc2hs

data VkAttachmentReference' -- ' closing tick for hsc2hs

instance Eq VkAttachmentReference where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkAttachmentReference where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkAttachmentReference where
        sizeOf ~_ = #{size VkAttachmentReference}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkAttachmentReference}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkAttachmentReference where
        type StructFields VkAttachmentReference = '["attachment", "layout"] -- ' closing tick for hsc2hs
        type CUnionType VkAttachmentReference = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAttachmentReference = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAttachmentReference = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "attachment" VkAttachmentReference where
        type FieldType "attachment" VkAttachmentReference = Word32
        type FieldOptional "attachment" VkAttachmentReference = 'False -- ' closing tick for hsc2hs
        type FieldOffset "attachment" VkAttachmentReference =
             #{offset VkAttachmentReference, attachment}
        type FieldIsArray "attachment" VkAttachmentReference = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentReference, attachment}

instance {-# OVERLAPPING #-}
         CanReadField "attachment" VkAttachmentReference where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentReference, attachment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentReference, attachment}

instance {-# OVERLAPPING #-}
         CanWriteField "attachment" VkAttachmentReference where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentReference, attachment}

instance {-# OVERLAPPING #-}
         HasField "layout" VkAttachmentReference where
        type FieldType "layout" VkAttachmentReference = VkImageLayout
        type FieldOptional "layout" VkAttachmentReference = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layout" VkAttachmentReference =
             #{offset VkAttachmentReference, layout}
        type FieldIsArray "layout" VkAttachmentReference = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkAttachmentReference, layout}

instance {-# OVERLAPPING #-}
         CanReadField "layout" VkAttachmentReference where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentReference, layout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentReference, layout}

instance {-# OVERLAPPING #-}
         CanWriteField "layout" VkAttachmentReference where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentReference, layout}

instance Show VkAttachmentReference where
        showsPrec d x
          = showString "VkAttachmentReference {" .
              showString "attachment = " .
                showsPrec d (getField @"attachment" x) .
                  showString ", " .
                    showString "layout = " .
                      showsPrec d (getField @"layout" x) . showChar '}'

-- | > typedef struct VkAttachmentSampleLocationsEXT {
--   >     uint32_t                         attachmentIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkAttachmentSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAttachmentSampleLocationsEXT VkAttachmentSampleLocationsEXT registry at www.khronos.org>
type VkAttachmentSampleLocationsEXT =
     VulkanStruct VkAttachmentSampleLocationsEXT' -- ' closing tick for hsc2hs

data VkAttachmentSampleLocationsEXT' -- ' closing tick for hsc2hs

instance Eq VkAttachmentSampleLocationsEXT where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkAttachmentSampleLocationsEXT where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE compare #-}

instance Storable VkAttachmentSampleLocationsEXT where
        sizeOf ~_ = #{size VkAttachmentSampleLocationsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAttachmentSampleLocationsEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshal VkAttachmentSampleLocationsEXT where
        type StructFields VkAttachmentSampleLocationsEXT =
             '["attachmentIndex", "sampleLocationsInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkAttachmentSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAttachmentSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkAttachmentSampleLocationsEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "attachmentIndex" VkAttachmentSampleLocationsEXT where
        type FieldType "attachmentIndex" VkAttachmentSampleLocationsEXT =
             Word32
        type FieldOptional "attachmentIndex" VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "attachmentIndex" VkAttachmentSampleLocationsEXT =
             #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}
        type FieldIsArray "attachmentIndex" VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

instance {-# OVERLAPPING #-}
         CanReadField "attachmentIndex" VkAttachmentSampleLocationsEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentSampleLocationsEXT, attachmentIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "attachmentIndex" VkAttachmentSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentSampleLocationsEXT, attachmentIndex}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsInfo" VkAttachmentSampleLocationsEXT where
        type FieldType "sampleLocationsInfo" VkAttachmentSampleLocationsEXT
             = VkSampleLocationsInfoEXT
        type FieldOptional "sampleLocationsInfo"
               VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsInfo"
               VkAttachmentSampleLocationsEXT
             =
             #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}
        type FieldIsArray "sampleLocationsInfo"
               VkAttachmentSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationsInfo" VkAttachmentSampleLocationsEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationsInfo" VkAttachmentSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAttachmentSampleLocationsEXT, sampleLocationsInfo}

instance Show VkAttachmentSampleLocationsEXT where
        showsPrec d x
          = showString "VkAttachmentSampleLocationsEXT {" .
              showString "attachmentIndex = " .
                showsPrec d (getField @"attachmentIndex" x) .
                  showString ", " .
                    showString "sampleLocationsInfo = " .
                      showsPrec d (getField @"sampleLocationsInfo" x) . showChar '}'
