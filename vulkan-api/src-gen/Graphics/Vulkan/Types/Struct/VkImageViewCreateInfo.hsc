#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo
       (VkImageViewCreateInfo(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Base                                             (Addr##, ByteArray##,
                                                                       byteArrayContents##,
                                                                       plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                       (VkImageViewCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkFormat                  (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageViewType           (VkImageViewType)
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Handles                        (VkImage)
import           Graphics.Vulkan.Types.Struct.VkComponentMapping      (VkComponentMapping)
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceRange (VkImageSubresourceRange)
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkImageViewCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkImageViewCreateFlags flags;
--   >     VkImage                image;
--   >     VkImageViewType        viewType;
--   >     VkFormat               format;
--   >     VkComponentMapping     components;
--   >     VkImageSubresourceRange subresourceRange;
--   > } VkImageViewCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkImageViewCreateInfo VkImageViewCreateInfo registry at www.khronos.org>
data VkImageViewCreateInfo = VkImageViewCreateInfo## Addr##
                                                    ByteArray##

instance Eq VkImageViewCreateInfo where
        (VkImageViewCreateInfo## a _) == x@(VkImageViewCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageViewCreateInfo where
        (VkImageViewCreateInfo## a _) `compare`
          x@(VkImageViewCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageViewCreateInfo where
        sizeOf ~_ = #{size VkImageViewCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageViewCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageViewCreateInfo where
        unsafeAddr (VkImageViewCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageViewCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageViewCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageViewCreateInfo where
        type StructFields VkImageViewCreateInfo =
             '["sType", "pNext", "flags", "image", "viewType", "format", -- ' closing tick for hsc2hs
               "components", "subresourceRange"]
        type CUnionType VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageViewCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkImageViewCreateInfo
         where
        type FieldType "sType" VkImageViewCreateInfo = VkStructureType
        type FieldOptional "sType" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, sType}
        type FieldIsArray "sType" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkImageViewCreateInfo
         where
        type FieldType "pNext" VkImageViewCreateInfo = Ptr Void
        type FieldOptional "pNext" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, pNext}
        type FieldIsArray "pNext" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkImageViewCreateInfo
         where
        type FieldType "flags" VkImageViewCreateInfo =
             VkImageViewCreateFlags
        type FieldOptional "flags" VkImageViewCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, flags}
        type FieldIsArray "flags" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, flags}

instance {-# OVERLAPPING #-} HasField "image" VkImageViewCreateInfo
         where
        type FieldType "image" VkImageViewCreateInfo = VkImage
        type FieldOptional "image" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "image" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, image}
        type FieldIsArray "image" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, image}

instance {-# OVERLAPPING #-}
         HasField "viewType" VkImageViewCreateInfo where
        type FieldType "viewType" VkImageViewCreateInfo = VkImageViewType
        type FieldOptional "viewType" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewType" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, viewType}
        type FieldIsArray "viewType" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, viewType}

instance {-# OVERLAPPING #-}
         CanReadField "viewType" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, viewType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, viewType}

instance {-# OVERLAPPING #-}
         CanWriteField "viewType" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, viewType}

instance {-# OVERLAPPING #-}
         HasField "format" VkImageViewCreateInfo where
        type FieldType "format" VkImageViewCreateInfo = VkFormat
        type FieldOptional "format" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, format}
        type FieldIsArray "format" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImageViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanReadField "format" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, format})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         CanWriteField "format" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, format}

instance {-# OVERLAPPING #-}
         HasField "components" VkImageViewCreateInfo where
        type FieldType "components" VkImageViewCreateInfo =
             VkComponentMapping
        type FieldOptional "components" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "components" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, components}
        type FieldIsArray "components" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewCreateInfo, components}

instance {-# OVERLAPPING #-}
         CanReadField "components" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, components})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, components}

instance {-# OVERLAPPING #-}
         CanWriteField "components" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, components}

instance {-# OVERLAPPING #-}
         HasField "subresourceRange" VkImageViewCreateInfo where
        type FieldType "subresourceRange" VkImageViewCreateInfo =
             VkImageSubresourceRange
        type FieldOptional "subresourceRange" VkImageViewCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subresourceRange" VkImageViewCreateInfo =
             #{offset VkImageViewCreateInfo, subresourceRange}
        type FieldIsArray "subresourceRange" VkImageViewCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewCreateInfo, subresourceRange}

instance {-# OVERLAPPING #-}
         CanReadField "subresourceRange" VkImageViewCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, subresourceRange})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewCreateInfo, subresourceRange}

instance {-# OVERLAPPING #-}
         CanWriteField "subresourceRange" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewCreateInfo, subresourceRange}

instance Show VkImageViewCreateInfo where
        showsPrec d x
          = showString "VkImageViewCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "image = " .
                                  showsPrec d (getField @"image" x) .
                                    showString ", " .
                                      showString "viewType = " .
                                        showsPrec d (getField @"viewType" x) .
                                          showString ", " .
                                            showString "format = " .
                                              showsPrec d (getField @"format" x) .
                                                showString ", " .
                                                  showString "components = " .
                                                    showsPrec d (getField @"components" x) .
                                                      showString ", " .
                                                        showString "subresourceRange = " .
                                                          showsPrec d
                                                            (getField @"subresourceRange" x)
                                                            . showChar '}'
