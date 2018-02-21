#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo
       (VkImageViewCreateInfo(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                       (VkImageViewCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkFormat                  (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageViewType           (VkImageViewType)
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Handles                        (VkImage)
import           Graphics.Vulkan.Types.Struct.VkComponentMapping      (VkComponentMapping)
import           Graphics.Vulkan.Types.Struct.VkImageSubresourceRange (VkImageSubresourceRange)
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageViewCreateInfo.html VkImageViewCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkImageViewCreateInfo where
        type VkSTypeMType VkImageViewCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageViewCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageViewCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageViewCreateInfo, sType}

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

instance CanReadField "sType" VkImageViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkImageViewCreateInfo where
        type VkPNextMType VkImageViewCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageViewCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageViewCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageViewCreateInfo, pNext}

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

instance CanReadField "pNext" VkImageViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkImageViewCreateInfo where
        type VkFlagsMType VkImageViewCreateInfo = VkImageViewCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkImageViewCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkImageViewCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkImageViewCreateInfo, flags}

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

instance CanReadField "flags" VkImageViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkImage VkImageViewCreateInfo where
        type VkImageMType VkImageViewCreateInfo = VkImage

        {-# NOINLINE vkImage #-}
        vkImage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, image})

        {-# INLINE vkImageByteOffset #-}
        vkImageByteOffset ~_
          = #{offset VkImageViewCreateInfo, image}

        {-# INLINE readVkImage #-}
        readVkImage p
          = peekByteOff p #{offset VkImageViewCreateInfo, image}

        {-# INLINE writeVkImage #-}
        writeVkImage p
          = pokeByteOff p #{offset VkImageViewCreateInfo, image}

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

instance CanReadField "image" VkImageViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkImage

        {-# INLINE readField #-}
        readField = readVkImage

instance CanWriteField "image" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkImage

instance {-# OVERLAPPING #-} HasVkViewType VkImageViewCreateInfo
         where
        type VkViewTypeMType VkImageViewCreateInfo = VkImageViewType

        {-# NOINLINE vkViewType #-}
        vkViewType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, viewType})

        {-# INLINE vkViewTypeByteOffset #-}
        vkViewTypeByteOffset ~_
          = #{offset VkImageViewCreateInfo, viewType}

        {-# INLINE readVkViewType #-}
        readVkViewType p
          = peekByteOff p #{offset VkImageViewCreateInfo, viewType}

        {-# INLINE writeVkViewType #-}
        writeVkViewType p
          = pokeByteOff p #{offset VkImageViewCreateInfo, viewType}

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

instance CanReadField "viewType" VkImageViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkViewType

        {-# INLINE readField #-}
        readField = readVkViewType

instance CanWriteField "viewType" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkViewType

instance {-# OVERLAPPING #-} HasVkFormat VkImageViewCreateInfo
         where
        type VkFormatMType VkImageViewCreateInfo = VkFormat

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkImageViewCreateInfo, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkImageViewCreateInfo, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkImageViewCreateInfo, format}

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

instance CanReadField "format" VkImageViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-} HasVkComponents VkImageViewCreateInfo
         where
        type VkComponentsMType VkImageViewCreateInfo = VkComponentMapping

        {-# NOINLINE vkComponents #-}
        vkComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, components})

        {-# INLINE vkComponentsByteOffset #-}
        vkComponentsByteOffset ~_
          = #{offset VkImageViewCreateInfo, components}

        {-# INLINE readVkComponents #-}
        readVkComponents p
          = peekByteOff p #{offset VkImageViewCreateInfo, components}

        {-# INLINE writeVkComponents #-}
        writeVkComponents p
          = pokeByteOff p #{offset VkImageViewCreateInfo, components}

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

instance CanReadField "components" VkImageViewCreateInfo where
        {-# INLINE getField #-}
        getField = vkComponents

        {-# INLINE readField #-}
        readField = readVkComponents

instance CanWriteField "components" VkImageViewCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkComponents

instance {-# OVERLAPPING #-}
         HasVkSubresourceRange VkImageViewCreateInfo where
        type VkSubresourceRangeMType VkImageViewCreateInfo =
             VkImageSubresourceRange

        {-# NOINLINE vkSubresourceRange #-}
        vkSubresourceRange x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewCreateInfo, subresourceRange})

        {-# INLINE vkSubresourceRangeByteOffset #-}
        vkSubresourceRangeByteOffset ~_
          = #{offset VkImageViewCreateInfo, subresourceRange}

        {-# INLINE readVkSubresourceRange #-}
        readVkSubresourceRange p
          = peekByteOff p #{offset VkImageViewCreateInfo, subresourceRange}

        {-# INLINE writeVkSubresourceRange #-}
        writeVkSubresourceRange p
          = pokeByteOff p #{offset VkImageViewCreateInfo, subresourceRange}

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

instance CanReadField "subresourceRange" VkImageViewCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkSubresourceRange

        {-# INLINE readField #-}
        readField = readVkSubresourceRange

instance CanWriteField "subresourceRange" VkImageViewCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubresourceRange

instance Show VkImageViewCreateInfo where
        showsPrec d x
          = showString "VkImageViewCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkImage = " .
                                  showsPrec d (vkImage x) .
                                    showString ", " .
                                      showString "vkViewType = " .
                                        showsPrec d (vkViewType x) .
                                          showString ", " .
                                            showString "vkFormat = " .
                                              showsPrec d (vkFormat x) .
                                                showString ", " .
                                                  showString "vkComponents = " .
                                                    showsPrec d (vkComponents x) .
                                                      showString ", " .
                                                        showString "vkSubresourceRange = " .
                                                          showsPrec d (vkSubresourceRange x) .
                                                            showChar '}'
