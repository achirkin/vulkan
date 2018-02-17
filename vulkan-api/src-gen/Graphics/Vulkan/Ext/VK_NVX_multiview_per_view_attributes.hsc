#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_NVX_multiview_per_view_attributes
       (-- * Vulkan extension: @VK_NVX_multiview_per_view_attributes@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @NVX@
        --
        -- type: @device@
        --
        -- Extension number: @98@
        --
        -- Required extensions: 'VK_KHX_multiview'.
        --

        -- ** Required extensions: 'VK_KHX_multiview'.
        VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..),
        VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION,
        pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION,
        VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME,
        pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX,
        pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX,
        pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX)
       where
import           Foreign.C.String                                           (CString)
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           GHC.Ptr                                                    (Ptr (..))
import           Graphics.Vulkan.Common                                     (VkBool32,
                                                                             VkStructureType (..),
                                                                             VkSubpassDescriptionFlagBits (..))
import           Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2 (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         perViewPositionAllComponents;
--   > } VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX.html VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX registry at www.khronos.org>
data VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## Addr##
                                                                                                                        ByteArray##

instance Eq VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## a _) ==
          x@(VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## a _)
          `compare`
          x@(VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        unsafeAddr
          (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type StructFields
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = '["sType", "pNext", "perViewPositionAllComponents"] -- ' closing tick for hsc2hs
        type CUnionType
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'True -- ' closing tick for hsc2hs
        type StructExtends
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type VkSTypeMType
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type FieldType "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             =
             #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, sType}

instance CanReadField "sType"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type VkPNextMType
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type FieldType "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             =
             #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkPerViewPositionAllComponents
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type VkPerViewPositionAllComponentsMType
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = VkBool32

        {-# NOINLINE vkPerViewPositionAllComponents #-}
        vkPerViewPositionAllComponents x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents})

        {-# INLINE vkPerViewPositionAllComponentsByteOffset #-}
        vkPerViewPositionAllComponentsByteOffset ~_
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

        {-# INLINE readVkPerViewPositionAllComponents #-}
        readVkPerViewPositionAllComponents p
          = peekByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

        {-# INLINE writeVkPerViewPositionAllComponents #-}
        writeVkPerViewPositionAllComponents p
          = pokeByteOff p #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

instance {-# OVERLAPPING #-}
         HasField "perViewPositionAllComponents"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        type FieldType "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = VkBool32
        type FieldOptional "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "perViewPositionAllComponents"
               VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
             =
             #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX, perViewPositionAllComponents}

instance CanReadField "perViewPositionAllComponents"
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        {-# INLINE getField #-}
        getField = vkPerViewPositionAllComponents

        {-# INLINE readField #-}
        readField = readVkPerViewPositionAllComponents

instance Show
           VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
         where
        showsPrec d x
          = showString
              "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPerViewPositionAllComponents = " .
                            showsPrec d (vkPerViewPositionAllComponents x) . showChar '}'

pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1

type VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1

pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME ::
        CString

pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME <-
        (is_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME -> True)
  where VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
          = _VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME

{-# INLINE _VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME #-}

_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME :: CString
_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  = Ptr "VK_NVX_multiview_per_view_attributes\NUL"##

{-# INLINE is_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
           #-}

is_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME ::
                                                       CString -> Bool
is_VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  = eqCStrings _VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME

type VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME =
     "VK_NVX_multiview_per_view_attributes"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
        = VkStructureType 1000097000

-- | bitpos = @0@
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX ::
        VkSubpassDescriptionFlagBits

pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX =
        VkSubpassDescriptionFlagBits 1

-- | bitpos = @1@
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX ::
        VkSubpassDescriptionFlagBits

pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX =
        VkSubpassDescriptionFlagBits 2
