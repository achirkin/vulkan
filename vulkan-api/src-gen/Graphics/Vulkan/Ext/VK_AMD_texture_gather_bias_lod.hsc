#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_AMD_texture_gather_bias_lod
       (-- * Vulkan extension: @VK_AMD_texture_gather_bias_lod@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Rex Xu @amdrexu@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @42@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2'.
        VkTextureLODGatherFormatPropertiesAMD(..),
        VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION,
        pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION,
        VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME,
        pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Common           (VkBool32,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

import Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2

-- | > typedef struct VkTextureLODGatherFormatPropertiesAMD {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         supportsTextureGatherLODBiasAMD;
--   > } VkTextureLODGatherFormatPropertiesAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkTextureLODGatherFormatPropertiesAMD.html VkTextureLODGatherFormatPropertiesAMD registry at www.khronos.org>
data VkTextureLODGatherFormatPropertiesAMD = VkTextureLODGatherFormatPropertiesAMD## Addr##
                                                                                    ByteArray##

instance Eq VkTextureLODGatherFormatPropertiesAMD where
        (VkTextureLODGatherFormatPropertiesAMD## a _) ==
          x@(VkTextureLODGatherFormatPropertiesAMD## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkTextureLODGatherFormatPropertiesAMD where
        (VkTextureLODGatherFormatPropertiesAMD## a _) `compare`
          x@(VkTextureLODGatherFormatPropertiesAMD## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkTextureLODGatherFormatPropertiesAMD where
        sizeOf ~_
          = #{size VkTextureLODGatherFormatPropertiesAMD}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkTextureLODGatherFormatPropertiesAMD}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkTextureLODGatherFormatPropertiesAMD
         where
        unsafeAddr (VkTextureLODGatherFormatPropertiesAMD## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkTextureLODGatherFormatPropertiesAMD## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkTextureLODGatherFormatPropertiesAMD##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkTextureLODGatherFormatPropertiesAMD where
        type StructFields VkTextureLODGatherFormatPropertiesAMD =
             '["sType", "pNext", "supportsTextureGatherLODBiasAMD"] -- ' closing tick for hsc2hs
        type CUnionType VkTextureLODGatherFormatPropertiesAMD = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkTextureLODGatherFormatPropertiesAMD = 'True -- ' closing tick for hsc2hs
        type StructExtends VkTextureLODGatherFormatPropertiesAMD =
             '[VkImageFormatProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkTextureLODGatherFormatPropertiesAMD where
        type VkSTypeMType VkTextureLODGatherFormatPropertiesAMD =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkTextureLODGatherFormatPropertiesAMD, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkTextureLODGatherFormatPropertiesAMD, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkTextureLODGatherFormatPropertiesAMD where
        type FieldType "sType" VkTextureLODGatherFormatPropertiesAMD =
             VkStructureType
        type FieldOptional "sType" VkTextureLODGatherFormatPropertiesAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkTextureLODGatherFormatPropertiesAMD =
             #{offset VkTextureLODGatherFormatPropertiesAMD, sType}
        type FieldIsArray "sType" VkTextureLODGatherFormatPropertiesAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkTextureLODGatherFormatPropertiesAMD, sType}

instance CanReadField "sType" VkTextureLODGatherFormatPropertiesAMD
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkTextureLODGatherFormatPropertiesAMD where
        type VkPNextMType VkTextureLODGatherFormatPropertiesAMD = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkTextureLODGatherFormatPropertiesAMD, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkTextureLODGatherFormatPropertiesAMD, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkTextureLODGatherFormatPropertiesAMD where
        type FieldType "pNext" VkTextureLODGatherFormatPropertiesAMD =
             Ptr Void
        type FieldOptional "pNext" VkTextureLODGatherFormatPropertiesAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkTextureLODGatherFormatPropertiesAMD =
             #{offset VkTextureLODGatherFormatPropertiesAMD, pNext}
        type FieldIsArray "pNext" VkTextureLODGatherFormatPropertiesAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkTextureLODGatherFormatPropertiesAMD, pNext}

instance CanReadField "pNext" VkTextureLODGatherFormatPropertiesAMD
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkSupportsTextureGatherLODBiasAMD
           VkTextureLODGatherFormatPropertiesAMD
         where
        type VkSupportsTextureGatherLODBiasAMDMType
               VkTextureLODGatherFormatPropertiesAMD
             = VkBool32

        {-# NOINLINE vkSupportsTextureGatherLODBiasAMD #-}
        vkSupportsTextureGatherLODBiasAMD x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkTextureLODGatherFormatPropertiesAMD, supportsTextureGatherLODBiasAMD})

        {-# INLINE vkSupportsTextureGatherLODBiasAMDByteOffset #-}
        vkSupportsTextureGatherLODBiasAMDByteOffset ~_
          = #{offset VkTextureLODGatherFormatPropertiesAMD, supportsTextureGatherLODBiasAMD}

        {-# INLINE readVkSupportsTextureGatherLODBiasAMD #-}
        readVkSupportsTextureGatherLODBiasAMD p
          = peekByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, supportsTextureGatherLODBiasAMD}

        {-# INLINE writeVkSupportsTextureGatherLODBiasAMD #-}
        writeVkSupportsTextureGatherLODBiasAMD p
          = pokeByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, supportsTextureGatherLODBiasAMD}

instance {-# OVERLAPPING #-}
         HasField "supportsTextureGatherLODBiasAMD"
           VkTextureLODGatherFormatPropertiesAMD
         where
        type FieldType "supportsTextureGatherLODBiasAMD"
               VkTextureLODGatherFormatPropertiesAMD
             = VkBool32
        type FieldOptional "supportsTextureGatherLODBiasAMD"
               VkTextureLODGatherFormatPropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "supportsTextureGatherLODBiasAMD"
               VkTextureLODGatherFormatPropertiesAMD
             =
             #{offset VkTextureLODGatherFormatPropertiesAMD, supportsTextureGatherLODBiasAMD}
        type FieldIsArray "supportsTextureGatherLODBiasAMD"
               VkTextureLODGatherFormatPropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkTextureLODGatherFormatPropertiesAMD, supportsTextureGatherLODBiasAMD}

instance CanReadField "supportsTextureGatherLODBiasAMD"
           VkTextureLODGatherFormatPropertiesAMD
         where
        {-# INLINE getField #-}
        getField = vkSupportsTextureGatherLODBiasAMD

        {-# INLINE readField #-}
        readField = readVkSupportsTextureGatherLODBiasAMD

instance Show VkTextureLODGatherFormatPropertiesAMD where
        showsPrec d x
          = showString "VkTextureLODGatherFormatPropertiesAMD {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSupportsTextureGatherLODBiasAMD = " .
                            showsPrec d (vkSupportsTextureGatherLODBiasAMD x) . showChar '}'

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = 1

type VK_AMD_TEXTURE_GATHER_BIAS_LOD_SPEC_VERSION = 1

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: CString

pattern VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME <-
        (is_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME -> True)
  where VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
          = _VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME

{-# INLINE _VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME #-}

_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: CString
_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  = Ptr "VK_AMD_texture_gather_bias_lod\NUL"##

{-# INLINE is_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME #-}

is_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME
  = eqCStrings _VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME

type VK_AMD_TEXTURE_GATHER_BIAS_LOD_EXTENSION_NAME =
     "VK_AMD_texture_gather_bias_lod"

pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
        = VkStructureType 1000041000
