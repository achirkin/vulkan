#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
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
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkBool32,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkTextureLODGatherFormatPropertiesAMD {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         supportsTextureGatherLODBiasAMD;
--   > } VkTextureLODGatherFormatPropertiesAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkTextureLODGatherFormatPropertiesAMD.html VkTextureLODGatherFormatPropertiesAMD registry at www.khronos.org>
data VkTextureLODGatherFormatPropertiesAMD = VkTextureLODGatherFormatPropertiesAMD## ByteArray##

instance Eq VkTextureLODGatherFormatPropertiesAMD where
        (VkTextureLODGatherFormatPropertiesAMD## a) ==
          (VkTextureLODGatherFormatPropertiesAMD## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkTextureLODGatherFormatPropertiesAMD where
        (VkTextureLODGatherFormatPropertiesAMD## a) `compare`
          (VkTextureLODGatherFormatPropertiesAMD## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkTextureLODGatherFormatPropertiesAMD where
        sizeOf ~_
          = #{size VkTextureLODGatherFormatPropertiesAMD}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkTextureLODGatherFormatPropertiesAMD}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkTextureLODGatherFormatPropertiesAMD),
            I## a <- alignment
                      (undefined :: VkTextureLODGatherFormatPropertiesAMD)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkTextureLODGatherFormatPropertiesAMD##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkTextureLODGatherFormatPropertiesAMD## ba)
          | I## n <- sizeOf
                      (undefined :: VkTextureLODGatherFormatPropertiesAMD)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkTextureLODGatherFormatPropertiesAMD where
        type StructFields VkTextureLODGatherFormatPropertiesAMD =
             '["sType", "pNext", "supportsTextureGatherLODBiasAMD"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkTextureLODGatherFormatPropertiesAMD),
            I## a <- alignment
                      (undefined :: VkTextureLODGatherFormatPropertiesAMD)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkTextureLODGatherFormatPropertiesAMD##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkTextureLODGatherFormatPropertiesAMD## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkTextureLODGatherFormatPropertiesAMD##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkTextureLODGatherFormatPropertiesAMD## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkTextureLODGatherFormatPropertiesAMD## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkTextureLODGatherFormatPropertiesAMD## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

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
