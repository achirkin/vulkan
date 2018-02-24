#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkTextureLODGatherFormatPropertiesAMD
       (VkTextureLODGatherFormatPropertiesAMD(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2KHR (VkImageFormatProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkTextureLODGatherFormatPropertiesAMD {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         supportsTextureGatherLODBiasAMD;
--   > } VkTextureLODGatherFormatPropertiesAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkTextureLODGatherFormatPropertiesAMD.html VkTextureLODGatherFormatPropertiesAMD registry at www.khronos.org>
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

instance CanWriteField "sType"
           VkTextureLODGatherFormatPropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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

instance CanWriteField "pNext"
           VkTextureLODGatherFormatPropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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

instance CanWriteField "supportsTextureGatherLODBiasAMD"
           VkTextureLODGatherFormatPropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkSupportsTextureGatherLODBiasAMD

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
