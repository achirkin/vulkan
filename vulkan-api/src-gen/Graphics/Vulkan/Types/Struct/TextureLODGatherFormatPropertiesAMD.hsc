#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.TextureLODGatherFormatPropertiesAMD
       (VkTextureLODGatherFormatPropertiesAMD(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Image       (VkImageFormatProperties2)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkTextureLODGatherFormatPropertiesAMD {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkBool32                         supportsTextureGatherLODBiasAMD;
--   > } VkTextureLODGatherFormatPropertiesAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkTextureLODGatherFormatPropertiesAMD VkTextureLODGatherFormatPropertiesAMD registry at www.khronos.org>
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
             '[VkImageFormatProperties2] -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkTextureLODGatherFormatPropertiesAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkTextureLODGatherFormatPropertiesAMD, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkTextureLODGatherFormatPropertiesAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkTextureLODGatherFormatPropertiesAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkTextureLODGatherFormatPropertiesAMD, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkTextureLODGatherFormatPropertiesAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "supportsTextureGatherLODBiasAMD"
           VkTextureLODGatherFormatPropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkTextureLODGatherFormatPropertiesAMD, supportsTextureGatherLODBiasAMD})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, supportsTextureGatherLODBiasAMD}

instance {-# OVERLAPPING #-}
         CanWriteField "supportsTextureGatherLODBiasAMD"
           VkTextureLODGatherFormatPropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkTextureLODGatherFormatPropertiesAMD, supportsTextureGatherLODBiasAMD}

instance Show VkTextureLODGatherFormatPropertiesAMD where
        showsPrec d x
          = showString "VkTextureLODGatherFormatPropertiesAMD {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "supportsTextureGatherLODBiasAMD = " .
                            showsPrec d (getField @"supportsTextureGatherLODBiasAMD" x) .
                              showChar '}'
