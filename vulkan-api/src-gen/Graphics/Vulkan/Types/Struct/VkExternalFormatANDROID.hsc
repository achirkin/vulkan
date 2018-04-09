#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalFormatANDROID
       (VkExternalFormatANDROID(..)) where
import           Foreign.Storable                                                (Storable (..))
import           GHC.Base                                                        (Addr##,
                                                                                  ByteArray##,
                                                                                  byteArrayContents##,
                                                                                  plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                      (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo                  (VkImageCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionCreateInfo (VkSamplerYcbcrConversionCreateInfo)
import           System.IO.Unsafe                                                (unsafeDupablePerformIO)

-- | > typedef struct VkExternalFormatANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     uint64_t                           externalFormat;
--   > } VkExternalFormatANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkExternalFormatANDROIDVkExternalFormatANDROID registry at www.khronos.org>
data VkExternalFormatANDROID = VkExternalFormatANDROID## Addr##
                                                        ByteArray##

instance Eq VkExternalFormatANDROID where
        (VkExternalFormatANDROID## a _) == x@(VkExternalFormatANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalFormatANDROID where
        (VkExternalFormatANDROID## a _) `compare`
          x@(VkExternalFormatANDROID## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalFormatANDROID where
        sizeOf ~_ = #{size VkExternalFormatANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExternalFormatANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalFormatANDROID where
        unsafeAddr (VkExternalFormatANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalFormatANDROID## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalFormatANDROID## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalFormatANDROID where
        type StructFields VkExternalFormatANDROID =
             '["sType", "pNext", "externalFormat"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalFormatANDROID =
             '[VkImageCreateInfo, VkSamplerYcbcrConversionCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalFormatANDROID where
        type FieldType "sType" VkExternalFormatANDROID = VkStructureType
        type FieldOptional "sType" VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalFormatANDROID =
             #{offset VkExternalFormatANDROID, sType}
        type FieldIsArray "sType" VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExternalFormatANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalFormatANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFormatANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFormatANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalFormatANDROID where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFormatANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalFormatANDROID where
        type FieldType "pNext" VkExternalFormatANDROID = Ptr Void
        type FieldOptional "pNext" VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalFormatANDROID =
             #{offset VkExternalFormatANDROID, pNext}
        type FieldIsArray "pNext" VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExternalFormatANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalFormatANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFormatANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFormatANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalFormatANDROID where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFormatANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "externalFormat" VkExternalFormatANDROID where
        type FieldType "externalFormat" VkExternalFormatANDROID = Word64
        type FieldOptional "externalFormat" VkExternalFormatANDROID =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "externalFormat" VkExternalFormatANDROID =
             #{offset VkExternalFormatANDROID, externalFormat}
        type FieldIsArray "externalFormat" VkExternalFormatANDROID = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalFormatANDROID, externalFormat}

instance {-# OVERLAPPING #-}
         CanReadField "externalFormat" VkExternalFormatANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalFormatANDROID, externalFormat})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalFormatANDROID, externalFormat}

instance {-# OVERLAPPING #-}
         CanWriteField "externalFormat" VkExternalFormatANDROID where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalFormatANDROID, externalFormat}

instance Show VkExternalFormatANDROID where
        showsPrec d x
          = showString "VkExternalFormatANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "externalFormat = " .
                            showsPrec d (getField @"externalFormat" x) . showChar '}'
