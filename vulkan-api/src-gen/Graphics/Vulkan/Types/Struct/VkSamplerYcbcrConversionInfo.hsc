#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionInfo
       (VkSamplerYcbcrConversionInfo(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Handles                      (VkSamplerYcbcrConversion)
import           Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo (VkImageViewCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo   (VkSamplerCreateInfo)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkSamplerYcbcrConversionInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSamplerYcbcrConversion      conversion;
--   > } VkSamplerYcbcrConversionInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSamplerYcbcrConversionInfo.html VkSamplerYcbcrConversionInfo registry at www.khronos.org>
data VkSamplerYcbcrConversionInfo = VkSamplerYcbcrConversionInfo## Addr##
                                                                  ByteArray##

instance Eq VkSamplerYcbcrConversionInfo where
        (VkSamplerYcbcrConversionInfo## a _) ==
          x@(VkSamplerYcbcrConversionInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionInfo where
        (VkSamplerYcbcrConversionInfo## a _) `compare`
          x@(VkSamplerYcbcrConversionInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionInfo where
        sizeOf ~_ = #{size VkSamplerYcbcrConversionInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSamplerYcbcrConversionInfo where
        unsafeAddr (VkSamplerYcbcrConversionInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSamplerYcbcrConversionInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerYcbcrConversionInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSamplerYcbcrConversionInfo where
        type StructFields VkSamplerYcbcrConversionInfo =
             '["sType", "pNext", "conversion"] -- ' closing tick for hsc2hs
        type CUnionType VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSamplerYcbcrConversionInfo =
             '[VkSamplerCreateInfo, VkImageViewCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerYcbcrConversionInfo where
        type FieldType "sType" VkSamplerYcbcrConversionInfo =
             VkStructureType
        type FieldOptional "sType" VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSamplerYcbcrConversionInfo =
             #{offset VkSamplerYcbcrConversionInfo, sType}
        type FieldIsArray "sType" VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSamplerYcbcrConversionInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSamplerYcbcrConversionInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerYcbcrConversionInfo where
        type FieldType "pNext" VkSamplerYcbcrConversionInfo = Ptr Void
        type FieldOptional "pNext" VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSamplerYcbcrConversionInfo =
             #{offset VkSamplerYcbcrConversionInfo, pNext}
        type FieldIsArray "pNext" VkSamplerYcbcrConversionInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSamplerYcbcrConversionInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSamplerYcbcrConversionInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "conversion" VkSamplerYcbcrConversionInfo where
        type FieldType "conversion" VkSamplerYcbcrConversionInfo =
             VkSamplerYcbcrConversion
        type FieldOptional "conversion" VkSamplerYcbcrConversionInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "conversion" VkSamplerYcbcrConversionInfo =
             #{offset VkSamplerYcbcrConversionInfo, conversion}
        type FieldIsArray "conversion" VkSamplerYcbcrConversionInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionInfo, conversion}

instance {-# OVERLAPPING #-}
         CanReadField "conversion" VkSamplerYcbcrConversionInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfo, conversion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfo, conversion}

instance {-# OVERLAPPING #-}
         CanWriteField "conversion" VkSamplerYcbcrConversionInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfo, conversion}

instance Show VkSamplerYcbcrConversionInfo where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "conversion = " .
                            showsPrec d (getField @"conversion" x) . showChar '}'
