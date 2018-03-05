#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSamplerYcbcrConversionInfoKHR
       (VkSamplerYcbcrConversionInfoKHR(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Handles                      (VkSamplerYcbcrConversionKHR)
import           Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo (VkImageViewCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkSamplerCreateInfo   (VkSamplerCreateInfo)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkSamplerYcbcrConversionInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSamplerYcbcrConversionKHR      conversion;
--   > } VkSamplerYcbcrConversionInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSamplerYcbcrConversionInfoKHR.html VkSamplerYcbcrConversionInfoKHR registry at www.khronos.org>
data VkSamplerYcbcrConversionInfoKHR = VkSamplerYcbcrConversionInfoKHR## Addr##
                                                                        ByteArray##

instance Eq VkSamplerYcbcrConversionInfoKHR where
        (VkSamplerYcbcrConversionInfoKHR## a _) ==
          x@(VkSamplerYcbcrConversionInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSamplerYcbcrConversionInfoKHR where
        (VkSamplerYcbcrConversionInfoKHR## a _) `compare`
          x@(VkSamplerYcbcrConversionInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSamplerYcbcrConversionInfoKHR where
        sizeOf ~_ = #{size VkSamplerYcbcrConversionInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSamplerYcbcrConversionInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSamplerYcbcrConversionInfoKHR where
        unsafeAddr (VkSamplerYcbcrConversionInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSamplerYcbcrConversionInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSamplerYcbcrConversionInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSamplerYcbcrConversionInfoKHR where
        type StructFields VkSamplerYcbcrConversionInfoKHR =
             '["sType", "pNext", "conversion"] -- ' closing tick for hsc2hs
        type CUnionType VkSamplerYcbcrConversionInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSamplerYcbcrConversionInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSamplerYcbcrConversionInfoKHR =
             '[VkSamplerCreateInfo, VkImageViewCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSamplerYcbcrConversionInfoKHR where
        type FieldType "sType" VkSamplerYcbcrConversionInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkSamplerYcbcrConversionInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSamplerYcbcrConversionInfoKHR =
             #{offset VkSamplerYcbcrConversionInfoKHR, sType}
        type FieldIsArray "sType" VkSamplerYcbcrConversionInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSamplerYcbcrConversionInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSamplerYcbcrConversionInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSamplerYcbcrConversionInfoKHR where
        type FieldType "pNext" VkSamplerYcbcrConversionInfoKHR = Ptr Void
        type FieldOptional "pNext" VkSamplerYcbcrConversionInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSamplerYcbcrConversionInfoKHR =
             #{offset VkSamplerYcbcrConversionInfoKHR, pNext}
        type FieldIsArray "pNext" VkSamplerYcbcrConversionInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSamplerYcbcrConversionInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSamplerYcbcrConversionInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "conversion" VkSamplerYcbcrConversionInfoKHR where
        type FieldType "conversion" VkSamplerYcbcrConversionInfoKHR =
             VkSamplerYcbcrConversionKHR
        type FieldOptional "conversion" VkSamplerYcbcrConversionInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "conversion" VkSamplerYcbcrConversionInfoKHR =
             #{offset VkSamplerYcbcrConversionInfoKHR, conversion}
        type FieldIsArray "conversion" VkSamplerYcbcrConversionInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSamplerYcbcrConversionInfoKHR, conversion}

instance {-# OVERLAPPING #-}
         CanReadField "conversion" VkSamplerYcbcrConversionInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfoKHR, conversion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, conversion}

instance {-# OVERLAPPING #-}
         CanWriteField "conversion" VkSamplerYcbcrConversionInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, conversion}

instance Show VkSamplerYcbcrConversionInfoKHR where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "conversion = " .
                            showsPrec d (getField @"conversion" x) . showChar '}'
