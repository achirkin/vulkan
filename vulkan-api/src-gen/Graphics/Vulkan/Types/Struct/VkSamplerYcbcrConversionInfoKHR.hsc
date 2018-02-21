#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
         HasVkSType VkSamplerYcbcrConversionInfoKHR where
        type VkSTypeMType VkSamplerYcbcrConversionInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSamplerYcbcrConversionInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, sType}

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

instance CanReadField "sType" VkSamplerYcbcrConversionInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSamplerYcbcrConversionInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSamplerYcbcrConversionInfoKHR where
        type VkPNextMType VkSamplerYcbcrConversionInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSamplerYcbcrConversionInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, pNext}

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

instance CanReadField "pNext" VkSamplerYcbcrConversionInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSamplerYcbcrConversionInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkConversion VkSamplerYcbcrConversionInfoKHR where
        type VkConversionMType VkSamplerYcbcrConversionInfoKHR =
             VkSamplerYcbcrConversionKHR

        {-# NOINLINE vkConversion #-}
        vkConversion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSamplerYcbcrConversionInfoKHR, conversion})

        {-# INLINE vkConversionByteOffset #-}
        vkConversionByteOffset ~_
          = #{offset VkSamplerYcbcrConversionInfoKHR, conversion}

        {-# INLINE readVkConversion #-}
        readVkConversion p
          = peekByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, conversion}

        {-# INLINE writeVkConversion #-}
        writeVkConversion p
          = pokeByteOff p #{offset VkSamplerYcbcrConversionInfoKHR, conversion}

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

instance CanReadField "conversion" VkSamplerYcbcrConversionInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkConversion

        {-# INLINE readField #-}
        readField = readVkConversion

instance CanWriteField "conversion" VkSamplerYcbcrConversionInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkConversion

instance Show VkSamplerYcbcrConversionInfoKHR where
        showsPrec d x
          = showString "VkSamplerYcbcrConversionInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkConversion = " .
                            showsPrec d (vkConversion x) . showChar '}'
