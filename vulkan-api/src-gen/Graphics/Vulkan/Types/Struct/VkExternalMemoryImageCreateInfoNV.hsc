#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalMemoryImageCreateInfoNV
       (VkExternalMemoryImageCreateInfoNV(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Base                                                     (Addr##,
                                                                               ByteArray##,
                                                                               byteArrayContents##,
                                                                               plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV (VkExternalMemoryHandleTypeFlagsNV)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo               (VkImageCreateInfo)
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryImageCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleTypes;
--   > } VkExternalMemoryImageCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkExternalMemoryImageCreateInfoNVVkExternalMemoryImageCreateInfoNV registry at www.khronos.org>
data VkExternalMemoryImageCreateInfoNV = VkExternalMemoryImageCreateInfoNV## Addr##
                                                                            ByteArray##

instance Eq VkExternalMemoryImageCreateInfoNV where
        (VkExternalMemoryImageCreateInfoNV## a _) ==
          x@(VkExternalMemoryImageCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryImageCreateInfoNV where
        (VkExternalMemoryImageCreateInfoNV## a _) `compare`
          x@(VkExternalMemoryImageCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryImageCreateInfoNV where
        sizeOf ~_ = #{size VkExternalMemoryImageCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryImageCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryImageCreateInfoNV where
        unsafeAddr (VkExternalMemoryImageCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryImageCreateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryImageCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryImageCreateInfoNV where
        type StructFields VkExternalMemoryImageCreateInfoNV =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryImageCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryImageCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryImageCreateInfoNV =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryImageCreateInfoNV where
        type FieldType "sType" VkExternalMemoryImageCreateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryImageCreateInfoNV =
             #{offset VkExternalMemoryImageCreateInfoNV, sType}
        type FieldIsArray "sType" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalMemoryImageCreateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalMemoryImageCreateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryImageCreateInfoNV where
        type FieldType "pNext" VkExternalMemoryImageCreateInfoNV = Ptr Void
        type FieldOptional "pNext" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryImageCreateInfoNV =
             #{offset VkExternalMemoryImageCreateInfoNV, pNext}
        type FieldIsArray "pNext" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalMemoryImageCreateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalMemoryImageCreateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryImageCreateInfoNV where
        type FieldType "handleTypes" VkExternalMemoryImageCreateInfoNV =
             VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "handleTypes" VkExternalMemoryImageCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryImageCreateInfoNV =
             #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}
        type FieldIsArray "handleTypes" VkExternalMemoryImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExternalMemoryImageCreateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfoNV, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExternalMemoryImageCreateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfoNV, handleTypes}

instance Show VkExternalMemoryImageCreateInfoNV where
        showsPrec d x
          = showString "VkExternalMemoryImageCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
