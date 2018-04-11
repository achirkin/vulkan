#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportMemoryAllocateInfoNV
       (VkExportMemoryAllocateInfoNV(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Base                                                     (Addr##,
                                                                               ByteArray##,
                                                                               byteArrayContents##,
                                                                               plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV (VkExternalMemoryHandleTypeFlagsNV)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo            (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkExportMemoryAllocateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleTypes;
--   > } VkExportMemoryAllocateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportMemoryAllocateInfoNV VkExportMemoryAllocateInfoNV registry at www.khronos.org>
data VkExportMemoryAllocateInfoNV = VkExportMemoryAllocateInfoNV## Addr##
                                                                  ByteArray##

instance Eq VkExportMemoryAllocateInfoNV where
        (VkExportMemoryAllocateInfoNV## a _) ==
          x@(VkExportMemoryAllocateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryAllocateInfoNV where
        (VkExportMemoryAllocateInfoNV## a _) `compare`
          x@(VkExportMemoryAllocateInfoNV## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryAllocateInfoNV where
        sizeOf ~_ = #{size VkExportMemoryAllocateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryAllocateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryAllocateInfoNV where
        unsafeAddr (VkExportMemoryAllocateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryAllocateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryAllocateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryAllocateInfoNV where
        type StructFields VkExportMemoryAllocateInfoNV =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryAllocateInfoNV =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryAllocateInfoNV where
        type FieldType "sType" VkExportMemoryAllocateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryAllocateInfoNV =
             #{offset VkExportMemoryAllocateInfoNV, sType}
        type FieldIsArray "sType" VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportMemoryAllocateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportMemoryAllocateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryAllocateInfoNV where
        type FieldType "pNext" VkExportMemoryAllocateInfoNV = Ptr Void
        type FieldOptional "pNext" VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryAllocateInfoNV =
             #{offset VkExportMemoryAllocateInfoNV, pNext}
        type FieldIsArray "pNext" VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportMemoryAllocateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportMemoryAllocateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportMemoryAllocateInfoNV where
        type FieldType "handleTypes" VkExportMemoryAllocateInfoNV =
             VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "handleTypes" VkExportMemoryAllocateInfoNV =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportMemoryAllocateInfoNV =
             #{offset VkExportMemoryAllocateInfoNV, handleTypes}
        type FieldIsArray "handleTypes" VkExportMemoryAllocateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoNV, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExportMemoryAllocateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExportMemoryAllocateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, handleTypes}

instance Show VkExportMemoryAllocateInfoNV where
        showsPrec d x
          = showString "VkExportMemoryAllocateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
