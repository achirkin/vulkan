#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalMemoryBufferCreateInfo
       (VkExternalMemoryBufferCreateInfo(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Base                                                   (Addr##,
                                                                             ByteArray##,
                                                                             byteArrayContents##,
                                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags (VkExternalMemoryHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBufferCreateInfo            (VkBufferCreateInfo)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryBufferCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlags handleTypes;
--   > } VkExternalMemoryBufferCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExternalMemoryBufferCreateInfo VkExternalMemoryBufferCreateInfo registry at www.khronos.org>
data VkExternalMemoryBufferCreateInfo = VkExternalMemoryBufferCreateInfo## Addr##
                                                                          ByteArray##

instance Eq VkExternalMemoryBufferCreateInfo where
        (VkExternalMemoryBufferCreateInfo## a _) ==
          x@(VkExternalMemoryBufferCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryBufferCreateInfo where
        (VkExternalMemoryBufferCreateInfo## a _) `compare`
          x@(VkExternalMemoryBufferCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryBufferCreateInfo where
        sizeOf ~_ = #{size VkExternalMemoryBufferCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryBufferCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryBufferCreateInfo where
        unsafeAddr (VkExternalMemoryBufferCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryBufferCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryBufferCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryBufferCreateInfo where
        type StructFields VkExternalMemoryBufferCreateInfo =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryBufferCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryBufferCreateInfo =
             '[VkBufferCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryBufferCreateInfo where
        type FieldType "sType" VkExternalMemoryBufferCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryBufferCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryBufferCreateInfo =
             #{offset VkExternalMemoryBufferCreateInfo, sType}
        type FieldIsArray "sType" VkExternalMemoryBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalMemoryBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalMemoryBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryBufferCreateInfo where
        type FieldType "pNext" VkExternalMemoryBufferCreateInfo = Ptr Void
        type FieldOptional "pNext" VkExternalMemoryBufferCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryBufferCreateInfo =
             #{offset VkExternalMemoryBufferCreateInfo, pNext}
        type FieldIsArray "pNext" VkExternalMemoryBufferCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalMemoryBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalMemoryBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryBufferCreateInfo where
        type FieldType "handleTypes" VkExternalMemoryBufferCreateInfo =
             VkExternalMemoryHandleTypeFlags
        type FieldOptional "handleTypes" VkExternalMemoryBufferCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryBufferCreateInfo =
             #{offset VkExternalMemoryBufferCreateInfo, handleTypes}
        type FieldIsArray "handleTypes" VkExternalMemoryBufferCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryBufferCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExternalMemoryBufferCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryBufferCreateInfo, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryBufferCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExternalMemoryBufferCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryBufferCreateInfo, handleTypes}

instance Show VkExternalMemoryBufferCreateInfo where
        showsPrec d x
          = showString "VkExternalMemoryBufferCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
