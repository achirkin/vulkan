#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalMemoryImageCreateInfo
       (VkExternalMemoryImageCreateInfo(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Base                                                   (Addr##,
                                                                             ByteArray##,
                                                                             byteArrayContents##,
                                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags (VkExternalMemoryHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageCreateInfo             (VkImageCreateInfo)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkExternalMemoryImageCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlags handleTypes;
--   > } VkExternalMemoryImageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkExternalMemoryImageCreateInfo VkExternalMemoryImageCreateInfo registry at www.khronos.org>
data VkExternalMemoryImageCreateInfo = VkExternalMemoryImageCreateInfo## Addr##
                                                                        ByteArray##

instance Eq VkExternalMemoryImageCreateInfo where
        (VkExternalMemoryImageCreateInfo## a _) ==
          x@(VkExternalMemoryImageCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalMemoryImageCreateInfo where
        (VkExternalMemoryImageCreateInfo## a _) `compare`
          x@(VkExternalMemoryImageCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalMemoryImageCreateInfo where
        sizeOf ~_ = #{size VkExternalMemoryImageCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExternalMemoryImageCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalMemoryImageCreateInfo where
        unsafeAddr (VkExternalMemoryImageCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalMemoryImageCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalMemoryImageCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalMemoryImageCreateInfo where
        type StructFields VkExternalMemoryImageCreateInfo =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExternalMemoryImageCreateInfo =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalMemoryImageCreateInfo where
        type FieldType "sType" VkExternalMemoryImageCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalMemoryImageCreateInfo =
             #{offset VkExternalMemoryImageCreateInfo, sType}
        type FieldIsArray "sType" VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalMemoryImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalMemoryImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalMemoryImageCreateInfo where
        type FieldType "pNext" VkExternalMemoryImageCreateInfo = Ptr Void
        type FieldOptional "pNext" VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalMemoryImageCreateInfo =
             #{offset VkExternalMemoryImageCreateInfo, pNext}
        type FieldIsArray "pNext" VkExternalMemoryImageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalMemoryImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalMemoryImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExternalMemoryImageCreateInfo where
        type FieldType "handleTypes" VkExternalMemoryImageCreateInfo =
             VkExternalMemoryHandleTypeFlags
        type FieldOptional "handleTypes" VkExternalMemoryImageCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExternalMemoryImageCreateInfo =
             #{offset VkExternalMemoryImageCreateInfo, handleTypes}
        type FieldIsArray "handleTypes" VkExternalMemoryImageCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalMemoryImageCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExternalMemoryImageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalMemoryImageCreateInfo, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalMemoryImageCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExternalMemoryImageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalMemoryImageCreateInfo, handleTypes}

instance Show VkExternalMemoryImageCreateInfo where
        showsPrec d x
          = showString "VkExternalMemoryImageCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
