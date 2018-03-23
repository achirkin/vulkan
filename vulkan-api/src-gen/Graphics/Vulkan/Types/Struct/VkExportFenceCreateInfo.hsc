#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportFenceCreateInfo
       (VkExportFenceCreateInfo(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags (VkExternalFenceHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkFenceCreateInfo            (VkFenceCreateInfo)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkExportFenceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalFenceHandleTypeFlags handleTypes;
--   > } VkExportFenceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkExportFenceCreateInfo.html VkExportFenceCreateInfo registry at www.khronos.org>
data VkExportFenceCreateInfo = VkExportFenceCreateInfo## Addr##
                                                        ByteArray##

instance Eq VkExportFenceCreateInfo where
        (VkExportFenceCreateInfo## a _) == x@(VkExportFenceCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportFenceCreateInfo where
        (VkExportFenceCreateInfo## a _) `compare`
          x@(VkExportFenceCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportFenceCreateInfo where
        sizeOf ~_ = #{size VkExportFenceCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExportFenceCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportFenceCreateInfo where
        unsafeAddr (VkExportFenceCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportFenceCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportFenceCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportFenceCreateInfo where
        type StructFields VkExportFenceCreateInfo =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportFenceCreateInfo = '[VkFenceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportFenceCreateInfo where
        type FieldType "sType" VkExportFenceCreateInfo = VkStructureType
        type FieldOptional "sType" VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportFenceCreateInfo =
             #{offset VkExportFenceCreateInfo, sType}
        type FieldIsArray "sType" VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExportFenceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportFenceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportFenceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportFenceCreateInfo where
        type FieldType "pNext" VkExportFenceCreateInfo = Ptr Void
        type FieldOptional "pNext" VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportFenceCreateInfo =
             #{offset VkExportFenceCreateInfo, pNext}
        type FieldIsArray "pNext" VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExportFenceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportFenceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportFenceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportFenceCreateInfo where
        type FieldType "handleTypes" VkExportFenceCreateInfo =
             VkExternalFenceHandleTypeFlags
        type FieldOptional "handleTypes" VkExportFenceCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportFenceCreateInfo =
             #{offset VkExportFenceCreateInfo, handleTypes}
        type FieldIsArray "handleTypes" VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExportFenceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfo, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExportFenceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceCreateInfo, handleTypes}

instance Show VkExportFenceCreateInfo where
        showsPrec d x
          = showString "VkExportFenceCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
