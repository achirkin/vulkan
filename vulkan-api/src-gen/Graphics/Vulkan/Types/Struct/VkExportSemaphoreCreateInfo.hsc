#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportSemaphoreCreateInfo
       (VkExportSemaphoreCreateInfo(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlags (VkExternalSemaphoreHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo            (VkSemaphoreCreateInfo)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkExportSemaphoreCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalSemaphoreHandleTypeFlags handleTypes;
--   > } VkExportSemaphoreCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkExportSemaphoreCreateInfo.html VkExportSemaphoreCreateInfo registry at www.khronos.org>
data VkExportSemaphoreCreateInfo = VkExportSemaphoreCreateInfo## Addr##
                                                                ByteArray##

instance Eq VkExportSemaphoreCreateInfo where
        (VkExportSemaphoreCreateInfo## a _) ==
          x@(VkExportSemaphoreCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportSemaphoreCreateInfo where
        (VkExportSemaphoreCreateInfo## a _) `compare`
          x@(VkExportSemaphoreCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportSemaphoreCreateInfo where
        sizeOf ~_ = #{size VkExportSemaphoreCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExportSemaphoreCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportSemaphoreCreateInfo where
        unsafeAddr (VkExportSemaphoreCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportSemaphoreCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportSemaphoreCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportSemaphoreCreateInfo where
        type StructFields VkExportSemaphoreCreateInfo =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportSemaphoreCreateInfo =
             '[VkSemaphoreCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportSemaphoreCreateInfo where
        type FieldType "sType" VkExportSemaphoreCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportSemaphoreCreateInfo =
             #{offset VkExportSemaphoreCreateInfo, sType}
        type FieldIsArray "sType" VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportSemaphoreCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportSemaphoreCreateInfo where
        type FieldType "pNext" VkExportSemaphoreCreateInfo = Ptr Void
        type FieldOptional "pNext" VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportSemaphoreCreateInfo =
             #{offset VkExportSemaphoreCreateInfo, pNext}
        type FieldIsArray "pNext" VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportSemaphoreCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportSemaphoreCreateInfo where
        type FieldType "handleTypes" VkExportSemaphoreCreateInfo =
             VkExternalSemaphoreHandleTypeFlags
        type FieldOptional "handleTypes" VkExportSemaphoreCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportSemaphoreCreateInfo =
             #{offset VkExportSemaphoreCreateInfo, handleTypes}
        type FieldIsArray "handleTypes" VkExportSemaphoreCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExportSemaphoreCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfo, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExportSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfo, handleTypes}

instance Show VkExportSemaphoreCreateInfo where
        showsPrec d x
          = showString "VkExportSemaphoreCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
