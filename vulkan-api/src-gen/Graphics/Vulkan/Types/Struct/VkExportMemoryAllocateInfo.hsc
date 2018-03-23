#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportMemoryAllocateInfo
       (VkExportMemoryAllocateInfo(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags (VkExternalMemoryHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo          (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkExportMemoryAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlags handleTypes;
--   > } VkExportMemoryAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkExportMemoryAllocateInfo.html VkExportMemoryAllocateInfo registry at www.khronos.org>
data VkExportMemoryAllocateInfo = VkExportMemoryAllocateInfo## Addr##
                                                              ByteArray##

instance Eq VkExportMemoryAllocateInfo where
        (VkExportMemoryAllocateInfo## a _) ==
          x@(VkExportMemoryAllocateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryAllocateInfo where
        (VkExportMemoryAllocateInfo## a _) `compare`
          x@(VkExportMemoryAllocateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryAllocateInfo where
        sizeOf ~_ = #{size VkExportMemoryAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExportMemoryAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryAllocateInfo where
        unsafeAddr (VkExportMemoryAllocateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryAllocateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryAllocateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryAllocateInfo where
        type StructFields VkExportMemoryAllocateInfo =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryAllocateInfo =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryAllocateInfo where
        type FieldType "sType" VkExportMemoryAllocateInfo = VkStructureType
        type FieldOptional "sType" VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryAllocateInfo =
             #{offset VkExportMemoryAllocateInfo, sType}
        type FieldIsArray "sType" VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryAllocateInfo where
        type FieldType "pNext" VkExportMemoryAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryAllocateInfo =
             #{offset VkExportMemoryAllocateInfo, pNext}
        type FieldIsArray "pNext" VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportMemoryAllocateInfo where
        type FieldType "handleTypes" VkExportMemoryAllocateInfo =
             VkExternalMemoryHandleTypeFlags
        type FieldOptional "handleTypes" VkExportMemoryAllocateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportMemoryAllocateInfo =
             #{offset VkExportMemoryAllocateInfo, handleTypes}
        type FieldIsArray "handleTypes" VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExportMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfo, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExportMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfo, handleTypes}

instance Show VkExportMemoryAllocateInfo where
        showsPrec d x
          = showString "VkExportMemoryAllocateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'
