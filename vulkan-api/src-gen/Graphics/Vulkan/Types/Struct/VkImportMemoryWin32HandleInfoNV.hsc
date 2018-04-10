#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportMemoryWin32HandleInfoNV
       (VkImportMemoryWin32HandleInfoNV(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Base                                                     (Addr##,
                                                                               ByteArray##,
                                                                               byteArrayContents##,
                                                                               plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV (VkExternalMemoryHandleTypeFlagsNV)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Include                                (HANDLE)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo            (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkImportMemoryWin32HandleInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleType;
--   >     HANDLE                           handle;
--   > } VkImportMemoryWin32HandleInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkImportMemoryWin32HandleInfoNV VkImportMemoryWin32HandleInfoNV registry at www.khronos.org>
data VkImportMemoryWin32HandleInfoNV = VkImportMemoryWin32HandleInfoNV## Addr##
                                                                        ByteArray##

instance Eq VkImportMemoryWin32HandleInfoNV where
        (VkImportMemoryWin32HandleInfoNV## a _) ==
          x@(VkImportMemoryWin32HandleInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryWin32HandleInfoNV where
        (VkImportMemoryWin32HandleInfoNV## a _) `compare`
          x@(VkImportMemoryWin32HandleInfoNV## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryWin32HandleInfoNV where
        sizeOf ~_ = #{size VkImportMemoryWin32HandleInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryWin32HandleInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportMemoryWin32HandleInfoNV where
        unsafeAddr (VkImportMemoryWin32HandleInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportMemoryWin32HandleInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportMemoryWin32HandleInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportMemoryWin32HandleInfoNV where
        type StructFields VkImportMemoryWin32HandleInfoNV =
             '["sType", "pNext", "handleType", "handle"] -- ' closing tick for hsc2hs
        type CUnionType VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportMemoryWin32HandleInfoNV =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryWin32HandleInfoNV where
        type FieldType "sType" VkImportMemoryWin32HandleInfoNV =
             VkStructureType
        type FieldOptional "sType" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, sType}
        type FieldIsArray "sType" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryWin32HandleInfoNV where
        type FieldType "pNext" VkImportMemoryWin32HandleInfoNV = Ptr Void
        type FieldOptional "pNext" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, pNext}
        type FieldIsArray "pNext" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryWin32HandleInfoNV where
        type FieldType "handleType" VkImportMemoryWin32HandleInfoNV =
             VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "handleType" VkImportMemoryWin32HandleInfoNV =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, handleType}
        type FieldIsArray "handleType" VkImportMemoryWin32HandleInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handleType}

instance {-# OVERLAPPING #-}
         HasField "handle" VkImportMemoryWin32HandleInfoNV where
        type FieldType "handle" VkImportMemoryWin32HandleInfoNV = HANDLE
        type FieldOptional "handle" VkImportMemoryWin32HandleInfoNV = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, handle}
        type FieldIsArray "handle" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, handle}

instance {-# OVERLAPPING #-}
         CanReadField "handle" VkImportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, handle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handle}

instance {-# OVERLAPPING #-}
         CanWriteField "handle" VkImportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handle}

instance Show VkImportMemoryWin32HandleInfoNV where
        showsPrec d x
          = showString "VkImportMemoryWin32HandleInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) .
                              showString ", " .
                                showString "handle = " .
                                  showsPrec d (getField @"handle" x) . showChar '}'
