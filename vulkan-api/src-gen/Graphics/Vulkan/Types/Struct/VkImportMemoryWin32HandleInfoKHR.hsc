#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportMemoryWin32HandleInfoKHR
       (VkImportMemoryWin32HandleInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Include                                 (HANDLE,
                                                                                LPCWSTR)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo             (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkImportMemoryWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   >     HANDLE           handle;
--   >     LPCWSTR          name;
--   > } VkImportMemoryWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImportMemoryWin32HandleInfoKHR.html VkImportMemoryWin32HandleInfoKHR registry at www.khronos.org>
data VkImportMemoryWin32HandleInfoKHR = VkImportMemoryWin32HandleInfoKHR## Addr##
                                                                          ByteArray##

instance Eq VkImportMemoryWin32HandleInfoKHR where
        (VkImportMemoryWin32HandleInfoKHR## a _) ==
          x@(VkImportMemoryWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryWin32HandleInfoKHR where
        (VkImportMemoryWin32HandleInfoKHR## a _) `compare`
          x@(VkImportMemoryWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkImportMemoryWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportMemoryWin32HandleInfoKHR where
        unsafeAddr (VkImportMemoryWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportMemoryWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportMemoryWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportMemoryWin32HandleInfoKHR where
        type StructFields VkImportMemoryWin32HandleInfoKHR =
             '["sType", "pNext", "handleType", "handle", "name"] -- ' closing tick for hsc2hs
        type CUnionType VkImportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportMemoryWin32HandleInfoKHR =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "sType" VkImportMemoryWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkImportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "pNext" VkImportMemoryWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "handleType" VkImportMemoryWin32HandleInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkImportMemoryWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handle" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "handle" VkImportMemoryWin32HandleInfoKHR = HANDLE
        type FieldOptional "handle" VkImportMemoryWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, handle}
        type FieldIsArray "handle" VkImportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanReadField "handle" VkImportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, handle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanWriteField "handle" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         HasField "name" VkImportMemoryWin32HandleInfoKHR where
        type FieldType "name" VkImportMemoryWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkImportMemoryWin32HandleInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "name" VkImportMemoryWin32HandleInfoKHR =
             #{offset VkImportMemoryWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkImportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkImportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkImportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoKHR, name}

instance Show VkImportMemoryWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkImportMemoryWin32HandleInfoKHR {" .
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
                                  showsPrec d (getField @"handle" x) .
                                    showString ", " .
                                      showString "name = " .
                                        showsPrec d (getField @"name" x) . showChar '}'
