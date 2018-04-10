#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportSemaphoreWin32HandleInfoKHR
       (VkImportSemaphoreWin32HandleInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Base                                                      (Addr##,
                                                                                ByteArray##,
                                                                                byteArrayContents##,
                                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlags (VkExternalSemaphoreHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkSemaphoreImportFlags             (VkSemaphoreImportFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                 (VkSemaphore)
import           Graphics.Vulkan.Types.Include                                 (HANDLE,
                                                                                LPCWSTR)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkImportSemaphoreWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore    semaphore;
--   >     VkSemaphoreImportFlags flags;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   >     HANDLE           handle;
--   >     LPCWSTR          name;
--   > } VkImportSemaphoreWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkImportSemaphoreWin32HandleInfoKHR VkImportSemaphoreWin32HandleInfoKHR registry at www.khronos.org>
data VkImportSemaphoreWin32HandleInfoKHR = VkImportSemaphoreWin32HandleInfoKHR## Addr##
                                                                                ByteArray##

instance Eq VkImportSemaphoreWin32HandleInfoKHR where
        (VkImportSemaphoreWin32HandleInfoKHR## a _) ==
          x@(VkImportSemaphoreWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportSemaphoreWin32HandleInfoKHR where
        (VkImportSemaphoreWin32HandleInfoKHR## a _) `compare`
          x@(VkImportSemaphoreWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportSemaphoreWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkImportSemaphoreWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportSemaphoreWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportSemaphoreWin32HandleInfoKHR
         where
        unsafeAddr (VkImportSemaphoreWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportSemaphoreWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportSemaphoreWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportSemaphoreWin32HandleInfoKHR where
        type StructFields VkImportSemaphoreWin32HandleInfoKHR =
             '["sType", "pNext", "semaphore", "flags", "handleType", "handle", -- ' closing tick for hsc2hs
               "name"]
        type CUnionType VkImportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportSemaphoreWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "sType" VkImportSemaphoreWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "semaphore" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "semaphore" VkImportSemaphoreWin32HandleInfoKHR =
             VkSemaphore
        type FieldOptional "semaphore" VkImportSemaphoreWin32HandleInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}
        type FieldIsArray "semaphore" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanReadField "semaphore" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanWriteField "semaphore" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         HasField "flags" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "flags" VkImportSemaphoreWin32HandleInfoKHR =
             VkSemaphoreImportFlags
        type FieldOptional "flags" VkImportSemaphoreWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}
        type FieldIsArray "flags" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "handleType" VkImportSemaphoreWin32HandleInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBits
        type FieldOptional "handleType" VkImportSemaphoreWin32HandleInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportSemaphoreWin32HandleInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handle" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "handle" VkImportSemaphoreWin32HandleInfoKHR =
             HANDLE
        type FieldOptional "handle" VkImportSemaphoreWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}
        type FieldIsArray "handle" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanReadField "handle" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, handle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanWriteField "handle" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         HasField "name" VkImportSemaphoreWin32HandleInfoKHR where
        type FieldType "name" VkImportSemaphoreWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkImportSemaphoreWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "name" VkImportSemaphoreWin32HandleInfoKHR =
             #{offset VkImportSemaphoreWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkImportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkImportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkImportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreWin32HandleInfoKHR, name}

instance Show VkImportSemaphoreWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkImportSemaphoreWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "semaphore = " .
                            showsPrec d (getField @"semaphore" x) .
                              showString ", " .
                                showString "flags = " .
                                  showsPrec d (getField @"flags" x) .
                                    showString ", " .
                                      showString "handleType = " .
                                        showsPrec d (getField @"handleType" x) .
                                          showString ", " .
                                            showString "handle = " .
                                              showsPrec d (getField @"handle" x) .
                                                showString ", " .
                                                  showString "name = " .
                                                    showsPrec d (getField @"name" x) . showChar '}'
