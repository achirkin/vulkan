#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportSemaphoreFdInfoKHR
       (VkImportSemaphoreFdInfoKHR(..)) where
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
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkImportSemaphoreFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore    semaphore;
--   >     VkSemaphoreImportFlags flags;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   >     int                              fd;
--   > } VkImportSemaphoreFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkImportSemaphoreFdInfoKHR VkImportSemaphoreFdInfoKHR registry at www.khronos.org>
data VkImportSemaphoreFdInfoKHR = VkImportSemaphoreFdInfoKHR## Addr##
                                                              ByteArray##

instance Eq VkImportSemaphoreFdInfoKHR where
        (VkImportSemaphoreFdInfoKHR## a _) ==
          x@(VkImportSemaphoreFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportSemaphoreFdInfoKHR where
        (VkImportSemaphoreFdInfoKHR## a _) `compare`
          x@(VkImportSemaphoreFdInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportSemaphoreFdInfoKHR where
        sizeOf ~_ = #{size VkImportSemaphoreFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImportSemaphoreFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportSemaphoreFdInfoKHR where
        unsafeAddr (VkImportSemaphoreFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportSemaphoreFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportSemaphoreFdInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportSemaphoreFdInfoKHR where
        type StructFields VkImportSemaphoreFdInfoKHR =
             '["sType", "pNext", "semaphore", "flags", "handleType", "fd"] -- ' closing tick for hsc2hs
        type CUnionType VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportSemaphoreFdInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportSemaphoreFdInfoKHR where
        type FieldType "sType" VkImportSemaphoreFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, sType}
        type FieldIsArray "sType" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportSemaphoreFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportSemaphoreFdInfoKHR where
        type FieldType "pNext" VkImportSemaphoreFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportSemaphoreFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "semaphore" VkImportSemaphoreFdInfoKHR where
        type FieldType "semaphore" VkImportSemaphoreFdInfoKHR = VkSemaphore
        type FieldOptional "semaphore" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, semaphore}
        type FieldIsArray "semaphore" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreFdInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanReadField "semaphore" VkImportSemaphoreFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, semaphore})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanWriteField "semaphore" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         HasField "flags" VkImportSemaphoreFdInfoKHR where
        type FieldType "flags" VkImportSemaphoreFdInfoKHR =
             VkSemaphoreImportFlags
        type FieldOptional "flags" VkImportSemaphoreFdInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, flags}
        type FieldIsArray "flags" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreFdInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkImportSemaphoreFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportSemaphoreFdInfoKHR where
        type FieldType "handleType" VkImportSemaphoreFdInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBits
        type FieldOptional "handleType" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportSemaphoreFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportSemaphoreFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "fd" VkImportSemaphoreFdInfoKHR where
        type FieldType "fd" VkImportSemaphoreFdInfoKHR = CInt
        type FieldOptional "fd" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fd" VkImportSemaphoreFdInfoKHR =
             #{offset VkImportSemaphoreFdInfoKHR, fd}
        type FieldIsArray "fd" VkImportSemaphoreFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportSemaphoreFdInfoKHR, fd}

instance {-# OVERLAPPING #-}
         CanReadField "fd" VkImportSemaphoreFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportSemaphoreFdInfoKHR, fd})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportSemaphoreFdInfoKHR, fd}

instance {-# OVERLAPPING #-}
         CanWriteField "fd" VkImportSemaphoreFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportSemaphoreFdInfoKHR, fd}

instance Show VkImportSemaphoreFdInfoKHR where
        showsPrec d x
          = showString "VkImportSemaphoreFdInfoKHR {" .
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
                                            showString "fd = " .
                                              showsPrec d (getField @"fd" x) . showChar '}'
