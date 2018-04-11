#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSemaphoreGetWin32HandleInfoKHR
       (VkSemaphoreGetWin32HandleInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Base                                                      (Addr##,
                                                                                ByteArray##,
                                                                                byteArrayContents##,
                                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlags (VkExternalSemaphoreHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                 (VkSemaphore)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkSemaphoreGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore                      semaphore;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   > } VkSemaphoreGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSemaphoreGetWin32HandleInfoKHR VkSemaphoreGetWin32HandleInfoKHR registry at www.khronos.org>
data VkSemaphoreGetWin32HandleInfoKHR = VkSemaphoreGetWin32HandleInfoKHR## Addr##
                                                                          ByteArray##

instance Eq VkSemaphoreGetWin32HandleInfoKHR where
        (VkSemaphoreGetWin32HandleInfoKHR## a _) ==
          x@(VkSemaphoreGetWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSemaphoreGetWin32HandleInfoKHR where
        (VkSemaphoreGetWin32HandleInfoKHR## a _) `compare`
          x@(VkSemaphoreGetWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSemaphoreGetWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkSemaphoreGetWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSemaphoreGetWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSemaphoreGetWin32HandleInfoKHR where
        unsafeAddr (VkSemaphoreGetWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSemaphoreGetWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSemaphoreGetWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSemaphoreGetWin32HandleInfoKHR where
        type StructFields VkSemaphoreGetWin32HandleInfoKHR =
             '["sType", "pNext", "semaphore", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkSemaphoreGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSemaphoreGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSemaphoreGetWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkSemaphoreGetWin32HandleInfoKHR where
        type FieldType "sType" VkSemaphoreGetWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSemaphoreGetWin32HandleInfoKHR =
             #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkSemaphoreGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSemaphoreGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSemaphoreGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSemaphoreGetWin32HandleInfoKHR where
        type FieldType "pNext" VkSemaphoreGetWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSemaphoreGetWin32HandleInfoKHR =
             #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkSemaphoreGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSemaphoreGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSemaphoreGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "semaphore" VkSemaphoreGetWin32HandleInfoKHR where
        type FieldType "semaphore" VkSemaphoreGetWin32HandleInfoKHR =
             VkSemaphore
        type FieldOptional "semaphore" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkSemaphoreGetWin32HandleInfoKHR =
             #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}
        type FieldIsArray "semaphore" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanReadField "semaphore" VkSemaphoreGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         CanWriteField "semaphore" VkSemaphoreGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkSemaphoreGetWin32HandleInfoKHR where
        type FieldType "handleType" VkSemaphoreGetWin32HandleInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBits
        type FieldOptional "handleType" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkSemaphoreGetWin32HandleInfoKHR =
             #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkSemaphoreGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkSemaphoreGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkSemaphoreGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreGetWin32HandleInfoKHR, handleType}

instance Show VkSemaphoreGetWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkSemaphoreGetWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "semaphore = " .
                            showsPrec d (getField @"semaphore" x) .
                              showString ", " .
                                showString "handleType = " .
                                  showsPrec d (getField @"handleType" x) . showChar '}'
