#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryGetWin32HandleInfoKHR
       (VkMemoryGetWin32HandleInfoKHR(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags (VkExternalMemoryHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Handles                              (VkDeviceMemory)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryGetWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceMemory                   memory;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   > } VkMemoryGetWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkMemoryGetWin32HandleInfoKHR.html VkMemoryGetWin32HandleInfoKHR registry at www.khronos.org>
data VkMemoryGetWin32HandleInfoKHR = VkMemoryGetWin32HandleInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkMemoryGetWin32HandleInfoKHR where
        (VkMemoryGetWin32HandleInfoKHR## a _) ==
          x@(VkMemoryGetWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryGetWin32HandleInfoKHR where
        (VkMemoryGetWin32HandleInfoKHR## a _) `compare`
          x@(VkMemoryGetWin32HandleInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryGetWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkMemoryGetWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryGetWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryGetWin32HandleInfoKHR where
        unsafeAddr (VkMemoryGetWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryGetWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryGetWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryGetWin32HandleInfoKHR where
        type StructFields VkMemoryGetWin32HandleInfoKHR =
             '["sType", "pNext", "memory", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryGetWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "sType" VkMemoryGetWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "pNext" VkMemoryGetWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "memory" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "memory" VkMemoryGetWin32HandleInfoKHR =
             VkDeviceMemory
        type FieldOptional "memory" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, memory}
        type FieldIsArray "memory" VkMemoryGetWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkMemoryGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, memory}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkMemoryGetWin32HandleInfoKHR where
        type FieldType "handleType" VkMemoryGetWin32HandleInfoKHR =
             VkExternalMemoryHandleTypeFlagBits
        type FieldOptional "handleType" VkMemoryGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkMemoryGetWin32HandleInfoKHR =
             #{offset VkMemoryGetWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkMemoryGetWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkMemoryGetWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkMemoryGetWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetWin32HandleInfoKHR, handleType}

instance Show VkMemoryGetWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkMemoryGetWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memory = " .
                            showsPrec d (getField @"memory" x) .
                              showString ", " .
                                showString "handleType = " .
                                  showsPrec d (getField @"handleType" x) . showChar '}'
