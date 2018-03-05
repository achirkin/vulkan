#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryGetFdInfoKHR
       (VkMemoryGetFdInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                 (VkDeviceMemory)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceMemory                   memory;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   > } VkMemoryGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryGetFdInfoKHR.html VkMemoryGetFdInfoKHR registry at www.khronos.org>
data VkMemoryGetFdInfoKHR = VkMemoryGetFdInfoKHR## Addr## ByteArray##

instance Eq VkMemoryGetFdInfoKHR where
        (VkMemoryGetFdInfoKHR## a _) == x@(VkMemoryGetFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryGetFdInfoKHR where
        (VkMemoryGetFdInfoKHR## a _) `compare` x@(VkMemoryGetFdInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryGetFdInfoKHR where
        sizeOf ~_ = #{size VkMemoryGetFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMemoryGetFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryGetFdInfoKHR where
        unsafeAddr (VkMemoryGetFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryGetFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryGetFdInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryGetFdInfoKHR where
        type StructFields VkMemoryGetFdInfoKHR =
             '["sType", "pNext", "memory", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryGetFdInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkMemoryGetFdInfoKHR
         where
        type FieldType "sType" VkMemoryGetFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, sType}
        type FieldIsArray "sType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkMemoryGetFdInfoKHR
         where
        type FieldType "pNext" VkMemoryGetFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasField "memory" VkMemoryGetFdInfoKHR
         where
        type FieldType "memory" VkMemoryGetFdInfoKHR = VkDeviceMemory
        type FieldOptional "memory" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, memory}
        type FieldIsArray "memory" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkMemoryGetFdInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkMemoryGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, memory}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkMemoryGetFdInfoKHR where
        type FieldType "handleType" VkMemoryGetFdInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkMemoryGetFdInfoKHR =
             #{offset VkMemoryGetFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkMemoryGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkMemoryGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetFdInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkMemoryGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetFdInfoKHR, handleType}

instance Show VkMemoryGetFdInfoKHR where
        showsPrec d x
          = showString "VkMemoryGetFdInfoKHR {" .
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
