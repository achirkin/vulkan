#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Import
       (VkImportFenceFdInfoKHR(..), VkImportMemoryFdInfoKHR(..),
        VkImportMemoryHostPointerInfoEXT(..),
        VkImportSemaphoreFdInfoKHR(..))
       where
import           Foreign.Storable                               (Storable (..))
import           GHC.Base                                       (Addr##,
                                                                 ByteArray##,
                                                                 byteArrayContents##,
                                                                 plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.External            (VkExternalFenceHandleTypeFlagBits,
                                                                 VkExternalMemoryHandleTypeFlagBits,
                                                                 VkExternalSemaphoreHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.Fence               (VkFenceImportFlags)
import           Graphics.Vulkan.Types.Enum.SemaphoreImportFlag (VkSemaphoreImportFlags)
import           Graphics.Vulkan.Types.Enum.StructureType       (VkStructureType)
import           Graphics.Vulkan.Types.Handles                  (VkFence,
                                                                 VkSemaphore)
import           Graphics.Vulkan.Types.Struct.Memory            (VkMemoryAllocateInfo)
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

-- | > typedef struct VkImportFenceFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence              fence;
--   >     VkFenceImportFlags  flags;
--   >     VkExternalFenceHandleTypeFlagBits   handleType;
--   >     int                                    fd;
--   > } VkImportFenceFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportFenceFdInfoKHR VkImportFenceFdInfoKHR registry at www.khronos.org>
data VkImportFenceFdInfoKHR = VkImportFenceFdInfoKHR## Addr##
                                                      ByteArray##

instance Eq VkImportFenceFdInfoKHR where
        (VkImportFenceFdInfoKHR## a _) == x@(VkImportFenceFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportFenceFdInfoKHR where
        (VkImportFenceFdInfoKHR## a _) `compare`
          x@(VkImportFenceFdInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportFenceFdInfoKHR where
        sizeOf ~_ = #{size VkImportFenceFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImportFenceFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportFenceFdInfoKHR where
        unsafeAddr (VkImportFenceFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportFenceFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportFenceFdInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportFenceFdInfoKHR where
        type StructFields VkImportFenceFdInfoKHR =
             '["sType", "pNext", "fence", "flags", "handleType", "fd"] -- ' closing tick for hsc2hs
        type CUnionType VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportFenceFdInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportFenceFdInfoKHR where
        type FieldType "sType" VkImportFenceFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, sType}
        type FieldIsArray "sType" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportFenceFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportFenceFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportFenceFdInfoKHR where
        type FieldType "pNext" VkImportFenceFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportFenceFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportFenceFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "fence" VkImportFenceFdInfoKHR where
        type FieldType "fence" VkImportFenceFdInfoKHR = VkFence
        type FieldOptional "fence" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fence" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, fence}
        type FieldIsArray "fence" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportFenceFdInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanReadField "fence" VkImportFenceFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, fence})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanWriteField "fence" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasField "flags" VkImportFenceFdInfoKHR where
        type FieldType "flags" VkImportFenceFdInfoKHR = VkFenceImportFlags
        type FieldOptional "flags" VkImportFenceFdInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, flags}
        type FieldIsArray "flags" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportFenceFdInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkImportFenceFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportFenceFdInfoKHR where
        type FieldType "handleType" VkImportFenceFdInfoKHR =
             VkExternalFenceHandleTypeFlagBits
        type FieldOptional "handleType" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportFenceFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, handleType}

instance {-# OVERLAPPING #-} HasField "fd" VkImportFenceFdInfoKHR
         where
        type FieldType "fd" VkImportFenceFdInfoKHR = CInt
        type FieldOptional "fd" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fd" VkImportFenceFdInfoKHR =
             #{offset VkImportFenceFdInfoKHR, fd}
        type FieldIsArray "fd" VkImportFenceFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportFenceFdInfoKHR, fd}

instance {-# OVERLAPPING #-}
         CanReadField "fd" VkImportFenceFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceFdInfoKHR, fd})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceFdInfoKHR, fd}

instance {-# OVERLAPPING #-}
         CanWriteField "fd" VkImportFenceFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceFdInfoKHR, fd}

instance Show VkImportFenceFdInfoKHR where
        showsPrec d x
          = showString "VkImportFenceFdInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "fence = " .
                            showsPrec d (getField @"fence" x) .
                              showString ", " .
                                showString "flags = " .
                                  showsPrec d (getField @"flags" x) .
                                    showString ", " .
                                      showString "handleType = " .
                                        showsPrec d (getField @"handleType" x) .
                                          showString ", " .
                                            showString "fd = " .
                                              showsPrec d (getField @"fd" x) . showChar '}'

-- | > typedef struct VkImportMemoryFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   >     int                              fd;
--   > } VkImportMemoryFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportMemoryFdInfoKHR VkImportMemoryFdInfoKHR registry at www.khronos.org>
data VkImportMemoryFdInfoKHR = VkImportMemoryFdInfoKHR## Addr##
                                                        ByteArray##

instance Eq VkImportMemoryFdInfoKHR where
        (VkImportMemoryFdInfoKHR## a _) == x@(VkImportMemoryFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryFdInfoKHR where
        (VkImportMemoryFdInfoKHR## a _) `compare`
          x@(VkImportMemoryFdInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryFdInfoKHR where
        sizeOf ~_ = #{size VkImportMemoryFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImportMemoryFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportMemoryFdInfoKHR where
        unsafeAddr (VkImportMemoryFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportMemoryFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportMemoryFdInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportMemoryFdInfoKHR where
        type StructFields VkImportMemoryFdInfoKHR =
             '["sType", "pNext", "handleType", "fd"] -- ' closing tick for hsc2hs
        type CUnionType VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportMemoryFdInfoKHR =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryFdInfoKHR where
        type FieldType "sType" VkImportMemoryFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryFdInfoKHR =
             #{offset VkImportMemoryFdInfoKHR, sType}
        type FieldIsArray "sType" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportMemoryFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportMemoryFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryFdInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportMemoryFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryFdInfoKHR where
        type FieldType "pNext" VkImportMemoryFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryFdInfoKHR =
             #{offset VkImportMemoryFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportMemoryFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportMemoryFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryFdInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportMemoryFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryFdInfoKHR where
        type FieldType "handleType" VkImportMemoryFdInfoKHR =
             VkExternalMemoryHandleTypeFlagBits
        type FieldOptional "handleType" VkImportMemoryFdInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryFdInfoKHR =
             #{offset VkImportMemoryFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportMemoryFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryFdInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportMemoryFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryFdInfoKHR, handleType}

instance {-# OVERLAPPING #-} HasField "fd" VkImportMemoryFdInfoKHR
         where
        type FieldType "fd" VkImportMemoryFdInfoKHR = CInt
        type FieldOptional "fd" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fd" VkImportMemoryFdInfoKHR =
             #{offset VkImportMemoryFdInfoKHR, fd}
        type FieldIsArray "fd" VkImportMemoryFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkImportMemoryFdInfoKHR, fd}

instance {-# OVERLAPPING #-}
         CanReadField "fd" VkImportMemoryFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryFdInfoKHR, fd})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryFdInfoKHR, fd}

instance {-# OVERLAPPING #-}
         CanWriteField "fd" VkImportMemoryFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryFdInfoKHR, fd}

instance Show VkImportMemoryFdInfoKHR where
        showsPrec d x
          = showString "VkImportMemoryFdInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) .
                              showString ", " .
                                showString "fd = " . showsPrec d (getField @"fd" x) . showChar '}'

-- | > typedef struct VkImportMemoryHostPointerInfoEXT {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   >     void* pHostPointer;
--   > } VkImportMemoryHostPointerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportMemoryHostPointerInfoEXT VkImportMemoryHostPointerInfoEXT registry at www.khronos.org>
data VkImportMemoryHostPointerInfoEXT = VkImportMemoryHostPointerInfoEXT## Addr##
                                                                          ByteArray##

instance Eq VkImportMemoryHostPointerInfoEXT where
        (VkImportMemoryHostPointerInfoEXT## a _) ==
          x@(VkImportMemoryHostPointerInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryHostPointerInfoEXT where
        (VkImportMemoryHostPointerInfoEXT## a _) `compare`
          x@(VkImportMemoryHostPointerInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryHostPointerInfoEXT where
        sizeOf ~_ = #{size VkImportMemoryHostPointerInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryHostPointerInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportMemoryHostPointerInfoEXT where
        unsafeAddr (VkImportMemoryHostPointerInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportMemoryHostPointerInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportMemoryHostPointerInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportMemoryHostPointerInfoEXT where
        type StructFields VkImportMemoryHostPointerInfoEXT =
             '["sType", "pNext", "handleType", "pHostPointer"] -- ' closing tick for hsc2hs
        type CUnionType VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportMemoryHostPointerInfoEXT =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryHostPointerInfoEXT where
        type FieldType "sType" VkImportMemoryHostPointerInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, sType}
        type FieldIsArray "sType" VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportMemoryHostPointerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportMemoryHostPointerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryHostPointerInfoEXT where
        type FieldType "pNext" VkImportMemoryHostPointerInfoEXT = Ptr Void
        type FieldOptional "pNext" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, pNext}
        type FieldIsArray "pNext" VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportMemoryHostPointerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportMemoryHostPointerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryHostPointerInfoEXT where
        type FieldType "handleType" VkImportMemoryHostPointerInfoEXT =
             VkExternalMemoryHandleTypeFlagBits
        type FieldOptional "handleType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, handleType}
        type FieldIsArray "handleType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportMemoryHostPointerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportMemoryHostPointerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, handleType}

instance {-# OVERLAPPING #-}
         HasField "pHostPointer" VkImportMemoryHostPointerInfoEXT where
        type FieldType "pHostPointer" VkImportMemoryHostPointerInfoEXT =
             Ptr Void
        type FieldOptional "pHostPointer" VkImportMemoryHostPointerInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pHostPointer" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}
        type FieldIsArray "pHostPointer" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

instance {-# OVERLAPPING #-}
         CanReadField "pHostPointer" VkImportMemoryHostPointerInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

instance {-# OVERLAPPING #-}
         CanWriteField "pHostPointer" VkImportMemoryHostPointerInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

instance Show VkImportMemoryHostPointerInfoEXT where
        showsPrec d x
          = showString "VkImportMemoryHostPointerInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleType = " .
                            showsPrec d (getField @"handleType" x) .
                              showString ", " .
                                showString "pHostPointer = " .
                                  showsPrec d (getField @"pHostPointer" x) . showChar '}'

-- | > typedef struct VkImportSemaphoreFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore    semaphore;
--   >     VkSemaphoreImportFlags flags;
--   >     VkExternalSemaphoreHandleTypeFlagBits handleType;
--   >     int                              fd;
--   > } VkImportSemaphoreFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImportSemaphoreFdInfoKHR VkImportSemaphoreFdInfoKHR registry at www.khronos.org>
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
