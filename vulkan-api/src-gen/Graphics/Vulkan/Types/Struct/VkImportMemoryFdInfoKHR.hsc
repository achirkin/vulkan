#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportMemoryFdInfoKHR
       (VkImportMemoryFdInfoKHR(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Base                                                   (Addr##,
                                                                             ByteArray##,
                                                                             byteArrayContents##,
                                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags (VkExternalMemoryHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo          (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

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
