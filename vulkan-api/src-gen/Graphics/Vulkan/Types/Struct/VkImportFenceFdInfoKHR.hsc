#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportFenceFdInfoKHR
       (VkImportFenceFdInfoKHR(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags (VkExternalFenceHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkFenceImportFlags             (VkFenceImportFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Handles                             (VkFence)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkImportFenceFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence              fence;
--   >     VkFenceImportFlags  flags;
--   >     VkExternalFenceHandleTypeFlagBits   handleType;
--   >     int                                    fd;
--   > } VkImportFenceFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkImportFenceFdInfoKHR.html VkImportFenceFdInfoKHR registry at www.khronos.org>
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
