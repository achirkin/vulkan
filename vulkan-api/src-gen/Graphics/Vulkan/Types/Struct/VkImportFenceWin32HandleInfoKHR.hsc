#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportFenceWin32HandleInfoKHR
       (VkImportFenceWin32HandleInfoKHR(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags (VkExternalFenceHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkFenceImportFlags             (VkFenceImportFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Handles                             (VkFence)
import           Graphics.Vulkan.Types.Include                             (HANDLE,
                                                                            LPCWSTR)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkImportFenceWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                        pNext;
--   >     VkFence                          fence;
--   >     VkFenceImportFlags              flags;
--   >     VkExternalFenceHandleTypeFlagBits  handleType;
--   >     HANDLE                             handle;
--   >     LPCWSTR                            name;
--   > } VkImportFenceWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkImportFenceWin32HandleInfoKHR.html VkImportFenceWin32HandleInfoKHR registry at www.khronos.org>
data VkImportFenceWin32HandleInfoKHR = VkImportFenceWin32HandleInfoKHR## Addr##
                                                                        ByteArray##

instance Eq VkImportFenceWin32HandleInfoKHR where
        (VkImportFenceWin32HandleInfoKHR## a _) ==
          x@(VkImportFenceWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportFenceWin32HandleInfoKHR where
        (VkImportFenceWin32HandleInfoKHR## a _) `compare`
          x@(VkImportFenceWin32HandleInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportFenceWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkImportFenceWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportFenceWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportFenceWin32HandleInfoKHR where
        unsafeAddr (VkImportFenceWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportFenceWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportFenceWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportFenceWin32HandleInfoKHR where
        type StructFields VkImportFenceWin32HandleInfoKHR =
             '["sType", "pNext", "fence", "flags", "handleType", "handle", -- ' closing tick for hsc2hs
               "name"]
        type CUnionType VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportFenceWin32HandleInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportFenceWin32HandleInfoKHR where
        type FieldType "sType" VkImportFenceWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportFenceWin32HandleInfoKHR where
        type FieldType "pNext" VkImportFenceWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "fence" VkImportFenceWin32HandleInfoKHR where
        type FieldType "fence" VkImportFenceWin32HandleInfoKHR = VkFence
        type FieldOptional "fence" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fence" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, fence}
        type FieldIsArray "fence" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanReadField "fence" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, fence})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanWriteField "fence" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasField "flags" VkImportFenceWin32HandleInfoKHR where
        type FieldType "flags" VkImportFenceWin32HandleInfoKHR =
             VkFenceImportFlags
        type FieldOptional "flags" VkImportFenceWin32HandleInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, flags}
        type FieldIsArray "flags" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportFenceWin32HandleInfoKHR where
        type FieldType "handleType" VkImportFenceWin32HandleInfoKHR =
             VkExternalFenceHandleTypeFlagBits
        type FieldOptional "handleType" VkImportFenceWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, handleType}
        type FieldIsArray "handleType" VkImportFenceWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handle" VkImportFenceWin32HandleInfoKHR where
        type FieldType "handle" VkImportFenceWin32HandleInfoKHR = HANDLE
        type FieldOptional "handle" VkImportFenceWin32HandleInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, handle}
        type FieldIsArray "handle" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanReadField "handle" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, handle})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         CanWriteField "handle" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, handle}

instance {-# OVERLAPPING #-}
         HasField "name" VkImportFenceWin32HandleInfoKHR where
        type FieldType "name" VkImportFenceWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkImportFenceWin32HandleInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "name" VkImportFenceWin32HandleInfoKHR =
             #{offset VkImportFenceWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkImportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportFenceWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkImportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportFenceWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportFenceWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkImportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportFenceWin32HandleInfoKHR, name}

instance Show VkImportFenceWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkImportFenceWin32HandleInfoKHR {" .
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
                                            showString "handle = " .
                                              showsPrec d (getField @"handle" x) .
                                                showString ", " .
                                                  showString "name = " .
                                                    showsPrec d (getField @"name" x) . showChar '}'
