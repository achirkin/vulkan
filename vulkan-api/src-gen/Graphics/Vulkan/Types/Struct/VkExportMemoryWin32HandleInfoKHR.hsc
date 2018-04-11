#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportMemoryWin32HandleInfoKHR
       (VkExportMemoryWin32HandleInfoKHR(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Base                                          (Addr##,
                                                                    ByteArray##,
                                                                    byteArrayContents##,
                                                                    plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Include                     (DWORD,
                                                                    LPCWSTR,
                                                                    SECURITY_ATTRIBUTES)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkExportMemoryWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES* pAttributes;
--   >     DWORD                            dwAccess;
--   >     LPCWSTR                          name;
--   > } VkExportMemoryWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportMemoryWin32HandleInfoKHR VkExportMemoryWin32HandleInfoKHR registry at www.khronos.org>
data VkExportMemoryWin32HandleInfoKHR = VkExportMemoryWin32HandleInfoKHR## Addr##
                                                                          ByteArray##

instance Eq VkExportMemoryWin32HandleInfoKHR where
        (VkExportMemoryWin32HandleInfoKHR## a _) ==
          x@(VkExportMemoryWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryWin32HandleInfoKHR where
        (VkExportMemoryWin32HandleInfoKHR## a _) `compare`
          x@(VkExportMemoryWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkExportMemoryWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryWin32HandleInfoKHR where
        unsafeAddr (VkExportMemoryWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryWin32HandleInfoKHR where
        type StructFields VkExportMemoryWin32HandleInfoKHR =
             '["sType", "pNext", "pAttributes", "dwAccess", "name"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryWin32HandleInfoKHR =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "sType" VkExportMemoryWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "pNext" VkExportMemoryWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pAttributes" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES
        type FieldOptional "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}
        type FieldIsArray "pAttributes" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanReadField "pAttributes" VkExportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttributes" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         HasField "dwAccess" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "dwAccess" VkExportMemoryWin32HandleInfoKHR = DWORD
        type FieldOptional "dwAccess" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dwAccess" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}
        type FieldIsArray "dwAccess" VkExportMemoryWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanReadField "dwAccess" VkExportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "dwAccess" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         HasField "name" VkExportMemoryWin32HandleInfoKHR where
        type FieldType "name" VkExportMemoryWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "name" VkExportMemoryWin32HandleInfoKHR =
             #{offset VkExportMemoryWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkExportMemoryWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkExportMemoryWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkExportMemoryWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoKHR, name}

instance Show VkExportMemoryWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkExportMemoryWin32HandleInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pAttributes = " .
                            showsPrec d (getField @"pAttributes" x) .
                              showString ", " .
                                showString "dwAccess = " .
                                  showsPrec d (getField @"dwAccess" x) .
                                    showString ", " .
                                      showString "name = " .
                                        showsPrec d (getField @"name" x) . showChar '}'
