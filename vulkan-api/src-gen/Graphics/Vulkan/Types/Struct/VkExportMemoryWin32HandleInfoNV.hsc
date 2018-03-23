#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportMemoryWin32HandleInfoNV
       (VkExportMemoryWin32HandleInfoNV(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Include                     (DWORD, SECURITY_ATTRIBUTES)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkExportMemoryWin32HandleInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES*       pAttributes;
--   >     DWORD                            dwAccess;
--   > } VkExportMemoryWin32HandleInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkExportMemoryWin32HandleInfoNV.html VkExportMemoryWin32HandleInfoNV registry at www.khronos.org>
data VkExportMemoryWin32HandleInfoNV = VkExportMemoryWin32HandleInfoNV## Addr##
                                                                        ByteArray##

instance Eq VkExportMemoryWin32HandleInfoNV where
        (VkExportMemoryWin32HandleInfoNV## a _) ==
          x@(VkExportMemoryWin32HandleInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryWin32HandleInfoNV where
        (VkExportMemoryWin32HandleInfoNV## a _) `compare`
          x@(VkExportMemoryWin32HandleInfoNV## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryWin32HandleInfoNV where
        sizeOf ~_ = #{size VkExportMemoryWin32HandleInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryWin32HandleInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryWin32HandleInfoNV where
        unsafeAddr (VkExportMemoryWin32HandleInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryWin32HandleInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryWin32HandleInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryWin32HandleInfoNV where
        type StructFields VkExportMemoryWin32HandleInfoNV =
             '["sType", "pNext", "pAttributes", "dwAccess"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryWin32HandleInfoNV =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryWin32HandleInfoNV where
        type FieldType "sType" VkExportMemoryWin32HandleInfoNV =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryWin32HandleInfoNV =
             #{offset VkExportMemoryWin32HandleInfoNV, sType}
        type FieldIsArray "sType" VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryWin32HandleInfoNV where
        type FieldType "pNext" VkExportMemoryWin32HandleInfoNV = Ptr Void
        type FieldOptional "pNext" VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryWin32HandleInfoNV =
             #{offset VkExportMemoryWin32HandleInfoNV, pNext}
        type FieldIsArray "pNext" VkExportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pAttributes" VkExportMemoryWin32HandleInfoNV where
        type FieldType "pAttributes" VkExportMemoryWin32HandleInfoNV =
             Ptr SECURITY_ATTRIBUTES
        type FieldOptional "pAttributes" VkExportMemoryWin32HandleInfoNV =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pAttributes" VkExportMemoryWin32HandleInfoNV =
             #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}
        type FieldIsArray "pAttributes" VkExportMemoryWin32HandleInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}

instance {-# OVERLAPPING #-}
         CanReadField "pAttributes" VkExportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, pAttributes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttributes" VkExportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, pAttributes}

instance {-# OVERLAPPING #-}
         HasField "dwAccess" VkExportMemoryWin32HandleInfoNV where
        type FieldType "dwAccess" VkExportMemoryWin32HandleInfoNV = DWORD
        type FieldOptional "dwAccess" VkExportMemoryWin32HandleInfoNV =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "dwAccess" VkExportMemoryWin32HandleInfoNV =
             #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}
        type FieldIsArray "dwAccess" VkExportMemoryWin32HandleInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}

instance {-# OVERLAPPING #-}
         CanReadField "dwAccess" VkExportMemoryWin32HandleInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryWin32HandleInfoNV, dwAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "dwAccess" VkExportMemoryWin32HandleInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryWin32HandleInfoNV, dwAccess}

instance Show VkExportMemoryWin32HandleInfoNV where
        showsPrec d x
          = showString "VkExportMemoryWin32HandleInfoNV {" .
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
                                  showsPrec d (getField @"dwAccess" x) . showChar '}'
