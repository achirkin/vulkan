#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportFenceWin32HandleInfoKHR
       (VkExportFenceWin32HandleInfoKHR(..)) where
import           Foreign.Storable                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType     (VkStructureType)
import           Graphics.Vulkan.Types.Include                  (DWORD, LPCWSTR, SECURITY_ATTRIBUTES)
import           Graphics.Vulkan.Types.Struct.VkFenceCreateInfo (VkFenceCreateInfo)
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

-- | > typedef struct VkExportFenceWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                                pNext;
--   >     const SECURITY_ATTRIBUTES* pAttributes;
--   >     DWORD                                      dwAccess;
--   >     LPCWSTR                                    name;
--   > } VkExportFenceWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExportFenceWin32HandleInfoKHR.html VkExportFenceWin32HandleInfoKHR registry at www.khronos.org>
data VkExportFenceWin32HandleInfoKHR = VkExportFenceWin32HandleInfoKHR## Addr##
                                                                        ByteArray##

instance Eq VkExportFenceWin32HandleInfoKHR where
        (VkExportFenceWin32HandleInfoKHR## a _) ==
          x@(VkExportFenceWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportFenceWin32HandleInfoKHR where
        (VkExportFenceWin32HandleInfoKHR## a _) `compare`
          x@(VkExportFenceWin32HandleInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportFenceWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkExportFenceWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportFenceWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportFenceWin32HandleInfoKHR where
        unsafeAddr (VkExportFenceWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportFenceWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportFenceWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportFenceWin32HandleInfoKHR where
        type StructFields VkExportFenceWin32HandleInfoKHR =
             '["sType", "pNext", "pAttributes", "dwAccess", "name"] -- ' closing tick for hsc2hs
        type CUnionType VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportFenceWin32HandleInfoKHR =
             '[VkFenceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportFenceWin32HandleInfoKHR where
        type FieldType "sType" VkExportFenceWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportFenceWin32HandleInfoKHR =
             #{offset VkExportFenceWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportFenceWin32HandleInfoKHR where
        type FieldType "pNext" VkExportFenceWin32HandleInfoKHR = Ptr Void
        type FieldOptional "pNext" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportFenceWin32HandleInfoKHR =
             #{offset VkExportFenceWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pAttributes" VkExportFenceWin32HandleInfoKHR where
        type FieldType "pAttributes" VkExportFenceWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES
        type FieldOptional "pAttributes" VkExportFenceWin32HandleInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pAttributes" VkExportFenceWin32HandleInfoKHR =
             #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}
        type FieldIsArray "pAttributes" VkExportFenceWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanReadField "pAttributes" VkExportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, pAttributes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttributes" VkExportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         HasField "dwAccess" VkExportFenceWin32HandleInfoKHR where
        type FieldType "dwAccess" VkExportFenceWin32HandleInfoKHR = DWORD
        type FieldOptional "dwAccess" VkExportFenceWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dwAccess" VkExportFenceWin32HandleInfoKHR =
             #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}
        type FieldIsArray "dwAccess" VkExportFenceWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanReadField "dwAccess" VkExportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, dwAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "dwAccess" VkExportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         HasField "name" VkExportFenceWin32HandleInfoKHR where
        type FieldType "name" VkExportFenceWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "name" VkExportFenceWin32HandleInfoKHR =
             #{offset VkExportFenceWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkExportFenceWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkExportFenceWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkExportFenceWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceWin32HandleInfoKHR, name}

instance Show VkExportFenceWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkExportFenceWin32HandleInfoKHR {" .
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
