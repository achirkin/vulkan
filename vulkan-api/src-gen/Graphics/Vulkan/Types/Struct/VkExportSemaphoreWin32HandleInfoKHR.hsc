#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportSemaphoreWin32HandleInfoKHR
       (VkExportSemaphoreWin32HandleInfoKHR(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Base                                           (Addr##,
                                                                     ByteArray##,
                                                                     byteArrayContents##,
                                                                     plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Include                      (DWORD,
                                                                     LPCWSTR,
                                                                     SECURITY_ATTRIBUTES)
import           Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo (VkSemaphoreCreateInfo)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkExportSemaphoreWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES*       pAttributes;
--   >     DWORD                            dwAccess;
--   >     LPCWSTR                          name;
--   > } VkExportSemaphoreWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportSemaphoreWin32HandleInfoKHR VkExportSemaphoreWin32HandleInfoKHR registry at www.khronos.org>
data VkExportSemaphoreWin32HandleInfoKHR = VkExportSemaphoreWin32HandleInfoKHR## Addr##
                                                                                ByteArray##

instance Eq VkExportSemaphoreWin32HandleInfoKHR where
        (VkExportSemaphoreWin32HandleInfoKHR## a _) ==
          x@(VkExportSemaphoreWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportSemaphoreWin32HandleInfoKHR where
        (VkExportSemaphoreWin32HandleInfoKHR## a _) `compare`
          x@(VkExportSemaphoreWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportSemaphoreWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkExportSemaphoreWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportSemaphoreWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportSemaphoreWin32HandleInfoKHR
         where
        unsafeAddr (VkExportSemaphoreWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportSemaphoreWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportSemaphoreWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportSemaphoreWin32HandleInfoKHR where
        type StructFields VkExportSemaphoreWin32HandleInfoKHR =
             '["sType", "pNext", "pAttributes", "dwAccess", "name"] -- ' closing tick for hsc2hs
        type CUnionType VkExportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportSemaphoreWin32HandleInfoKHR =
             '[VkSemaphoreCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "sType" VkExportSemaphoreWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pAttributes" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "pAttributes" VkExportSemaphoreWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES
        type FieldOptional "pAttributes"
               VkExportSemaphoreWin32HandleInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pAttributes" VkExportSemaphoreWin32HandleInfoKHR
             =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}
        type FieldIsArray "pAttributes" VkExportSemaphoreWin32HandleInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanReadField "pAttributes" VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttributes" VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         HasField "dwAccess" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             DWORD
        type FieldOptional "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}
        type FieldIsArray "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanReadField "dwAccess" VkExportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         CanWriteField "dwAccess" VkExportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         HasField "name" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "name" VkExportSemaphoreWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "name" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanReadField "name" VkExportSemaphoreWin32HandleInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, name})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         CanWriteField "name" VkExportSemaphoreWin32HandleInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

instance Show VkExportSemaphoreWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkExportSemaphoreWin32HandleInfoKHR {" .
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
