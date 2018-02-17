#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_NV_win32_keyed_mutex
       (-- * Vulkan extension: @VK_NV_win32_keyed_mutex@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Carsten Rohde@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @59@
        --
        -- Required extensions: 'VK_NV_external_memory_win32'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_WIN32_KHR@
        --

        -- ** Required extensions: 'VK_NV_external_memory_win32'.
        VkWin32KeyedMutexAcquireReleaseInfoNV(..),
        VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION,
        pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION,
        VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME,
        pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkSubmitInfo)
import           Graphics.Vulkan.Common           (VkDeviceMemory,
                                                   VkStructureType (..), Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkWin32KeyedMutexAcquireReleaseInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         acquireCount;
--   >     const VkDeviceMemory*            pAcquireSyncs;
--   >     const uint64_t*                  pAcquireKeys;
--   >     const uint32_t*                  pAcquireTimeoutMilliseconds;
--   >     uint32_t                         releaseCount;
--   >     const VkDeviceMemory*            pReleaseSyncs;
--   >     const uint64_t*                  pReleaseKeys;
--   > } VkWin32KeyedMutexAcquireReleaseInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkWin32KeyedMutexAcquireReleaseInfoNV.html VkWin32KeyedMutexAcquireReleaseInfoNV registry at www.khronos.org>
data VkWin32KeyedMutexAcquireReleaseInfoNV = VkWin32KeyedMutexAcquireReleaseInfoNV## Addr##
                                                                                    ByteArray##

instance Eq VkWin32KeyedMutexAcquireReleaseInfoNV where
        (VkWin32KeyedMutexAcquireReleaseInfoNV## a _) ==
          x@(VkWin32KeyedMutexAcquireReleaseInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkWin32KeyedMutexAcquireReleaseInfoNV where
        (VkWin32KeyedMutexAcquireReleaseInfoNV## a _) `compare`
          x@(VkWin32KeyedMutexAcquireReleaseInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkWin32KeyedMutexAcquireReleaseInfoNV where
        sizeOf ~_
          = #{size VkWin32KeyedMutexAcquireReleaseInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkWin32KeyedMutexAcquireReleaseInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        unsafeAddr (VkWin32KeyedMutexAcquireReleaseInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkWin32KeyedMutexAcquireReleaseInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkWin32KeyedMutexAcquireReleaseInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkWin32KeyedMutexAcquireReleaseInfoNV where
        type StructFields VkWin32KeyedMutexAcquireReleaseInfoNV =
             '["sType", "pNext", "acquireCount", "pAcquireSyncs", -- ' closing tick for hsc2hs
               "pAcquireKeys", "pAcquireTimeoutMilliseconds", "releaseCount",
               "pReleaseSyncs", "pReleaseKeys"]
        type CUnionType VkWin32KeyedMutexAcquireReleaseInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkWin32KeyedMutexAcquireReleaseInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkWin32KeyedMutexAcquireReleaseInfoNV =
             '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkSTypeMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             VkStructureType
        type FieldOptional "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

instance CanReadField "sType" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkPNextMType VkWin32KeyedMutexAcquireReleaseInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr Void
        type FieldOptional "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

instance CanReadField "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkAcquireCount VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkAcquireCountMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Word32

        {-# NOINLINE vkAcquireCount #-}
        vkAcquireCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount})

        {-# INLINE vkAcquireCountByteOffset #-}
        vkAcquireCountByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

        {-# INLINE readVkAcquireCount #-}
        readVkAcquireCount p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

        {-# INLINE writeVkAcquireCount #-}
        writeVkAcquireCount p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

instance {-# OVERLAPPING #-}
         HasField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoNV
             = Word32
        type FieldOptional "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

instance CanReadField "acquireCount"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE getField #-}
        getField = vkAcquireCount

        {-# INLINE readField #-}
        readField = readVkAcquireCount

instance CanWriteField "acquireCount"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkAcquireCount

instance {-# OVERLAPPING #-}
         HasVkPAcquireSyncs VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkPAcquireSyncsMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr VkDeviceMemory

        {-# NOINLINE vkPAcquireSyncs #-}
        vkPAcquireSyncs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs})

        {-# INLINE vkPAcquireSyncsByteOffset #-}
        vkPAcquireSyncsByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

        {-# INLINE readVkPAcquireSyncs #-}
        readVkPAcquireSyncs p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

        {-# INLINE writeVkPAcquireSyncs #-}
        writeVkPAcquireSyncs p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         HasField "pAcquireSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        type FieldType "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr VkDeviceMemory
        type FieldOptional "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

instance CanReadField "pAcquireSyncs"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPAcquireSyncs

        {-# INLINE readField #-}
        readField = readVkPAcquireSyncs

instance CanWriteField "pAcquireSyncs"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPAcquireSyncs

instance {-# OVERLAPPING #-}
         HasVkPAcquireKeys VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkPAcquireKeysMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr Word64

        {-# NOINLINE vkPAcquireKeys #-}
        vkPAcquireKeys x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys})

        {-# INLINE vkPAcquireKeysByteOffset #-}
        vkPAcquireKeysByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

        {-# INLINE readVkPAcquireKeys #-}
        readVkPAcquireKeys p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

        {-# INLINE writeVkPAcquireKeys #-}
        writeVkPAcquireKeys p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

instance {-# OVERLAPPING #-}
         HasField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr Word64
        type FieldOptional "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

instance CanReadField "pAcquireKeys"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPAcquireKeys

        {-# INLINE readField #-}
        readField = readVkPAcquireKeys

instance CanWriteField "pAcquireKeys"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPAcquireKeys

instance {-# OVERLAPPING #-}
         HasVkPAcquireTimeoutMilliseconds
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        type VkPAcquireTimeoutMillisecondsMType
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr Word32

        {-# NOINLINE vkPAcquireTimeoutMilliseconds #-}
        vkPAcquireTimeoutMilliseconds x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds})

        {-# INLINE vkPAcquireTimeoutMillisecondsByteOffset #-}
        vkPAcquireTimeoutMillisecondsByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

        {-# INLINE readVkPAcquireTimeoutMilliseconds #-}
        readVkPAcquireTimeoutMilliseconds p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

        {-# INLINE writeVkPAcquireTimeoutMilliseconds #-}
        writeVkPAcquireTimeoutMilliseconds p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

instance {-# OVERLAPPING #-}
         HasField "pAcquireTimeoutMilliseconds"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        type FieldType "pAcquireTimeoutMilliseconds"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr Word32
        type FieldOptional "pAcquireTimeoutMilliseconds"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireTimeoutMilliseconds"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

instance CanReadField "pAcquireTimeoutMilliseconds"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPAcquireTimeoutMilliseconds

        {-# INLINE readField #-}
        readField = readVkPAcquireTimeoutMilliseconds

instance CanWriteField "pAcquireTimeoutMilliseconds"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPAcquireTimeoutMilliseconds

instance {-# OVERLAPPING #-}
         HasVkReleaseCount VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkReleaseCountMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Word32

        {-# NOINLINE vkReleaseCount #-}
        vkReleaseCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount})

        {-# INLINE vkReleaseCountByteOffset #-}
        vkReleaseCountByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

        {-# INLINE readVkReleaseCount #-}
        readVkReleaseCount p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

        {-# INLINE writeVkReleaseCount #-}
        writeVkReleaseCount p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

instance {-# OVERLAPPING #-}
         HasField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoNV
             = Word32
        type FieldOptional "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

instance CanReadField "releaseCount"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE getField #-}
        getField = vkReleaseCount

        {-# INLINE readField #-}
        readField = readVkReleaseCount

instance CanWriteField "releaseCount"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkReleaseCount

instance {-# OVERLAPPING #-}
         HasVkPReleaseSyncs VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkPReleaseSyncsMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr VkDeviceMemory

        {-# NOINLINE vkPReleaseSyncs #-}
        vkPReleaseSyncs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs})

        {-# INLINE vkPReleaseSyncsByteOffset #-}
        vkPReleaseSyncsByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

        {-# INLINE readVkPReleaseSyncs #-}
        readVkPReleaseSyncs p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

        {-# INLINE writeVkPReleaseSyncs #-}
        writeVkPReleaseSyncs p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         HasField "pReleaseSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        type FieldType "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr VkDeviceMemory
        type FieldOptional "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

instance CanReadField "pReleaseSyncs"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPReleaseSyncs

        {-# INLINE readField #-}
        readField = readVkPReleaseSyncs

instance CanWriteField "pReleaseSyncs"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPReleaseSyncs

instance {-# OVERLAPPING #-}
         HasVkPReleaseKeys VkWin32KeyedMutexAcquireReleaseInfoNV where
        type VkPReleaseKeysMType VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr Word64

        {-# NOINLINE vkPReleaseKeys #-}
        vkPReleaseKeys x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys})

        {-# INLINE vkPReleaseKeysByteOffset #-}
        vkPReleaseKeysByteOffset ~_
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

        {-# INLINE readVkPReleaseKeys #-}
        readVkPReleaseKeys p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

        {-# INLINE writeVkPReleaseKeys #-}
        writeVkPReleaseKeys p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

instance {-# OVERLAPPING #-}
         HasField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
             = Ptr Word64
        type FieldOptional "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

instance CanReadField "pReleaseKeys"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPReleaseKeys

        {-# INLINE readField #-}
        readField = readVkPReleaseKeys

instance CanWriteField "pReleaseKeys"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPReleaseKeys

instance Show VkWin32KeyedMutexAcquireReleaseInfoNV where
        showsPrec d x
          = showString "VkWin32KeyedMutexAcquireReleaseInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAcquireCount = " .
                            showsPrec d (vkAcquireCount x) .
                              showString ", " .
                                showString "vkPAcquireSyncs = " .
                                  showsPrec d (vkPAcquireSyncs x) .
                                    showString ", " .
                                      showString "vkPAcquireKeys = " .
                                        showsPrec d (vkPAcquireKeys x) .
                                          showString ", " .
                                            showString "vkPAcquireTimeoutMilliseconds = " .
                                              showsPrec d (vkPAcquireTimeoutMilliseconds x) .
                                                showString ", " .
                                                  showString "vkReleaseCount = " .
                                                    showsPrec d (vkReleaseCount x) .
                                                      showString ", " .
                                                        showString "vkPReleaseSyncs = " .
                                                          showsPrec d (vkPReleaseSyncs x) .
                                                            showString ", " .
                                                              showString "vkPReleaseKeys = " .
                                                                showsPrec d (vkPReleaseKeys x) .
                                                                  showChar '}'

pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

type VK_NV_WIN32_KEYED_MUTEX_SPEC_VERSION = 1

pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString

pattern VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME <-
        (is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME -> True)
  where VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
          = _VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME

{-# INLINE _VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME #-}

_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString
_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  = Ptr "VK_NV_win32_keyed_mutex\NUL"##

{-# INLINE is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME #-}

is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME :: CString -> Bool
is_VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME
  = eqCStrings _VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME

type VK_NV_WIN32_KEYED_MUTEX_EXTENSION_NAME =
     "VK_NV_win32_keyed_mutex"

pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
        = VkStructureType 1000058000
