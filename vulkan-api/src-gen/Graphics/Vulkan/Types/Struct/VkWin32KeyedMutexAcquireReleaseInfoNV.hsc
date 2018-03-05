#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkWin32KeyedMutexAcquireReleaseInfoNV
       (VkWin32KeyedMutexAcquireReleaseInfoNV(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkDeviceMemory)
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo  (VkSubmitInfo)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkWin32KeyedMutexAcquireReleaseInfoNV.html VkWin32KeyedMutexAcquireReleaseInfoNV registry at www.khronos.org>
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
         HasField "sType" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             VkStructureType
        type FieldOptional "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}
        type FieldIsArray "sType" VkWin32KeyedMutexAcquireReleaseInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkWin32KeyedMutexAcquireReleaseInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkWin32KeyedMutexAcquireReleaseInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV where
        type FieldType "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             Ptr Void
        type FieldOptional "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}
        type FieldIsArray "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkWin32KeyedMutexAcquireReleaseInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pNext}

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
        type FieldIsArray "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

instance {-# OVERLAPPING #-}
         CanReadField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

instance {-# OVERLAPPING #-}
         CanWriteField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, acquireCount}

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
        type FieldIsArray "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireSyncs}

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
        type FieldIsArray "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireKeys}

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
        type FieldIsArray "pAcquireTimeoutMilliseconds"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireTimeoutMilliseconds"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireTimeoutMilliseconds"
           VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pAcquireTimeoutMilliseconds}

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
        type FieldIsArray "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

instance {-# OVERLAPPING #-}
         CanReadField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

instance {-# OVERLAPPING #-}
         CanWriteField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, releaseCount}

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
        type FieldIsArray "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         CanReadField "pReleaseSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         CanWriteField "pReleaseSyncs" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseSyncs}

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
        type FieldIsArray "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

instance {-# OVERLAPPING #-}
         CanReadField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

instance {-# OVERLAPPING #-}
         CanWriteField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoNV, pReleaseKeys}

instance Show VkWin32KeyedMutexAcquireReleaseInfoNV where
        showsPrec d x
          = showString "VkWin32KeyedMutexAcquireReleaseInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "acquireCount = " .
                            showsPrec d (getField @"acquireCount" x) .
                              showString ", " .
                                showString "pAcquireSyncs = " .
                                  showsPrec d (getField @"pAcquireSyncs" x) .
                                    showString ", " .
                                      showString "pAcquireKeys = " .
                                        showsPrec d (getField @"pAcquireKeys" x) .
                                          showString ", " .
                                            showString "pAcquireTimeoutMilliseconds = " .
                                              showsPrec d
                                                (getField @"pAcquireTimeoutMilliseconds" x)
                                                .
                                                showString ", " .
                                                  showString "releaseCount = " .
                                                    showsPrec d (getField @"releaseCount" x) .
                                                      showString ", " .
                                                        showString "pReleaseSyncs = " .
                                                          showsPrec d (getField @"pReleaseSyncs" x)
                                                            .
                                                            showString ", " .
                                                              showString "pReleaseKeys = " .
                                                                showsPrec d
                                                                  (getField @"pReleaseKeys" x)
                                                                  . showChar '}'
