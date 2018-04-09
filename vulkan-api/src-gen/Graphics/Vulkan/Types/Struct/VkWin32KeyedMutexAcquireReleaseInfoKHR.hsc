#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkWin32KeyedMutexAcquireReleaseInfoKHR
       (VkWin32KeyedMutexAcquireReleaseInfoKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkDeviceMemory)
import           Graphics.Vulkan.Types.Struct.VkSubmitInfo  (VkSubmitInfo)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkWin32KeyedMutexAcquireReleaseInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         acquireCount;
--   >     const VkDeviceMemory* pAcquireSyncs;
--   >     const uint64_t* pAcquireKeys;
--   >     const uint32_t* pAcquireTimeouts;
--   >     uint32_t         releaseCount;
--   >     const VkDeviceMemory* pReleaseSyncs;
--   >     const uint64_t* pReleaseKeys;
--   > } VkWin32KeyedMutexAcquireReleaseInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkWin32KeyedMutexAcquireReleaseInfoKHRVkWin32KeyedMutexAcquireReleaseInfoKHR registry at www.khronos.org>
data VkWin32KeyedMutexAcquireReleaseInfoKHR = VkWin32KeyedMutexAcquireReleaseInfoKHR## Addr##
                                                                                      ByteArray##

instance Eq VkWin32KeyedMutexAcquireReleaseInfoKHR where
        (VkWin32KeyedMutexAcquireReleaseInfoKHR## a _) ==
          x@(VkWin32KeyedMutexAcquireReleaseInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkWin32KeyedMutexAcquireReleaseInfoKHR where
        (VkWin32KeyedMutexAcquireReleaseInfoKHR## a _) `compare`
          x@(VkWin32KeyedMutexAcquireReleaseInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkWin32KeyedMutexAcquireReleaseInfoKHR where
        sizeOf ~_
          = #{size VkWin32KeyedMutexAcquireReleaseInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkWin32KeyedMutexAcquireReleaseInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        unsafeAddr (VkWin32KeyedMutexAcquireReleaseInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkWin32KeyedMutexAcquireReleaseInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkWin32KeyedMutexAcquireReleaseInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type StructFields VkWin32KeyedMutexAcquireReleaseInfoKHR =
             '["sType", "pNext", "acquireCount", "pAcquireSyncs", -- ' closing tick for hsc2hs
               "pAcquireKeys", "pAcquireTimeouts", "releaseCount",
               "pReleaseSyncs", "pReleaseKeys"]
        type CUnionType VkWin32KeyedMutexAcquireReleaseInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkWin32KeyedMutexAcquireReleaseInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkWin32KeyedMutexAcquireReleaseInfoKHR =
             '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type FieldType "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}
        type FieldIsArray "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        type FieldType "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}
        type FieldIsArray "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkWin32KeyedMutexAcquireReleaseInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Word32
        type FieldOptional "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}
        type FieldIsArray "acquireCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}

instance {-# OVERLAPPING #-}
         CanReadField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}

instance {-# OVERLAPPING #-}
         CanWriteField "acquireCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, acquireCount}

instance {-# OVERLAPPING #-}
         HasField "pAcquireSyncs" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr VkDeviceMemory
        type FieldOptional "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}
        type FieldIsArray "pAcquireSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireSyncs" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireSyncs"
           VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireSyncs}

instance {-# OVERLAPPING #-}
         HasField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr Word64
        type FieldOptional "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}
        type FieldIsArray "pAcquireKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireKeys}

instance {-# OVERLAPPING #-}
         HasField "pAcquireTimeouts" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "pAcquireTimeouts"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr Word32
        type FieldOptional "pAcquireTimeouts"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAcquireTimeouts"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}
        type FieldIsArray "pAcquireTimeouts"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}

instance {-# OVERLAPPING #-}
         CanReadField "pAcquireTimeouts"
           VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}

instance {-# OVERLAPPING #-}
         CanWriteField "pAcquireTimeouts"
           VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pAcquireTimeouts}

instance {-# OVERLAPPING #-}
         HasField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Word32
        type FieldOptional "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}
        type FieldIsArray "releaseCount"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}

instance {-# OVERLAPPING #-}
         CanReadField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}

instance {-# OVERLAPPING #-}
         CanWriteField "releaseCount" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, releaseCount}

instance {-# OVERLAPPING #-}
         HasField "pReleaseSyncs" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr VkDeviceMemory
        type FieldOptional "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}
        type FieldIsArray "pReleaseSyncs"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         CanReadField "pReleaseSyncs" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         CanWriteField "pReleaseSyncs"
           VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseSyncs}

instance {-# OVERLAPPING #-}
         HasField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        type FieldType "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = Ptr Word64
        type FieldOptional "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             =
             #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}
        type FieldIsArray "pReleaseKeys"
               VkWin32KeyedMutexAcquireReleaseInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}

instance {-# OVERLAPPING #-}
         CanReadField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}

instance {-# OVERLAPPING #-}
         CanWriteField "pReleaseKeys" VkWin32KeyedMutexAcquireReleaseInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkWin32KeyedMutexAcquireReleaseInfoKHR, pReleaseKeys}

instance Show VkWin32KeyedMutexAcquireReleaseInfoKHR where
        showsPrec d x
          = showString "VkWin32KeyedMutexAcquireReleaseInfoKHR {" .
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
                                            showString "pAcquireTimeouts = " .
                                              showsPrec d (getField @"pAcquireTimeouts" x) .
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
