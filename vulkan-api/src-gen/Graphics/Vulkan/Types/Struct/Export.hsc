#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Export
       (VkExportFenceCreateInfo(..), VkExportFenceCreateInfoKHR,
        VkExportMemoryAllocateInfo(..), VkExportMemoryAllocateInfoKHR,
        VkExportMemoryAllocateInfoNV(..), VkExportSemaphoreCreateInfo(..),
        VkExportSemaphoreCreateInfoKHR)
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.External      (VkExternalFenceHandleTypeFlags,
                                                           VkExternalMemoryHandleTypeFlags,
                                                           VkExternalMemoryHandleTypeFlagsNV,
                                                           VkExternalSemaphoreHandleTypeFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Struct.Fence       (VkFenceCreateInfo)
import           Graphics.Vulkan.Types.Struct.Memory      (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.Struct.Semaphore   (VkSemaphoreCreateInfo)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkExportFenceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalFenceHandleTypeFlags handleTypes;
--   > } VkExportFenceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportFenceCreateInfo VkExportFenceCreateInfo registry at www.khronos.org>
data VkExportFenceCreateInfo = VkExportFenceCreateInfo## Addr##
                                                        ByteArray##

instance Eq VkExportFenceCreateInfo where
        (VkExportFenceCreateInfo## a _) == x@(VkExportFenceCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportFenceCreateInfo where
        (VkExportFenceCreateInfo## a _) `compare`
          x@(VkExportFenceCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportFenceCreateInfo where
        sizeOf ~_ = #{size VkExportFenceCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExportFenceCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportFenceCreateInfo where
        unsafeAddr (VkExportFenceCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportFenceCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportFenceCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportFenceCreateInfo where
        type StructFields VkExportFenceCreateInfo =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportFenceCreateInfo = '[VkFenceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportFenceCreateInfo where
        type FieldType "sType" VkExportFenceCreateInfo = VkStructureType
        type FieldOptional "sType" VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportFenceCreateInfo =
             #{offset VkExportFenceCreateInfo, sType}
        type FieldIsArray "sType" VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExportFenceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportFenceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportFenceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportFenceCreateInfo where
        type FieldType "pNext" VkExportFenceCreateInfo = Ptr Void
        type FieldOptional "pNext" VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportFenceCreateInfo =
             #{offset VkExportFenceCreateInfo, pNext}
        type FieldIsArray "pNext" VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkExportFenceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportFenceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportFenceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportFenceCreateInfo where
        type FieldType "handleTypes" VkExportFenceCreateInfo =
             VkExternalFenceHandleTypeFlags
        type FieldOptional "handleTypes" VkExportFenceCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportFenceCreateInfo =
             #{offset VkExportFenceCreateInfo, handleTypes}
        type FieldIsArray "handleTypes" VkExportFenceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportFenceCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExportFenceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportFenceCreateInfo, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportFenceCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExportFenceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportFenceCreateInfo, handleTypes}

instance Show VkExportFenceCreateInfo where
        showsPrec d x
          = showString "VkExportFenceCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'

-- | Alias for `VkExportFenceCreateInfo`
type VkExportFenceCreateInfoKHR = VkExportFenceCreateInfo

-- | > typedef struct VkExportMemoryAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlags handleTypes;
--   > } VkExportMemoryAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportMemoryAllocateInfo VkExportMemoryAllocateInfo registry at www.khronos.org>
data VkExportMemoryAllocateInfo = VkExportMemoryAllocateInfo## Addr##
                                                              ByteArray##

instance Eq VkExportMemoryAllocateInfo where
        (VkExportMemoryAllocateInfo## a _) ==
          x@(VkExportMemoryAllocateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryAllocateInfo where
        (VkExportMemoryAllocateInfo## a _) `compare`
          x@(VkExportMemoryAllocateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryAllocateInfo where
        sizeOf ~_ = #{size VkExportMemoryAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExportMemoryAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryAllocateInfo where
        unsafeAddr (VkExportMemoryAllocateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryAllocateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryAllocateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryAllocateInfo where
        type StructFields VkExportMemoryAllocateInfo =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryAllocateInfo =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryAllocateInfo where
        type FieldType "sType" VkExportMemoryAllocateInfo = VkStructureType
        type FieldOptional "sType" VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryAllocateInfo =
             #{offset VkExportMemoryAllocateInfo, sType}
        type FieldIsArray "sType" VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryAllocateInfo where
        type FieldType "pNext" VkExportMemoryAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryAllocateInfo =
             #{offset VkExportMemoryAllocateInfo, pNext}
        type FieldIsArray "pNext" VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportMemoryAllocateInfo where
        type FieldType "handleTypes" VkExportMemoryAllocateInfo =
             VkExternalMemoryHandleTypeFlags
        type FieldOptional "handleTypes" VkExportMemoryAllocateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportMemoryAllocateInfo =
             #{offset VkExportMemoryAllocateInfo, handleTypes}
        type FieldIsArray "handleTypes" VkExportMemoryAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExportMemoryAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfo, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExportMemoryAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfo, handleTypes}

instance Show VkExportMemoryAllocateInfo where
        showsPrec d x
          = showString "VkExportMemoryAllocateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'

-- | Alias for `VkExportMemoryAllocateInfo`
type VkExportMemoryAllocateInfoKHR = VkExportMemoryAllocateInfo

-- | > typedef struct VkExportMemoryAllocateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleTypes;
--   > } VkExportMemoryAllocateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportMemoryAllocateInfoNV VkExportMemoryAllocateInfoNV registry at www.khronos.org>
data VkExportMemoryAllocateInfoNV = VkExportMemoryAllocateInfoNV## Addr##
                                                                  ByteArray##

instance Eq VkExportMemoryAllocateInfoNV where
        (VkExportMemoryAllocateInfoNV## a _) ==
          x@(VkExportMemoryAllocateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportMemoryAllocateInfoNV where
        (VkExportMemoryAllocateInfoNV## a _) `compare`
          x@(VkExportMemoryAllocateInfoNV## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportMemoryAllocateInfoNV where
        sizeOf ~_ = #{size VkExportMemoryAllocateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportMemoryAllocateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportMemoryAllocateInfoNV where
        unsafeAddr (VkExportMemoryAllocateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportMemoryAllocateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportMemoryAllocateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportMemoryAllocateInfoNV where
        type StructFields VkExportMemoryAllocateInfoNV =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportMemoryAllocateInfoNV =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportMemoryAllocateInfoNV where
        type FieldType "sType" VkExportMemoryAllocateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportMemoryAllocateInfoNV =
             #{offset VkExportMemoryAllocateInfoNV, sType}
        type FieldIsArray "sType" VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportMemoryAllocateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportMemoryAllocateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportMemoryAllocateInfoNV where
        type FieldType "pNext" VkExportMemoryAllocateInfoNV = Ptr Void
        type FieldOptional "pNext" VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportMemoryAllocateInfoNV =
             #{offset VkExportMemoryAllocateInfoNV, pNext}
        type FieldIsArray "pNext" VkExportMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportMemoryAllocateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportMemoryAllocateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportMemoryAllocateInfoNV where
        type FieldType "handleTypes" VkExportMemoryAllocateInfoNV =
             VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "handleTypes" VkExportMemoryAllocateInfoNV =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportMemoryAllocateInfoNV =
             #{offset VkExportMemoryAllocateInfoNV, handleTypes}
        type FieldIsArray "handleTypes" VkExportMemoryAllocateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportMemoryAllocateInfoNV, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExportMemoryAllocateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportMemoryAllocateInfoNV, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportMemoryAllocateInfoNV, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExportMemoryAllocateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportMemoryAllocateInfoNV, handleTypes}

instance Show VkExportMemoryAllocateInfoNV where
        showsPrec d x
          = showString "VkExportMemoryAllocateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'

-- | > typedef struct VkExportSemaphoreCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalSemaphoreHandleTypeFlags handleTypes;
--   > } VkExportSemaphoreCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExportSemaphoreCreateInfo VkExportSemaphoreCreateInfo registry at www.khronos.org>
data VkExportSemaphoreCreateInfo = VkExportSemaphoreCreateInfo## Addr##
                                                                ByteArray##

instance Eq VkExportSemaphoreCreateInfo where
        (VkExportSemaphoreCreateInfo## a _) ==
          x@(VkExportSemaphoreCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportSemaphoreCreateInfo where
        (VkExportSemaphoreCreateInfo## a _) `compare`
          x@(VkExportSemaphoreCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportSemaphoreCreateInfo where
        sizeOf ~_ = #{size VkExportSemaphoreCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExportSemaphoreCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportSemaphoreCreateInfo where
        unsafeAddr (VkExportSemaphoreCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportSemaphoreCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportSemaphoreCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportSemaphoreCreateInfo where
        type StructFields VkExportSemaphoreCreateInfo =
             '["sType", "pNext", "handleTypes"] -- ' closing tick for hsc2hs
        type CUnionType VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportSemaphoreCreateInfo =
             '[VkSemaphoreCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportSemaphoreCreateInfo where
        type FieldType "sType" VkExportSemaphoreCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportSemaphoreCreateInfo =
             #{offset VkExportSemaphoreCreateInfo, sType}
        type FieldIsArray "sType" VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExportSemaphoreCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExportSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportSemaphoreCreateInfo where
        type FieldType "pNext" VkExportSemaphoreCreateInfo = Ptr Void
        type FieldOptional "pNext" VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportSemaphoreCreateInfo =
             #{offset VkExportSemaphoreCreateInfo, pNext}
        type FieldIsArray "pNext" VkExportSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExportSemaphoreCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExportSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "handleTypes" VkExportSemaphoreCreateInfo where
        type FieldType "handleTypes" VkExportSemaphoreCreateInfo =
             VkExternalSemaphoreHandleTypeFlags
        type FieldOptional "handleTypes" VkExportSemaphoreCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleTypes" VkExportSemaphoreCreateInfo =
             #{offset VkExportSemaphoreCreateInfo, handleTypes}
        type FieldIsArray "handleTypes" VkExportSemaphoreCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanReadField "handleTypes" VkExportSemaphoreCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreCreateInfo, handleTypes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExportSemaphoreCreateInfo, handleTypes}

instance {-# OVERLAPPING #-}
         CanWriteField "handleTypes" VkExportSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExportSemaphoreCreateInfo, handleTypes}

instance Show VkExportSemaphoreCreateInfo where
        showsPrec d x
          = showString "VkExportSemaphoreCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "handleTypes = " .
                            showsPrec d (getField @"handleTypes" x) . showChar '}'

-- | Alias for `VkExportSemaphoreCreateInfo`
type VkExportSemaphoreCreateInfoKHR = VkExportSemaphoreCreateInfo
