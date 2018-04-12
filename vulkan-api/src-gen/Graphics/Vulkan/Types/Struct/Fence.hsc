#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.Fence
       (VkFenceCreateInfo(..), VkFenceGetFdInfoKHR(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.External      (VkExternalFenceHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.Fence         (VkFenceCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkFence)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkFenceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkFenceCreateFlags     flags;
--   > } VkFenceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFenceCreateInfo VkFenceCreateInfo registry at www.khronos.org>
data VkFenceCreateInfo = VkFenceCreateInfo## Addr## ByteArray##

instance Eq VkFenceCreateInfo where
        (VkFenceCreateInfo## a _) == x@(VkFenceCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFenceCreateInfo where
        (VkFenceCreateInfo## a _) `compare` x@(VkFenceCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFenceCreateInfo where
        sizeOf ~_ = #{size VkFenceCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFenceCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFenceCreateInfo where
        unsafeAddr (VkFenceCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFenceCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFenceCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFenceCreateInfo where
        type StructFields VkFenceCreateInfo = '["sType", "pNext", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkFenceCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkFenceCreateInfo
         where
        type FieldType "sType" VkFenceCreateInfo = VkStructureType
        type FieldOptional "sType" VkFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkFenceCreateInfo =
             #{offset VkFenceCreateInfo, sType}
        type FieldIsArray "sType" VkFenceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceCreateInfo, sType}

instance {-# OVERLAPPING #-} CanReadField "sType" VkFenceCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkFenceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkFenceCreateInfo
         where
        type FieldType "pNext" VkFenceCreateInfo = Ptr Void
        type FieldOptional "pNext" VkFenceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkFenceCreateInfo =
             #{offset VkFenceCreateInfo, pNext}
        type FieldIsArray "pNext" VkFenceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceCreateInfo, pNext}

instance {-# OVERLAPPING #-} CanReadField "pNext" VkFenceCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkFenceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkFenceCreateInfo
         where
        type FieldType "flags" VkFenceCreateInfo = VkFenceCreateFlags
        type FieldOptional "flags" VkFenceCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkFenceCreateInfo =
             #{offset VkFenceCreateInfo, flags}
        type FieldIsArray "flags" VkFenceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceCreateInfo, flags}

instance {-# OVERLAPPING #-} CanReadField "flags" VkFenceCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkFenceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceCreateInfo, flags}

instance Show VkFenceCreateInfo where
        showsPrec d x
          = showString "VkFenceCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) . showChar '}'

-- | > typedef struct VkFenceGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence                                fence;
--   >     VkExternalFenceHandleTypeFlagBits   handleType;
--   > } VkFenceGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkFenceGetFdInfoKHR VkFenceGetFdInfoKHR registry at www.khronos.org>
data VkFenceGetFdInfoKHR = VkFenceGetFdInfoKHR## Addr## ByteArray##

instance Eq VkFenceGetFdInfoKHR where
        (VkFenceGetFdInfoKHR## a _) == x@(VkFenceGetFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFenceGetFdInfoKHR where
        (VkFenceGetFdInfoKHR## a _) `compare` x@(VkFenceGetFdInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFenceGetFdInfoKHR where
        sizeOf ~_ = #{size VkFenceGetFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFenceGetFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFenceGetFdInfoKHR where
        unsafeAddr (VkFenceGetFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFenceGetFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFenceGetFdInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFenceGetFdInfoKHR where
        type StructFields VkFenceGetFdInfoKHR =
             '["sType", "pNext", "fence", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkFenceGetFdInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkFenceGetFdInfoKHR
         where
        type FieldType "sType" VkFenceGetFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkFenceGetFdInfoKHR =
             #{offset VkFenceGetFdInfoKHR, sType}
        type FieldIsArray "sType" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkFenceGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkFenceGetFdInfoKHR
         where
        type FieldType "pNext" VkFenceGetFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkFenceGetFdInfoKHR =
             #{offset VkFenceGetFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkFenceGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasField "fence" VkFenceGetFdInfoKHR
         where
        type FieldType "fence" VkFenceGetFdInfoKHR = VkFence
        type FieldOptional "fence" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "fence" VkFenceGetFdInfoKHR =
             #{offset VkFenceGetFdInfoKHR, fence}
        type FieldIsArray "fence" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceGetFdInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanReadField "fence" VkFenceGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, fence})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, fence}

instance {-# OVERLAPPING #-}
         CanWriteField "fence" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, fence}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkFenceGetFdInfoKHR where
        type FieldType "handleType" VkFenceGetFdInfoKHR =
             VkExternalFenceHandleTypeFlagBits
        type FieldOptional "handleType" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkFenceGetFdInfoKHR =
             #{offset VkFenceGetFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkFenceGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFenceGetFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkFenceGetFdInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFenceGetFdInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFenceGetFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkFenceGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFenceGetFdInfoKHR, handleType}

instance Show VkFenceGetFdInfoKHR where
        showsPrec d x
          = showString "VkFenceGetFdInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "fence = " .
                            showsPrec d (getField @"fence" x) .
                              showString ", " .
                                showString "handleType = " .
                                  showsPrec d (getField @"handleType" x) . showChar '}'
