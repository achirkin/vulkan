#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkFenceGetFdInfoKHR
       (VkFenceGetFdInfoKHR(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalFenceHandleTypeFlags (VkExternalFenceHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType                (VkStructureType)
import           Graphics.Vulkan.Types.Handles                             (VkFence)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkFenceGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                            pNext;
--   >     VkFence                                fence;
--   >     VkExternalFenceHandleTypeFlagBits   handleType;
--   > } VkFenceGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkFenceGetFdInfoKHR.html VkFenceGetFdInfoKHR registry at www.khronos.org>
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
