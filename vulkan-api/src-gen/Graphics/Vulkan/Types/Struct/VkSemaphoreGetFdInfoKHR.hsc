#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSemaphoreGetFdInfoKHR
       (VkSemaphoreGetFdInfoKHR(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalSemaphoreHandleTypeFlagsKHR
                                                                                   (VkExternalSemaphoreHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                   (VkStructureType)
import           Graphics.Vulkan.Types.Handles
                                                                                   (VkSemaphore)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkSemaphoreGetFdInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkSemaphore                      semaphore;
--   >     VkExternalSemaphoreHandleTypeFlagBitsKHR handleType;
--   > } VkSemaphoreGetFdInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSemaphoreGetFdInfoKHR.html VkSemaphoreGetFdInfoKHR registry at www.khronos.org>
data VkSemaphoreGetFdInfoKHR = VkSemaphoreGetFdInfoKHR## Addr##
                                                        ByteArray##

instance Eq VkSemaphoreGetFdInfoKHR where
        (VkSemaphoreGetFdInfoKHR## a _) == x@(VkSemaphoreGetFdInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSemaphoreGetFdInfoKHR where
        (VkSemaphoreGetFdInfoKHR## a _) `compare`
          x@(VkSemaphoreGetFdInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSemaphoreGetFdInfoKHR where
        sizeOf ~_ = #{size VkSemaphoreGetFdInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSemaphoreGetFdInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSemaphoreGetFdInfoKHR where
        unsafeAddr (VkSemaphoreGetFdInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSemaphoreGetFdInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSemaphoreGetFdInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSemaphoreGetFdInfoKHR where
        type StructFields VkSemaphoreGetFdInfoKHR =
             '["sType", "pNext", "semaphore", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSemaphoreGetFdInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkSemaphoreGetFdInfoKHR
         where
        type VkSTypeMType VkSemaphoreGetFdInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetFdInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSemaphoreGetFdInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSemaphoreGetFdInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSemaphoreGetFdInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSemaphoreGetFdInfoKHR where
        type FieldType "sType" VkSemaphoreGetFdInfoKHR = VkStructureType
        type FieldOptional "sType" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSemaphoreGetFdInfoKHR =
             #{offset VkSemaphoreGetFdInfoKHR, sType}
        type FieldIsArray "sType" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSemaphoreGetFdInfoKHR, sType}

instance CanReadField "sType" VkSemaphoreGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSemaphoreGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSemaphoreGetFdInfoKHR
         where
        type VkPNextMType VkSemaphoreGetFdInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetFdInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSemaphoreGetFdInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSemaphoreGetFdInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSemaphoreGetFdInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSemaphoreGetFdInfoKHR where
        type FieldType "pNext" VkSemaphoreGetFdInfoKHR = Ptr Void
        type FieldOptional "pNext" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSemaphoreGetFdInfoKHR =
             #{offset VkSemaphoreGetFdInfoKHR, pNext}
        type FieldIsArray "pNext" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSemaphoreGetFdInfoKHR, pNext}

instance CanReadField "pNext" VkSemaphoreGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSemaphoreGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkSemaphore VkSemaphoreGetFdInfoKHR
         where
        type VkSemaphoreMType VkSemaphoreGetFdInfoKHR = VkSemaphore

        {-# NOINLINE vkSemaphore #-}
        vkSemaphore x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetFdInfoKHR, semaphore})

        {-# INLINE vkSemaphoreByteOffset #-}
        vkSemaphoreByteOffset ~_
          = #{offset VkSemaphoreGetFdInfoKHR, semaphore}

        {-# INLINE readVkSemaphore #-}
        readVkSemaphore p
          = peekByteOff p #{offset VkSemaphoreGetFdInfoKHR, semaphore}

        {-# INLINE writeVkSemaphore #-}
        writeVkSemaphore p
          = pokeByteOff p #{offset VkSemaphoreGetFdInfoKHR, semaphore}

instance {-# OVERLAPPING #-}
         HasField "semaphore" VkSemaphoreGetFdInfoKHR where
        type FieldType "semaphore" VkSemaphoreGetFdInfoKHR = VkSemaphore
        type FieldOptional "semaphore" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "semaphore" VkSemaphoreGetFdInfoKHR =
             #{offset VkSemaphoreGetFdInfoKHR, semaphore}
        type FieldIsArray "semaphore" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetFdInfoKHR, semaphore}

instance CanReadField "semaphore" VkSemaphoreGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkSemaphore

        {-# INLINE readField #-}
        readField = readVkSemaphore

instance CanWriteField "semaphore" VkSemaphoreGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSemaphore

instance {-# OVERLAPPING #-}
         HasVkHandleType VkSemaphoreGetFdInfoKHR where
        type VkHandleTypeMType VkSemaphoreGetFdInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreGetFdInfoKHR, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkSemaphoreGetFdInfoKHR, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkSemaphoreGetFdInfoKHR, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkSemaphoreGetFdInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkSemaphoreGetFdInfoKHR where
        type FieldType "handleType" VkSemaphoreGetFdInfoKHR =
             VkExternalSemaphoreHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkSemaphoreGetFdInfoKHR =
             #{offset VkSemaphoreGetFdInfoKHR, handleType}
        type FieldIsArray "handleType" VkSemaphoreGetFdInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSemaphoreGetFdInfoKHR, handleType}

instance CanReadField "handleType" VkSemaphoreGetFdInfoKHR where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkSemaphoreGetFdInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance Show VkSemaphoreGetFdInfoKHR where
        showsPrec d x
          = showString "VkSemaphoreGetFdInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSemaphore = " .
                            showsPrec d (vkSemaphore x) .
                              showString ", " .
                                showString "vkHandleType = " .
                                  showsPrec d (vkHandleType x) . showChar '}'
