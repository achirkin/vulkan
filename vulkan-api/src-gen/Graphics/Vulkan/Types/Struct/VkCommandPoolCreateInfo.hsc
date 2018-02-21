#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCommandPoolCreateInfo
       (VkCommandPoolCreateInfo(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkCommandPoolCreateFlags (VkCommandPoolCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType          (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkCommandPoolCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandPoolCreateFlags   flags;
--   >     uint32_t               queueFamilyIndex;
--   > } VkCommandPoolCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkCommandPoolCreateInfo.html VkCommandPoolCreateInfo registry at www.khronos.org>
data VkCommandPoolCreateInfo = VkCommandPoolCreateInfo## Addr##
                                                        ByteArray##

instance Eq VkCommandPoolCreateInfo where
        (VkCommandPoolCreateInfo## a _) == x@(VkCommandPoolCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandPoolCreateInfo where
        (VkCommandPoolCreateInfo## a _) `compare`
          x@(VkCommandPoolCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandPoolCreateInfo where
        sizeOf ~_ = #{size VkCommandPoolCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCommandPoolCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandPoolCreateInfo where
        unsafeAddr (VkCommandPoolCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandPoolCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandPoolCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandPoolCreateInfo where
        type StructFields VkCommandPoolCreateInfo =
             '["sType", "pNext", "flags", "queueFamilyIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandPoolCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkCommandPoolCreateInfo
         where
        type VkSTypeMType VkCommandPoolCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkCommandPoolCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandPoolCreateInfo where
        type FieldType "sType" VkCommandPoolCreateInfo = VkStructureType
        type FieldOptional "sType" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, sType}
        type FieldIsArray "sType" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandPoolCreateInfo, sType}

instance CanReadField "sType" VkCommandPoolCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkCommandPoolCreateInfo
         where
        type VkPNextMType VkCommandPoolCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkCommandPoolCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandPoolCreateInfo where
        type FieldType "pNext" VkCommandPoolCreateInfo = Ptr Void
        type FieldOptional "pNext" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, pNext}
        type FieldIsArray "pNext" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandPoolCreateInfo, pNext}

instance CanReadField "pNext" VkCommandPoolCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkCommandPoolCreateInfo
         where
        type VkFlagsMType VkCommandPoolCreateInfo =
             VkCommandPoolCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkCommandPoolCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkCommandPoolCreateInfo where
        type FieldType "flags" VkCommandPoolCreateInfo =
             VkCommandPoolCreateFlags
        type FieldOptional "flags" VkCommandPoolCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, flags}
        type FieldIsArray "flags" VkCommandPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandPoolCreateInfo, flags}

instance CanReadField "flags" VkCommandPoolCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkCommandPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkQueueFamilyIndex VkCommandPoolCreateInfo where
        type VkQueueFamilyIndexMType VkCommandPoolCreateInfo = Word32

        {-# NOINLINE vkQueueFamilyIndex #-}
        vkQueueFamilyIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandPoolCreateInfo, queueFamilyIndex})

        {-# INLINE vkQueueFamilyIndexByteOffset #-}
        vkQueueFamilyIndexByteOffset ~_
          = #{offset VkCommandPoolCreateInfo, queueFamilyIndex}

        {-# INLINE readVkQueueFamilyIndex #-}
        readVkQueueFamilyIndex p
          = peekByteOff p #{offset VkCommandPoolCreateInfo, queueFamilyIndex}

        {-# INLINE writeVkQueueFamilyIndex #-}
        writeVkQueueFamilyIndex p
          = pokeByteOff p #{offset VkCommandPoolCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndex" VkCommandPoolCreateInfo where
        type FieldType "queueFamilyIndex" VkCommandPoolCreateInfo = Word32
        type FieldOptional "queueFamilyIndex" VkCommandPoolCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndex" VkCommandPoolCreateInfo =
             #{offset VkCommandPoolCreateInfo, queueFamilyIndex}
        type FieldIsArray "queueFamilyIndex" VkCommandPoolCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandPoolCreateInfo, queueFamilyIndex}

instance CanReadField "queueFamilyIndex" VkCommandPoolCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkQueueFamilyIndex

        {-# INLINE readField #-}
        readField = readVkQueueFamilyIndex

instance CanWriteField "queueFamilyIndex" VkCommandPoolCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkQueueFamilyIndex

instance Show VkCommandPoolCreateInfo where
        showsPrec d x
          = showString "VkCommandPoolCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkQueueFamilyIndex = " .
                                  showsPrec d (vkQueueFamilyIndex x) . showChar '}'
