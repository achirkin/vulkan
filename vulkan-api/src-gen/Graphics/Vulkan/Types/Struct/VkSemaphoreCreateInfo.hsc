#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo
       (VkSemaphoreCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkSemaphoreCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkSemaphoreCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSemaphoreCreateFlags flags;
--   > } VkSemaphoreCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSemaphoreCreateInfo.html VkSemaphoreCreateInfo registry at www.khronos.org>
data VkSemaphoreCreateInfo = VkSemaphoreCreateInfo## Addr##
                                                    ByteArray##

instance Eq VkSemaphoreCreateInfo where
        (VkSemaphoreCreateInfo## a _) == x@(VkSemaphoreCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSemaphoreCreateInfo where
        (VkSemaphoreCreateInfo## a _) `compare`
          x@(VkSemaphoreCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSemaphoreCreateInfo where
        sizeOf ~_ = #{size VkSemaphoreCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSemaphoreCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSemaphoreCreateInfo where
        unsafeAddr (VkSemaphoreCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSemaphoreCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSemaphoreCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSemaphoreCreateInfo where
        type StructFields VkSemaphoreCreateInfo =
             '["sType", "pNext", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSemaphoreCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkSemaphoreCreateInfo where
        type VkSTypeMType VkSemaphoreCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSemaphoreCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSemaphoreCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSemaphoreCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkSemaphoreCreateInfo
         where
        type FieldType "sType" VkSemaphoreCreateInfo = VkStructureType
        type FieldOptional "sType" VkSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSemaphoreCreateInfo =
             #{offset VkSemaphoreCreateInfo, sType}
        type FieldIsArray "sType" VkSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSemaphoreCreateInfo, sType}

instance CanReadField "sType" VkSemaphoreCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkSemaphoreCreateInfo where
        type VkPNextMType VkSemaphoreCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSemaphoreCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSemaphoreCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSemaphoreCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkSemaphoreCreateInfo
         where
        type FieldType "pNext" VkSemaphoreCreateInfo = Ptr Void
        type FieldOptional "pNext" VkSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSemaphoreCreateInfo =
             #{offset VkSemaphoreCreateInfo, pNext}
        type FieldIsArray "pNext" VkSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSemaphoreCreateInfo, pNext}

instance CanReadField "pNext" VkSemaphoreCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkSemaphoreCreateInfo where
        type VkFlagsMType VkSemaphoreCreateInfo = VkSemaphoreCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkSemaphoreCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkSemaphoreCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkSemaphoreCreateInfo, flags}

instance {-# OVERLAPPING #-} HasField "flags" VkSemaphoreCreateInfo
         where
        type FieldType "flags" VkSemaphoreCreateInfo =
             VkSemaphoreCreateFlags
        type FieldOptional "flags" VkSemaphoreCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkSemaphoreCreateInfo =
             #{offset VkSemaphoreCreateInfo, flags}
        type FieldIsArray "flags" VkSemaphoreCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkSemaphoreCreateInfo, flags}

instance CanReadField "flags" VkSemaphoreCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance Show VkSemaphoreCreateInfo where
        showsPrec d x
          = showString "VkSemaphoreCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " . showsPrec d (vkFlags x) . showChar '}'
