#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkEventCreateInfo
       (VkEventCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkEventCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkEventCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkEventCreateFlags     flags;
--   > } VkEventCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkEventCreateInfo.html VkEventCreateInfo registry at www.khronos.org>
data VkEventCreateInfo = VkEventCreateInfo## Addr## ByteArray##

instance Eq VkEventCreateInfo where
        (VkEventCreateInfo## a _) == x@(VkEventCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkEventCreateInfo where
        (VkEventCreateInfo## a _) `compare` x@(VkEventCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkEventCreateInfo where
        sizeOf ~_ = #{size VkEventCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkEventCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkEventCreateInfo where
        unsafeAddr (VkEventCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkEventCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkEventCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkEventCreateInfo where
        type StructFields VkEventCreateInfo = '["sType", "pNext", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkEventCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkEventCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkEventCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkEventCreateInfo where
        type VkSTypeMType VkEventCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkEventCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkEventCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkEventCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkEventCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkEventCreateInfo
         where
        type FieldType "sType" VkEventCreateInfo = VkStructureType
        type FieldOptional "sType" VkEventCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkEventCreateInfo =
             #{offset VkEventCreateInfo, sType}
        type FieldIsArray "sType" VkEventCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkEventCreateInfo, sType}

instance CanReadField "sType" VkEventCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkEventCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkEventCreateInfo where
        type VkPNextMType VkEventCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkEventCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkEventCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkEventCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkEventCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkEventCreateInfo
         where
        type FieldType "pNext" VkEventCreateInfo = Ptr Void
        type FieldOptional "pNext" VkEventCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkEventCreateInfo =
             #{offset VkEventCreateInfo, pNext}
        type FieldIsArray "pNext" VkEventCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkEventCreateInfo, pNext}

instance CanReadField "pNext" VkEventCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkEventCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkEventCreateInfo where
        type VkFlagsMType VkEventCreateInfo = VkEventCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkEventCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkEventCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkEventCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkEventCreateInfo, flags}

instance {-# OVERLAPPING #-} HasField "flags" VkEventCreateInfo
         where
        type FieldType "flags" VkEventCreateInfo = VkEventCreateFlags
        type FieldOptional "flags" VkEventCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkEventCreateInfo =
             #{offset VkEventCreateInfo, flags}
        type FieldIsArray "flags" VkEventCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkEventCreateInfo, flags}

instance CanReadField "flags" VkEventCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkEventCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance Show VkEventCreateInfo where
        showsPrec d x
          = showString "VkEventCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " . showsPrec d (vkFlags x) . showChar '}'
