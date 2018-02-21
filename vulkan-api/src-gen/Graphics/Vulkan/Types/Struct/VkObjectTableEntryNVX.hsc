#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkObjectTableEntryNVX
       (VkObjectTableEntryNVX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkObjectEntryTypeNVX       (VkObjectEntryTypeNVX)
import           Graphics.Vulkan.Types.Enum.VkObjectEntryUsageFlagsNVX (VkObjectEntryUsageFlagsNVX)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkObjectTableEntryNVX {
--   >     VkObjectEntryTypeNVX         type;
--   >     VkObjectEntryUsageFlagsNVX   flags;
--   > } VkObjectTableEntryNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkObjectTableEntryNVX.html VkObjectTableEntryNVX registry at www.khronos.org>
data VkObjectTableEntryNVX = VkObjectTableEntryNVX## Addr##
                                                    ByteArray##

instance Eq VkObjectTableEntryNVX where
        (VkObjectTableEntryNVX## a _) == x@(VkObjectTableEntryNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkObjectTableEntryNVX where
        (VkObjectTableEntryNVX## a _) `compare`
          x@(VkObjectTableEntryNVX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkObjectTableEntryNVX where
        sizeOf ~_ = #{size VkObjectTableEntryNVX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkObjectTableEntryNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkObjectTableEntryNVX where
        unsafeAddr (VkObjectTableEntryNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkObjectTableEntryNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkObjectTableEntryNVX## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkObjectTableEntryNVX where
        type StructFields VkObjectTableEntryNVX = '["type", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkObjectTableEntryNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkType VkObjectTableEntryNVX where
        type VkTypeMType VkObjectTableEntryNVX = VkObjectEntryTypeNVX

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableEntryNVX, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkObjectTableEntryNVX, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkObjectTableEntryNVX, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkObjectTableEntryNVX, type}

instance {-# OVERLAPPING #-} HasField "type" VkObjectTableEntryNVX
         where
        type FieldType "type" VkObjectTableEntryNVX = VkObjectEntryTypeNVX
        type FieldOptional "type" VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkObjectTableEntryNVX =
             #{offset VkObjectTableEntryNVX, type}
        type FieldIsArray "type" VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkObjectTableEntryNVX, type}

instance CanReadField "type" VkObjectTableEntryNVX where
        {-# INLINE getField #-}
        getField = vkType

        {-# INLINE readField #-}
        readField = readVkType

instance CanWriteField "type" VkObjectTableEntryNVX where
        {-# INLINE writeField #-}
        writeField = writeVkType

instance {-# OVERLAPPING #-} HasVkFlags VkObjectTableEntryNVX where
        type VkFlagsMType VkObjectTableEntryNVX =
             VkObjectEntryUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkObjectTableEntryNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkObjectTableEntryNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkObjectTableEntryNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkObjectTableEntryNVX, flags}

instance {-# OVERLAPPING #-} HasField "flags" VkObjectTableEntryNVX
         where
        type FieldType "flags" VkObjectTableEntryNVX =
             VkObjectEntryUsageFlagsNVX
        type FieldOptional "flags" VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkObjectTableEntryNVX =
             #{offset VkObjectTableEntryNVX, flags}
        type FieldIsArray "flags" VkObjectTableEntryNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkObjectTableEntryNVX, flags}

instance CanReadField "flags" VkObjectTableEntryNVX where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkObjectTableEntryNVX where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance Show VkObjectTableEntryNVX where
        showsPrec d x
          = showString "VkObjectTableEntryNVX {" .
              showString "vkType = " .
                showsPrec d (vkType x) .
                  showString ", " .
                    showString "vkFlags = " . showsPrec d (vkFlags x) . showChar '}'
