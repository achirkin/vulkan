#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRefreshCycleDurationGOOGLE
       (VkRefreshCycleDurationGOOGLE(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkRefreshCycleDurationGOOGLE {
--   >     uint64_t                         refreshDuration;
--   > } VkRefreshCycleDurationGOOGLE;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkRefreshCycleDurationGOOGLE.html VkRefreshCycleDurationGOOGLE registry at www.khronos.org>
data VkRefreshCycleDurationGOOGLE = VkRefreshCycleDurationGOOGLE## Addr##
                                                                  ByteArray##

instance Eq VkRefreshCycleDurationGOOGLE where
        (VkRefreshCycleDurationGOOGLE## a _) ==
          x@(VkRefreshCycleDurationGOOGLE## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRefreshCycleDurationGOOGLE where
        (VkRefreshCycleDurationGOOGLE## a _) `compare`
          x@(VkRefreshCycleDurationGOOGLE## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRefreshCycleDurationGOOGLE where
        sizeOf ~_ = #{size VkRefreshCycleDurationGOOGLE}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRefreshCycleDurationGOOGLE}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRefreshCycleDurationGOOGLE where
        unsafeAddr (VkRefreshCycleDurationGOOGLE## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRefreshCycleDurationGOOGLE## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRefreshCycleDurationGOOGLE##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRefreshCycleDurationGOOGLE where
        type StructFields VkRefreshCycleDurationGOOGLE =
             '["refreshDuration"] -- ' closing tick for hsc2hs
        type CUnionType VkRefreshCycleDurationGOOGLE = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRefreshCycleDurationGOOGLE = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRefreshCycleDurationGOOGLE = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkRefreshDuration VkRefreshCycleDurationGOOGLE where
        type VkRefreshDurationMType VkRefreshCycleDurationGOOGLE = Word64

        {-# NOINLINE vkRefreshDuration #-}
        vkRefreshDuration x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRefreshCycleDurationGOOGLE, refreshDuration})

        {-# INLINE vkRefreshDurationByteOffset #-}
        vkRefreshDurationByteOffset ~_
          = #{offset VkRefreshCycleDurationGOOGLE, refreshDuration}

        {-# INLINE readVkRefreshDuration #-}
        readVkRefreshDuration p
          = peekByteOff p #{offset VkRefreshCycleDurationGOOGLE, refreshDuration}

        {-# INLINE writeVkRefreshDuration #-}
        writeVkRefreshDuration p
          = pokeByteOff p #{offset VkRefreshCycleDurationGOOGLE, refreshDuration}

instance {-# OVERLAPPING #-}
         HasField "refreshDuration" VkRefreshCycleDurationGOOGLE where
        type FieldType "refreshDuration" VkRefreshCycleDurationGOOGLE =
             Word64
        type FieldOptional "refreshDuration" VkRefreshCycleDurationGOOGLE =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "refreshDuration" VkRefreshCycleDurationGOOGLE =
             #{offset VkRefreshCycleDurationGOOGLE, refreshDuration}
        type FieldIsArray "refreshDuration" VkRefreshCycleDurationGOOGLE =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRefreshCycleDurationGOOGLE, refreshDuration}

instance CanReadField "refreshDuration"
           VkRefreshCycleDurationGOOGLE
         where
        {-# INLINE getField #-}
        getField = vkRefreshDuration

        {-# INLINE readField #-}
        readField = readVkRefreshDuration

instance CanWriteField "refreshDuration"
           VkRefreshCycleDurationGOOGLE
         where
        {-# INLINE writeField #-}
        writeField = writeVkRefreshDuration

instance Show VkRefreshCycleDurationGOOGLE where
        showsPrec d x
          = showString "VkRefreshCycleDurationGOOGLE {" .
              showString "vkRefreshDuration = " .
                showsPrec d (vkRefreshDuration x) . showChar '}'
