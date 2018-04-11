#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo
       (VkSemaphoreCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkSemaphoreCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkSemaphoreCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkSemaphoreCreateFlags flags;
--   > } VkSemaphoreCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkSemaphoreCreateInfo VkSemaphoreCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkSemaphoreCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreCreateInfo, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkSemaphoreCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreCreateInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkSemaphoreCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSemaphoreCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkSemaphoreCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkSemaphoreCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkSemaphoreCreateInfo, flags}

instance Show VkSemaphoreCreateInfo where
        showsPrec d x
          = showString "VkSemaphoreCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) . showChar '}'
