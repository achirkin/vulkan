#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.EventCreateInfo
       (VkEventCreateInfo(..)) where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks           (VkEventCreateFlags)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkEventCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkEventCreateFlags     flags;
--   > } VkEventCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkEventCreateInfo VkEventCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} CanReadField "sType" VkEventCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkEventCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkEventCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkEventCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkEventCreateInfo, sType}

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

instance {-# OVERLAPPING #-} CanReadField "pNext" VkEventCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkEventCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkEventCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkEventCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkEventCreateInfo, pNext}

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

instance {-# OVERLAPPING #-} CanReadField "flags" VkEventCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkEventCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkEventCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkEventCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkEventCreateInfo, flags}

instance Show VkEventCreateInfo where
        showsPrec d x
          = showString "VkEventCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) . showChar '}'
