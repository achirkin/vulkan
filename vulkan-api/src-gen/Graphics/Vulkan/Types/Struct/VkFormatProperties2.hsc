#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkFormatProperties2
       (VkFormatProperties2(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Base                                        (Addr##,
                                                                  ByteArray##,
                                                                  byteArrayContents##,
                                                                  plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkFormatProperties (VkFormatProperties)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkFormatProperties2 {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkFormatProperties               formatProperties;
--   > } VkFormatProperties2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkFormatProperties2VkFormatProperties2 registry at www.khronos.org>
data VkFormatProperties2 = VkFormatProperties2## Addr## ByteArray##

instance Eq VkFormatProperties2 where
        (VkFormatProperties2## a _) == x@(VkFormatProperties2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkFormatProperties2 where
        (VkFormatProperties2## a _) `compare` x@(VkFormatProperties2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkFormatProperties2 where
        sizeOf ~_ = #{size VkFormatProperties2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFormatProperties2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkFormatProperties2 where
        unsafeAddr (VkFormatProperties2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkFormatProperties2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkFormatProperties2## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkFormatProperties2 where
        type StructFields VkFormatProperties2 =
             '["sType", "pNext", "formatProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkFormatProperties2 = 'True -- ' closing tick for hsc2hs
        type StructExtends VkFormatProperties2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkFormatProperties2
         where
        type FieldType "sType" VkFormatProperties2 = VkStructureType
        type FieldOptional "sType" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkFormatProperties2 =
             #{offset VkFormatProperties2, sType}
        type FieldIsArray "sType" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties2, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkFormatProperties2
         where
        type FieldType "pNext" VkFormatProperties2 = Ptr Void
        type FieldOptional "pNext" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkFormatProperties2 =
             #{offset VkFormatProperties2, pNext}
        type FieldIsArray "pNext" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties2, pNext}

instance {-# OVERLAPPING #-}
         HasField "formatProperties" VkFormatProperties2 where
        type FieldType "formatProperties" VkFormatProperties2 =
             VkFormatProperties
        type FieldOptional "formatProperties" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "formatProperties" VkFormatProperties2 =
             #{offset VkFormatProperties2, formatProperties}
        type FieldIsArray "formatProperties" VkFormatProperties2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkFormatProperties2, formatProperties}

instance {-# OVERLAPPING #-}
         CanReadField "formatProperties" VkFormatProperties2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2, formatProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkFormatProperties2, formatProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "formatProperties" VkFormatProperties2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkFormatProperties2, formatProperties}

instance Show VkFormatProperties2 where
        showsPrec d x
          = showString "VkFormatProperties2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "formatProperties = " .
                            showsPrec d (getField @"formatProperties" x) . showChar '}'
