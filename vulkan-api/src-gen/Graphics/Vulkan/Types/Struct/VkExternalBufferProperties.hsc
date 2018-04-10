#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExternalBufferProperties
       (VkExternalBufferProperties(..)) where
import           Foreign.Storable                                        (Storable (..))
import           GHC.Base                                                (Addr##, ByteArray##,
                                                                          byteArrayContents##,
                                                                          plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType              (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkExternalMemoryProperties (VkExternalMemoryProperties)
import           System.IO.Unsafe                                        (unsafeDupablePerformIO)

-- | > typedef struct VkExternalBufferProperties {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkExternalMemoryProperties    externalMemoryProperties;
--   > } VkExternalBufferProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkExternalBufferProperties VkExternalBufferProperties registry at www.khronos.org>
data VkExternalBufferProperties = VkExternalBufferProperties## Addr##
                                                              ByteArray##

instance Eq VkExternalBufferProperties where
        (VkExternalBufferProperties## a _) ==
          x@(VkExternalBufferProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExternalBufferProperties where
        (VkExternalBufferProperties## a _) `compare`
          x@(VkExternalBufferProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExternalBufferProperties where
        sizeOf ~_ = #{size VkExternalBufferProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExternalBufferProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExternalBufferProperties where
        unsafeAddr (VkExternalBufferProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExternalBufferProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExternalBufferProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExternalBufferProperties where
        type StructFields VkExternalBufferProperties =
             '["sType", "pNext", "externalMemoryProperties"] -- ' closing tick for hsc2hs
        type CUnionType VkExternalBufferProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExternalBufferProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExternalBufferProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkExternalBufferProperties where
        type FieldType "sType" VkExternalBufferProperties = VkStructureType
        type FieldOptional "sType" VkExternalBufferProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExternalBufferProperties =
             #{offset VkExternalBufferProperties, sType}
        type FieldIsArray "sType" VkExternalBufferProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalBufferProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkExternalBufferProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalBufferProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkExternalBufferProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalBufferProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExternalBufferProperties where
        type FieldType "pNext" VkExternalBufferProperties = Ptr Void
        type FieldOptional "pNext" VkExternalBufferProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExternalBufferProperties =
             #{offset VkExternalBufferProperties, pNext}
        type FieldIsArray "pNext" VkExternalBufferProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalBufferProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkExternalBufferProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalBufferProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkExternalBufferProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalBufferProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "externalMemoryProperties" VkExternalBufferProperties
         where
        type FieldType "externalMemoryProperties"
               VkExternalBufferProperties
             = VkExternalMemoryProperties
        type FieldOptional "externalMemoryProperties"
               VkExternalBufferProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "externalMemoryProperties"
               VkExternalBufferProperties
             =
             #{offset VkExternalBufferProperties, externalMemoryProperties}
        type FieldIsArray "externalMemoryProperties"
               VkExternalBufferProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExternalBufferProperties, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanReadField "externalMemoryProperties" VkExternalBufferProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExternalBufferProperties, externalMemoryProperties})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExternalBufferProperties, externalMemoryProperties}

instance {-# OVERLAPPING #-}
         CanWriteField "externalMemoryProperties" VkExternalBufferProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExternalBufferProperties, externalMemoryProperties}

instance Show VkExternalBufferProperties where
        showsPrec d x
          = showString "VkExternalBufferProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "externalMemoryProperties = " .
                            showsPrec d (getField @"externalMemoryProperties" x) . showChar '}'
