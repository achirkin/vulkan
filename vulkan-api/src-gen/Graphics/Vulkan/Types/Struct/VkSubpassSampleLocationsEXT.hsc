#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkSubpassSampleLocationsEXT
       (VkSubpassSampleLocationsEXT(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Struct.VkSampleLocationsInfoEXT (VkSampleLocationsInfoEXT)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkSubpassSampleLocationsEXT {
--   >     uint32_t                         subpassIndex;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkSubpassSampleLocationsEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkSubpassSampleLocationsEXT.html VkSubpassSampleLocationsEXT registry at www.khronos.org>
data VkSubpassSampleLocationsEXT = VkSubpassSampleLocationsEXT## Addr##
                                                                ByteArray##

instance Eq VkSubpassSampleLocationsEXT where
        (VkSubpassSampleLocationsEXT## a _) ==
          x@(VkSubpassSampleLocationsEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkSubpassSampleLocationsEXT where
        (VkSubpassSampleLocationsEXT## a _) `compare`
          x@(VkSubpassSampleLocationsEXT## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkSubpassSampleLocationsEXT where
        sizeOf ~_ = #{size VkSubpassSampleLocationsEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkSubpassSampleLocationsEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkSubpassSampleLocationsEXT where
        unsafeAddr (VkSubpassSampleLocationsEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkSubpassSampleLocationsEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkSubpassSampleLocationsEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkSubpassSampleLocationsEXT where
        type StructFields VkSubpassSampleLocationsEXT =
             '["subpassIndex", "sampleLocationsInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkSubpassSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkSubpassSampleLocationsEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkSubpassSampleLocationsEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSubpassIndex VkSubpassSampleLocationsEXT where
        type VkSubpassIndexMType VkSubpassSampleLocationsEXT = Word32

        {-# NOINLINE vkSubpassIndex #-}
        vkSubpassIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassSampleLocationsEXT, subpassIndex})

        {-# INLINE vkSubpassIndexByteOffset #-}
        vkSubpassIndexByteOffset ~_
          = #{offset VkSubpassSampleLocationsEXT, subpassIndex}

        {-# INLINE readVkSubpassIndex #-}
        readVkSubpassIndex p
          = peekByteOff p #{offset VkSubpassSampleLocationsEXT, subpassIndex}

        {-# INLINE writeVkSubpassIndex #-}
        writeVkSubpassIndex p
          = pokeByteOff p #{offset VkSubpassSampleLocationsEXT, subpassIndex}

instance {-# OVERLAPPING #-}
         HasField "subpassIndex" VkSubpassSampleLocationsEXT where
        type FieldType "subpassIndex" VkSubpassSampleLocationsEXT = Word32
        type FieldOptional "subpassIndex" VkSubpassSampleLocationsEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "subpassIndex" VkSubpassSampleLocationsEXT =
             #{offset VkSubpassSampleLocationsEXT, subpassIndex}
        type FieldIsArray "subpassIndex" VkSubpassSampleLocationsEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassSampleLocationsEXT, subpassIndex}

instance CanReadField "subpassIndex" VkSubpassSampleLocationsEXT
         where
        {-# INLINE getField #-}
        getField = vkSubpassIndex

        {-# INLINE readField #-}
        readField = readVkSubpassIndex

instance CanWriteField "subpassIndex" VkSubpassSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSubpassIndex

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsInfo VkSubpassSampleLocationsEXT where
        type VkSampleLocationsInfoMType VkSubpassSampleLocationsEXT =
             VkSampleLocationsInfoEXT

        {-# NOINLINE vkSampleLocationsInfo #-}
        vkSampleLocationsInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo})

        {-# INLINE vkSampleLocationsInfoByteOffset #-}
        vkSampleLocationsInfoByteOffset ~_
          = #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE readVkSampleLocationsInfo #-}
        readVkSampleLocationsInfo p
          = peekByteOff p #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

        {-# INLINE writeVkSampleLocationsInfo #-}
        writeVkSampleLocationsInfo p
          = pokeByteOff p #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsInfo" VkSubpassSampleLocationsEXT where
        type FieldType "sampleLocationsInfo" VkSubpassSampleLocationsEXT =
             VkSampleLocationsInfoEXT
        type FieldOptional "sampleLocationsInfo"
               VkSubpassSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsInfo" VkSubpassSampleLocationsEXT
             =
             #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}
        type FieldIsArray "sampleLocationsInfo" VkSubpassSampleLocationsEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSubpassSampleLocationsEXT, sampleLocationsInfo}

instance CanReadField "sampleLocationsInfo"
           VkSubpassSampleLocationsEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationsInfo

        {-# INLINE readField #-}
        readField = readVkSampleLocationsInfo

instance CanWriteField "sampleLocationsInfo"
           VkSubpassSampleLocationsEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationsInfo

instance Show VkSubpassSampleLocationsEXT where
        showsPrec d x
          = showString "VkSubpassSampleLocationsEXT {" .
              showString "vkSubpassIndex = " .
                showsPrec d (vkSubpassIndex x) .
                  showString ", " .
                    showString "vkSampleLocationsInfo = " .
                      showsPrec d (vkSampleLocationsInfo x) . showChar '}'
