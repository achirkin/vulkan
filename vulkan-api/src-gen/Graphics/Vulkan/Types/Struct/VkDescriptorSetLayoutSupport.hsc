#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutSupport
       (VkDescriptorSetLayoutSupport(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorSetLayoutSupport {
--   >     VkStructureType sType;
--   >     void*            pNext;
--   >     VkBool32         supported;
--   > } VkDescriptorSetLayoutSupport;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDescriptorSetLayoutSupport VkDescriptorSetLayoutSupport registry at www.khronos.org>
data VkDescriptorSetLayoutSupport = VkDescriptorSetLayoutSupport## Addr##
                                                                  ByteArray##

instance Eq VkDescriptorSetLayoutSupport where
        (VkDescriptorSetLayoutSupport## a _) ==
          x@(VkDescriptorSetLayoutSupport## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetLayoutSupport where
        (VkDescriptorSetLayoutSupport## a _) `compare`
          x@(VkDescriptorSetLayoutSupport## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetLayoutSupport where
        sizeOf ~_ = #{size VkDescriptorSetLayoutSupport}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetLayoutSupport}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorSetLayoutSupport where
        unsafeAddr (VkDescriptorSetLayoutSupport## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorSetLayoutSupport## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetLayoutSupport##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorSetLayoutSupport where
        type StructFields VkDescriptorSetLayoutSupport =
             '["sType", "pNext", "supported"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetLayoutSupport = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetLayoutSupport = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorSetLayoutSupport where
        type FieldType "sType" VkDescriptorSetLayoutSupport =
             VkStructureType
        type FieldOptional "sType" VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorSetLayoutSupport =
             #{offset VkDescriptorSetLayoutSupport, sType}
        type FieldIsArray "sType" VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutSupport, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorSetLayoutSupport where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutSupport, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutSupport, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorSetLayoutSupport where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutSupport, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorSetLayoutSupport where
        type FieldType "pNext" VkDescriptorSetLayoutSupport = Ptr Void
        type FieldOptional "pNext" VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorSetLayoutSupport =
             #{offset VkDescriptorSetLayoutSupport, pNext}
        type FieldIsArray "pNext" VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutSupport, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorSetLayoutSupport where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutSupport, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutSupport, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorSetLayoutSupport where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutSupport, pNext}

instance {-# OVERLAPPING #-}
         HasField "supported" VkDescriptorSetLayoutSupport where
        type FieldType "supported" VkDescriptorSetLayoutSupport = VkBool32
        type FieldOptional "supported" VkDescriptorSetLayoutSupport =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "supported" VkDescriptorSetLayoutSupport =
             #{offset VkDescriptorSetLayoutSupport, supported}
        type FieldIsArray "supported" VkDescriptorSetLayoutSupport = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutSupport, supported}

instance {-# OVERLAPPING #-}
         CanReadField "supported" VkDescriptorSetLayoutSupport where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutSupport, supported})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorSetLayoutSupport, supported}

instance {-# OVERLAPPING #-}
         CanWriteField "supported" VkDescriptorSetLayoutSupport where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorSetLayoutSupport, supported}

instance Show VkDescriptorSetLayoutSupport where
        showsPrec d x
          = showString "VkDescriptorSetLayoutSupport {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "supported = " .
                            showsPrec d (getField @"supported" x) . showChar '}'
