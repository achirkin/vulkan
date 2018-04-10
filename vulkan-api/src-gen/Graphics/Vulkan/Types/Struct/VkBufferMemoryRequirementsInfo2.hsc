#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferMemoryRequirementsInfo2
       (VkBufferMemoryRequirementsInfo2(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkBuffer)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBufferMemoryRequirementsInfo2 {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkBuffer                                                             buffer;
--   > } VkBufferMemoryRequirementsInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkBufferMemoryRequirementsInfo2 VkBufferMemoryRequirementsInfo2 registry at www.khronos.org>
data VkBufferMemoryRequirementsInfo2 = VkBufferMemoryRequirementsInfo2## Addr##
                                                                        ByteArray##

instance Eq VkBufferMemoryRequirementsInfo2 where
        (VkBufferMemoryRequirementsInfo2## a _) ==
          x@(VkBufferMemoryRequirementsInfo2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferMemoryRequirementsInfo2 where
        (VkBufferMemoryRequirementsInfo2## a _) `compare`
          x@(VkBufferMemoryRequirementsInfo2## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferMemoryRequirementsInfo2 where
        sizeOf ~_ = #{size VkBufferMemoryRequirementsInfo2}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBufferMemoryRequirementsInfo2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferMemoryRequirementsInfo2 where
        unsafeAddr (VkBufferMemoryRequirementsInfo2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferMemoryRequirementsInfo2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferMemoryRequirementsInfo2##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferMemoryRequirementsInfo2 where
        type StructFields VkBufferMemoryRequirementsInfo2 =
             '["sType", "pNext", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferMemoryRequirementsInfo2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBufferMemoryRequirementsInfo2 where
        type FieldType "sType" VkBufferMemoryRequirementsInfo2 =
             VkStructureType
        type FieldOptional "sType" VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferMemoryRequirementsInfo2 =
             #{offset VkBufferMemoryRequirementsInfo2, sType}
        type FieldIsArray "sType" VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBufferMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBufferMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBufferMemoryRequirementsInfo2 where
        type FieldType "pNext" VkBufferMemoryRequirementsInfo2 = Ptr Void
        type FieldOptional "pNext" VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferMemoryRequirementsInfo2 =
             #{offset VkBufferMemoryRequirementsInfo2, pNext}
        type FieldIsArray "pNext" VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBufferMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBufferMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2, pNext}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBufferMemoryRequirementsInfo2 where
        type FieldType "buffer" VkBufferMemoryRequirementsInfo2 = VkBuffer
        type FieldOptional "buffer" VkBufferMemoryRequirementsInfo2 =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBufferMemoryRequirementsInfo2 =
             #{offset VkBufferMemoryRequirementsInfo2, buffer}
        type FieldIsArray "buffer" VkBufferMemoryRequirementsInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkBufferMemoryRequirementsInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkBufferMemoryRequirementsInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2, buffer}

instance Show VkBufferMemoryRequirementsInfo2 where
        showsPrec d x
          = showString "VkBufferMemoryRequirementsInfo2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) . showChar '}'
