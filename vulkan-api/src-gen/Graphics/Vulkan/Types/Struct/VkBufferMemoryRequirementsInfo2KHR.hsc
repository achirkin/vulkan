#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBufferMemoryRequirementsInfo2KHR
       (VkBufferMemoryRequirementsInfo2KHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkBuffer)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkBufferMemoryRequirementsInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                                                          pNext;
--   >     VkBuffer                                                             buffer;
--   > } VkBufferMemoryRequirementsInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBufferMemoryRequirementsInfo2KHR.html VkBufferMemoryRequirementsInfo2KHR registry at www.khronos.org>
data VkBufferMemoryRequirementsInfo2KHR = VkBufferMemoryRequirementsInfo2KHR## Addr##
                                                                              ByteArray##

instance Eq VkBufferMemoryRequirementsInfo2KHR where
        (VkBufferMemoryRequirementsInfo2KHR## a _) ==
          x@(VkBufferMemoryRequirementsInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBufferMemoryRequirementsInfo2KHR where
        (VkBufferMemoryRequirementsInfo2KHR## a _) `compare`
          x@(VkBufferMemoryRequirementsInfo2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBufferMemoryRequirementsInfo2KHR where
        sizeOf ~_ = #{size VkBufferMemoryRequirementsInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBufferMemoryRequirementsInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBufferMemoryRequirementsInfo2KHR where
        unsafeAddr (VkBufferMemoryRequirementsInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBufferMemoryRequirementsInfo2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBufferMemoryRequirementsInfo2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBufferMemoryRequirementsInfo2KHR where
        type StructFields VkBufferMemoryRequirementsInfo2KHR =
             '["sType", "pNext", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkBufferMemoryRequirementsInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBufferMemoryRequirementsInfo2KHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBufferMemoryRequirementsInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBufferMemoryRequirementsInfo2KHR where
        type FieldType "sType" VkBufferMemoryRequirementsInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBufferMemoryRequirementsInfo2KHR =
             #{offset VkBufferMemoryRequirementsInfo2KHR, sType}
        type FieldIsArray "sType" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBufferMemoryRequirementsInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2KHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBufferMemoryRequirementsInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBufferMemoryRequirementsInfo2KHR where
        type FieldType "pNext" VkBufferMemoryRequirementsInfo2KHR =
             Ptr Void
        type FieldOptional "pNext" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBufferMemoryRequirementsInfo2KHR =
             #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}
        type FieldIsArray "pNext" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBufferMemoryRequirementsInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2KHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBufferMemoryRequirementsInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkBufferMemoryRequirementsInfo2KHR where
        type FieldType "buffer" VkBufferMemoryRequirementsInfo2KHR =
             VkBuffer
        type FieldOptional "buffer" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkBufferMemoryRequirementsInfo2KHR =
             #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}
        type FieldIsArray "buffer" VkBufferMemoryRequirementsInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkBufferMemoryRequirementsInfo2KHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBufferMemoryRequirementsInfo2KHR, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkBufferMemoryRequirementsInfo2KHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBufferMemoryRequirementsInfo2KHR, buffer}

instance Show VkBufferMemoryRequirementsInfo2KHR where
        showsPrec d x
          = showString "VkBufferMemoryRequirementsInfo2KHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) . showChar '}'
