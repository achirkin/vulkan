#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DedicatedAllocation
       (VkDedicatedAllocationBufferCreateInfoNV(..),
        VkDedicatedAllocationImageCreateInfoNV(..),
        VkDedicatedAllocationMemoryAllocateInfoNV(..))
       where
import           Foreign.Storable                         (Storable (..))
import           GHC.Base                                 (Addr##, ByteArray##,
                                                           byteArrayContents##,
                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes          (VkBool32)
import           Graphics.Vulkan.Types.Enum.StructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles            (VkBuffer, VkImage)
import           Graphics.Vulkan.Types.Struct.Buffer      (VkBufferCreateInfo)
import           Graphics.Vulkan.Types.Struct.Image       (VkImageCreateInfo)
import           Graphics.Vulkan.Types.Struct.Memory      (VkMemoryAllocateInfo)
import           System.IO.Unsafe                         (unsafeDupablePerformIO)

-- | > typedef struct VkDedicatedAllocationBufferCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         dedicatedAllocation;
--   > } VkDedicatedAllocationBufferCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDedicatedAllocationBufferCreateInfoNV VkDedicatedAllocationBufferCreateInfoNV registry at www.khronos.org>
data VkDedicatedAllocationBufferCreateInfoNV = VkDedicatedAllocationBufferCreateInfoNV## Addr##
                                                                                        ByteArray##

instance Eq VkDedicatedAllocationBufferCreateInfoNV where
        (VkDedicatedAllocationBufferCreateInfoNV## a _) ==
          x@(VkDedicatedAllocationBufferCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDedicatedAllocationBufferCreateInfoNV where
        (VkDedicatedAllocationBufferCreateInfoNV## a _) `compare`
          x@(VkDedicatedAllocationBufferCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDedicatedAllocationBufferCreateInfoNV where
        sizeOf ~_
          = #{size VkDedicatedAllocationBufferCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDedicatedAllocationBufferCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDedicatedAllocationBufferCreateInfoNV
         where
        unsafeAddr (VkDedicatedAllocationBufferCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDedicatedAllocationBufferCreateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDedicatedAllocationBufferCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDedicatedAllocationBufferCreateInfoNV
         where
        type StructFields VkDedicatedAllocationBufferCreateInfoNV =
             '["sType", "pNext", "dedicatedAllocation"] -- ' closing tick for hsc2hs
        type CUnionType VkDedicatedAllocationBufferCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDedicatedAllocationBufferCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDedicatedAllocationBufferCreateInfoNV =
             '[VkBufferCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDedicatedAllocationBufferCreateInfoNV where
        type FieldType "sType" VkDedicatedAllocationBufferCreateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDedicatedAllocationBufferCreateInfoNV =
             #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}
        type FieldIsArray "sType" VkDedicatedAllocationBufferCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDedicatedAllocationBufferCreateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationBufferCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDedicatedAllocationBufferCreateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDedicatedAllocationBufferCreateInfoNV where
        type FieldType "pNext" VkDedicatedAllocationBufferCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext" VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDedicatedAllocationBufferCreateInfoNV =
             #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}
        type FieldIsArray "pNext" VkDedicatedAllocationBufferCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDedicatedAllocationBufferCreateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDedicatedAllocationBufferCreateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "dedicatedAllocation"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        type FieldType "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             = VkBool32
        type FieldOptional "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             =
             #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}
        type FieldIsArray "dedicatedAllocation"
               VkDedicatedAllocationBufferCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "dedicatedAllocation"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "dedicatedAllocation"
           VkDedicatedAllocationBufferCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDedicatedAllocationBufferCreateInfoNV, dedicatedAllocation}

instance Show VkDedicatedAllocationBufferCreateInfoNV where
        showsPrec d x
          = showString "VkDedicatedAllocationBufferCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "dedicatedAllocation = " .
                            showsPrec d (getField @"dedicatedAllocation" x) . showChar '}'

-- | > typedef struct VkDedicatedAllocationImageCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         dedicatedAllocation;
--   > } VkDedicatedAllocationImageCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDedicatedAllocationImageCreateInfoNV VkDedicatedAllocationImageCreateInfoNV registry at www.khronos.org>
data VkDedicatedAllocationImageCreateInfoNV = VkDedicatedAllocationImageCreateInfoNV## Addr##
                                                                                      ByteArray##

instance Eq VkDedicatedAllocationImageCreateInfoNV where
        (VkDedicatedAllocationImageCreateInfoNV## a _) ==
          x@(VkDedicatedAllocationImageCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDedicatedAllocationImageCreateInfoNV where
        (VkDedicatedAllocationImageCreateInfoNV## a _) `compare`
          x@(VkDedicatedAllocationImageCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDedicatedAllocationImageCreateInfoNV where
        sizeOf ~_
          = #{size VkDedicatedAllocationImageCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDedicatedAllocationImageCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDedicatedAllocationImageCreateInfoNV
         where
        unsafeAddr (VkDedicatedAllocationImageCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDedicatedAllocationImageCreateInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDedicatedAllocationImageCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDedicatedAllocationImageCreateInfoNV where
        type StructFields VkDedicatedAllocationImageCreateInfoNV =
             '["sType", "pNext", "dedicatedAllocation"] -- ' closing tick for hsc2hs
        type CUnionType VkDedicatedAllocationImageCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDedicatedAllocationImageCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDedicatedAllocationImageCreateInfoNV =
             '[VkImageCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDedicatedAllocationImageCreateInfoNV where
        type FieldType "sType" VkDedicatedAllocationImageCreateInfoNV =
             VkStructureType
        type FieldOptional "sType" VkDedicatedAllocationImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDedicatedAllocationImageCreateInfoNV =
             #{offset VkDedicatedAllocationImageCreateInfoNV, sType}
        type FieldIsArray "sType" VkDedicatedAllocationImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDedicatedAllocationImageCreateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationImageCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDedicatedAllocationImageCreateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDedicatedAllocationImageCreateInfoNV where
        type FieldType "pNext" VkDedicatedAllocationImageCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext" VkDedicatedAllocationImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDedicatedAllocationImageCreateInfoNV =
             #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}
        type FieldIsArray "pNext" VkDedicatedAllocationImageCreateInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDedicatedAllocationImageCreateInfoNV where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationImageCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDedicatedAllocationImageCreateInfoNV where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "dedicatedAllocation"
           VkDedicatedAllocationImageCreateInfoNV
         where
        type FieldType "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             = VkBool32
        type FieldOptional "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             =
             #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}
        type FieldIsArray "dedicatedAllocation"
               VkDedicatedAllocationImageCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "dedicatedAllocation"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "dedicatedAllocation"
           VkDedicatedAllocationImageCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDedicatedAllocationImageCreateInfoNV, dedicatedAllocation}

instance Show VkDedicatedAllocationImageCreateInfoNV where
        showsPrec d x
          = showString "VkDedicatedAllocationImageCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "dedicatedAllocation = " .
                            showsPrec d (getField @"dedicatedAllocation" x) . showChar '}'

-- | > typedef struct VkDedicatedAllocationMemoryAllocateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage          image;
--   >     VkBuffer         buffer;
--   > } VkDedicatedAllocationMemoryAllocateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDedicatedAllocationMemoryAllocateInfoNV VkDedicatedAllocationMemoryAllocateInfoNV registry at www.khronos.org>
data VkDedicatedAllocationMemoryAllocateInfoNV = VkDedicatedAllocationMemoryAllocateInfoNV## Addr##
                                                                                            ByteArray##

instance Eq VkDedicatedAllocationMemoryAllocateInfoNV where
        (VkDedicatedAllocationMemoryAllocateInfoNV## a _) ==
          x@(VkDedicatedAllocationMemoryAllocateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDedicatedAllocationMemoryAllocateInfoNV where
        (VkDedicatedAllocationMemoryAllocateInfoNV## a _) `compare`
          x@(VkDedicatedAllocationMemoryAllocateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDedicatedAllocationMemoryAllocateInfoNV where
        sizeOf ~_
          = #{size VkDedicatedAllocationMemoryAllocateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDedicatedAllocationMemoryAllocateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkDedicatedAllocationMemoryAllocateInfoNV
         where
        unsafeAddr (VkDedicatedAllocationMemoryAllocateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDedicatedAllocationMemoryAllocateInfoNV## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDedicatedAllocationMemoryAllocateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDedicatedAllocationMemoryAllocateInfoNV
         where
        type StructFields VkDedicatedAllocationMemoryAllocateInfoNV =
             '["sType", "pNext", "image", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkDedicatedAllocationMemoryAllocateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDedicatedAllocationMemoryAllocateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkDedicatedAllocationMemoryAllocateInfoNV =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "sType" VkDedicatedAllocationMemoryAllocateInfoNV =
             VkStructureType
        type FieldOptional "sType"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}
        type FieldIsArray "sType" VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "pNext" VkDedicatedAllocationMemoryAllocateInfoNV =
             Ptr Void
        type FieldOptional "pNext"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}
        type FieldIsArray "pNext" VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "image" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "image" VkDedicatedAllocationMemoryAllocateInfoNV =
             VkImage
        type FieldOptional "image"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "image" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}
        type FieldIsArray "image" VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, image}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkDedicatedAllocationMemoryAllocateInfoNV where
        type FieldType "buffer" VkDedicatedAllocationMemoryAllocateInfoNV =
             VkBuffer
        type FieldOptional "buffer"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkDedicatedAllocationMemoryAllocateInfoNV
             =
             #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}
        type FieldIsArray "buffer"
               VkDedicatedAllocationMemoryAllocateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkDedicatedAllocationMemoryAllocateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDedicatedAllocationMemoryAllocateInfoNV, buffer}

instance Show VkDedicatedAllocationMemoryAllocateInfoNV where
        showsPrec d x
          = showString "VkDedicatedAllocationMemoryAllocateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "image = " .
                            showsPrec d (getField @"image" x) .
                              showString ", " .
                                showString "buffer = " .
                                  showsPrec d (getField @"buffer" x) . showChar '}'
