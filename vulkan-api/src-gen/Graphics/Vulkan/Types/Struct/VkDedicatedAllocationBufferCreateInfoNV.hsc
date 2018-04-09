#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDedicatedAllocationBufferCreateInfoNV
       (VkDedicatedAllocationBufferCreateInfoNV(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Base                                        (Addr##,
                                                                  ByteArray##,
                                                                  byteArrayContents##,
                                                                  plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                 (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBufferCreateInfo (VkBufferCreateInfo)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkDedicatedAllocationBufferCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         dedicatedAllocation;
--   > } VkDedicatedAllocationBufferCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDedicatedAllocationBufferCreateInfoNVVkDedicatedAllocationBufferCreateInfoNV registry at www.khronos.org>
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
