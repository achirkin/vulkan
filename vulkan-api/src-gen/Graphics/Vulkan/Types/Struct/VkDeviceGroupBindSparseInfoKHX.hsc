#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupBindSparseInfoKHX
       (VkDeviceGroupBindSparseInfoKHX(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBindSparseInfo (VkBindSparseInfo)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupBindSparseInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         resourceDeviceIndex;
--   >     uint32_t                         memoryDeviceIndex;
--   > } VkDeviceGroupBindSparseInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupBindSparseInfoKHX.html VkDeviceGroupBindSparseInfoKHX registry at www.khronos.org>
data VkDeviceGroupBindSparseInfoKHX = VkDeviceGroupBindSparseInfoKHX## Addr##
                                                                      ByteArray##

instance Eq VkDeviceGroupBindSparseInfoKHX where
        (VkDeviceGroupBindSparseInfoKHX## a _) ==
          x@(VkDeviceGroupBindSparseInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupBindSparseInfoKHX where
        (VkDeviceGroupBindSparseInfoKHX## a _) `compare`
          x@(VkDeviceGroupBindSparseInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupBindSparseInfoKHX where
        sizeOf ~_ = #{size VkDeviceGroupBindSparseInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupBindSparseInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupBindSparseInfoKHX where
        unsafeAddr (VkDeviceGroupBindSparseInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupBindSparseInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupBindSparseInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupBindSparseInfoKHX where
        type StructFields VkDeviceGroupBindSparseInfoKHX =
             '["sType", "pNext", "resourceDeviceIndex", "memoryDeviceIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupBindSparseInfoKHX =
             '[VkBindSparseInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "sType" VkDeviceGroupBindSparseInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupBindSparseInfoKHX =
             #{offset VkDeviceGroupBindSparseInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupBindSparseInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "pNext" VkDeviceGroupBindSparseInfoKHX = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupBindSparseInfoKHX =
             #{offset VkDeviceGroupBindSparseInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupBindSparseInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupBindSparseInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupBindSparseInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "resourceDeviceIndex" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "resourceDeviceIndex" VkDeviceGroupBindSparseInfoKHX
             = Word32
        type FieldOptional "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             =
             #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}
        type FieldIsArray "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         CanReadField "resourceDeviceIndex" VkDeviceGroupBindSparseInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "resourceDeviceIndex" VkDeviceGroupBindSparseInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         HasField "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX where
        type FieldType "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX =
             Word32
        type FieldOptional "memoryDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX
             =
             #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}
        type FieldIsArray "memoryDeviceIndex"
               VkDeviceGroupBindSparseInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

instance {-# OVERLAPPING #-}
         CanReadField "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryDeviceIndex" VkDeviceGroupBindSparseInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfoKHX, memoryDeviceIndex}

instance Show VkDeviceGroupBindSparseInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupBindSparseInfoKHX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "resourceDeviceIndex = " .
                            showsPrec d (getField @"resourceDeviceIndex" x) .
                              showString ", " .
                                showString "memoryDeviceIndex = " .
                                  showsPrec d (getField @"memoryDeviceIndex" x) . showChar '}'
