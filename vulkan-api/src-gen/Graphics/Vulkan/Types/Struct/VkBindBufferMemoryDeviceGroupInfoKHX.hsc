#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkBindBufferMemoryDeviceGroupInfoKHX
       (VkBindBufferMemoryDeviceGroupInfoKHX(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBindBufferMemoryInfoKHR (VkBindBufferMemoryInfoKHR)
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkBindBufferMemoryDeviceGroupInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         deviceIndexCount;
--   >     const uint32_t*  pDeviceIndices;
--   > } VkBindBufferMemoryDeviceGroupInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkBindBufferMemoryDeviceGroupInfoKHX.html VkBindBufferMemoryDeviceGroupInfoKHX registry at www.khronos.org>
data VkBindBufferMemoryDeviceGroupInfoKHX = VkBindBufferMemoryDeviceGroupInfoKHX## Addr##
                                                                                  ByteArray##

instance Eq VkBindBufferMemoryDeviceGroupInfoKHX where
        (VkBindBufferMemoryDeviceGroupInfoKHX## a _) ==
          x@(VkBindBufferMemoryDeviceGroupInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkBindBufferMemoryDeviceGroupInfoKHX where
        (VkBindBufferMemoryDeviceGroupInfoKHX## a _) `compare`
          x@(VkBindBufferMemoryDeviceGroupInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkBindBufferMemoryDeviceGroupInfoKHX where
        sizeOf ~_
          = #{size VkBindBufferMemoryDeviceGroupInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkBindBufferMemoryDeviceGroupInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkBindBufferMemoryDeviceGroupInfoKHX
         where
        unsafeAddr (VkBindBufferMemoryDeviceGroupInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkBindBufferMemoryDeviceGroupInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkBindBufferMemoryDeviceGroupInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkBindBufferMemoryDeviceGroupInfoKHX where
        type StructFields VkBindBufferMemoryDeviceGroupInfoKHX =
             '["sType", "pNext", "deviceIndexCount", "pDeviceIndices"] -- ' closing tick for hsc2hs
        type CUnionType VkBindBufferMemoryDeviceGroupInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkBindBufferMemoryDeviceGroupInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkBindBufferMemoryDeviceGroupInfoKHX =
             '[VkBindBufferMemoryInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkBindBufferMemoryDeviceGroupInfoKHX where
        type FieldType "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}
        type FieldIsArray "sType" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkBindBufferMemoryDeviceGroupInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkBindBufferMemoryDeviceGroupInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkBindBufferMemoryDeviceGroupInfoKHX where
        type FieldType "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}
        type FieldIsArray "pNext" VkBindBufferMemoryDeviceGroupInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkBindBufferMemoryDeviceGroupInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkBindBufferMemoryDeviceGroupInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceIndexCount" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        type FieldType "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = Word32
        type FieldOptional "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}
        type FieldIsArray "deviceIndexCount"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanReadField "deviceIndexCount"
           VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceIndexCount"
           VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, deviceIndexCount}

instance {-# OVERLAPPING #-}
         HasField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        type FieldType "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = Ptr Word32
        type FieldOptional "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             =
             #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}
        type FieldIsArray "pDeviceIndices"
               VkBindBufferMemoryDeviceGroupInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceIndices" VkBindBufferMemoryDeviceGroupInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkBindBufferMemoryDeviceGroupInfoKHX, pDeviceIndices}

instance Show VkBindBufferMemoryDeviceGroupInfoKHX where
        showsPrec d x
          = showString "VkBindBufferMemoryDeviceGroupInfoKHX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceIndexCount = " .
                            showsPrec d (getField @"deviceIndexCount" x) .
                              showString ", " .
                                showString "pDeviceIndices = " .
                                  showsPrec d (getField @"pDeviceIndices" x) . showChar '}'
