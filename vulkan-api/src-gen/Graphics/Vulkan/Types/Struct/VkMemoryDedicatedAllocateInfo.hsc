#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryDedicatedAllocateInfo
       (VkMemoryDedicatedAllocateInfo(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Handles                     (VkBuffer,
                                                                    VkImage)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryDedicatedAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkImage          image;
--   >     VkBuffer         buffer;
--   > } VkMemoryDedicatedAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkMemoryDedicatedAllocateInfo.html VkMemoryDedicatedAllocateInfo registry at www.khronos.org>
data VkMemoryDedicatedAllocateInfo = VkMemoryDedicatedAllocateInfo## Addr##
                                                                    ByteArray##

instance Eq VkMemoryDedicatedAllocateInfo where
        (VkMemoryDedicatedAllocateInfo## a _) ==
          x@(VkMemoryDedicatedAllocateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryDedicatedAllocateInfo where
        (VkMemoryDedicatedAllocateInfo## a _) `compare`
          x@(VkMemoryDedicatedAllocateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryDedicatedAllocateInfo where
        sizeOf ~_ = #{size VkMemoryDedicatedAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryDedicatedAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryDedicatedAllocateInfo where
        unsafeAddr (VkMemoryDedicatedAllocateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryDedicatedAllocateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryDedicatedAllocateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryDedicatedAllocateInfo where
        type StructFields VkMemoryDedicatedAllocateInfo =
             '["sType", "pNext", "image", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryDedicatedAllocateInfo =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryDedicatedAllocateInfo where
        type FieldType "sType" VkMemoryDedicatedAllocateInfo =
             VkStructureType
        type FieldOptional "sType" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryDedicatedAllocateInfo =
             #{offset VkMemoryDedicatedAllocateInfo, sType}
        type FieldIsArray "sType" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryDedicatedAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryDedicatedAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryDedicatedAllocateInfo where
        type FieldType "pNext" VkMemoryDedicatedAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryDedicatedAllocateInfo =
             #{offset VkMemoryDedicatedAllocateInfo, pNext}
        type FieldIsArray "pNext" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryDedicatedAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryDedicatedAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "image" VkMemoryDedicatedAllocateInfo where
        type FieldType "image" VkMemoryDedicatedAllocateInfo = VkImage
        type FieldOptional "image" VkMemoryDedicatedAllocateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "image" VkMemoryDedicatedAllocateInfo =
             #{offset VkMemoryDedicatedAllocateInfo, image}
        type FieldIsArray "image" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfo, image}

instance {-# OVERLAPPING #-}
         CanReadField "image" VkMemoryDedicatedAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfo, image})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfo, image}

instance {-# OVERLAPPING #-}
         CanWriteField "image" VkMemoryDedicatedAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfo, image}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkMemoryDedicatedAllocateInfo where
        type FieldType "buffer" VkMemoryDedicatedAllocateInfo = VkBuffer
        type FieldOptional "buffer" VkMemoryDedicatedAllocateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkMemoryDedicatedAllocateInfo =
             #{offset VkMemoryDedicatedAllocateInfo, buffer}
        type FieldIsArray "buffer" VkMemoryDedicatedAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryDedicatedAllocateInfo, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkMemoryDedicatedAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryDedicatedAllocateInfo, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryDedicatedAllocateInfo, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkMemoryDedicatedAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryDedicatedAllocateInfo, buffer}

instance Show VkMemoryDedicatedAllocateInfo where
        showsPrec d x
          = showString "VkMemoryDedicatedAllocateInfo {" .
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
