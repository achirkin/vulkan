#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupBindSparseInfo
       (VkDeviceGroupBindSparseInfo(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkBindSparseInfo (VkBindSparseInfo)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupBindSparseInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         resourceDeviceIndex;
--   >     uint32_t                         memoryDeviceIndex;
--   > } VkDeviceGroupBindSparseInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkDeviceGroupBindSparseInfo VkDeviceGroupBindSparseInfo registry at www.khronos.org>
data VkDeviceGroupBindSparseInfo = VkDeviceGroupBindSparseInfo## Addr##
                                                                ByteArray##

instance Eq VkDeviceGroupBindSparseInfo where
        (VkDeviceGroupBindSparseInfo## a _) ==
          x@(VkDeviceGroupBindSparseInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupBindSparseInfo where
        (VkDeviceGroupBindSparseInfo## a _) `compare`
          x@(VkDeviceGroupBindSparseInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupBindSparseInfo where
        sizeOf ~_ = #{size VkDeviceGroupBindSparseInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupBindSparseInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupBindSparseInfo where
        unsafeAddr (VkDeviceGroupBindSparseInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupBindSparseInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupBindSparseInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupBindSparseInfo where
        type StructFields VkDeviceGroupBindSparseInfo =
             '["sType", "pNext", "resourceDeviceIndex", "memoryDeviceIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupBindSparseInfo =
             '[VkBindSparseInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupBindSparseInfo where
        type FieldType "sType" VkDeviceGroupBindSparseInfo =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupBindSparseInfo =
             #{offset VkDeviceGroupBindSparseInfo, sType}
        type FieldIsArray "sType" VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupBindSparseInfo where
        type FieldType "pNext" VkDeviceGroupBindSparseInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupBindSparseInfo =
             #{offset VkDeviceGroupBindSparseInfo, pNext}
        type FieldIsArray "pNext" VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "resourceDeviceIndex" VkDeviceGroupBindSparseInfo where
        type FieldType "resourceDeviceIndex" VkDeviceGroupBindSparseInfo =
             Word32
        type FieldOptional "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "resourceDeviceIndex" VkDeviceGroupBindSparseInfo
             =
             #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex}
        type FieldIsArray "resourceDeviceIndex" VkDeviceGroupBindSparseInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         CanReadField "resourceDeviceIndex" VkDeviceGroupBindSparseInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "resourceDeviceIndex" VkDeviceGroupBindSparseInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         HasField "memoryDeviceIndex" VkDeviceGroupBindSparseInfo where
        type FieldType "memoryDeviceIndex" VkDeviceGroupBindSparseInfo =
             Word32
        type FieldOptional "memoryDeviceIndex" VkDeviceGroupBindSparseInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryDeviceIndex" VkDeviceGroupBindSparseInfo =
             #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex}
        type FieldIsArray "memoryDeviceIndex" VkDeviceGroupBindSparseInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex}

instance {-# OVERLAPPING #-}
         CanReadField "memoryDeviceIndex" VkDeviceGroupBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryDeviceIndex" VkDeviceGroupBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex}

instance Show VkDeviceGroupBindSparseInfo where
        showsPrec d x
          = showString "VkDeviceGroupBindSparseInfo {" .
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
