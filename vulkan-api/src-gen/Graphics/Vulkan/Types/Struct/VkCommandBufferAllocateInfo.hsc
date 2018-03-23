#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCommandBufferAllocateInfo
       (VkCommandBufferAllocateInfo(..)) where
import           Foreign.Storable                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkCommandBufferLevel (VkCommandBufferLevel)
import           Graphics.Vulkan.Types.Enum.VkStructureType      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                   (VkCommandPool)
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

-- | > typedef struct VkCommandBufferAllocateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandPool          commandPool;
--   >     VkCommandBufferLevel   level;
--   >     uint32_t               commandBufferCount;
--   > } VkCommandBufferAllocateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkCommandBufferAllocateInfo.html VkCommandBufferAllocateInfo registry at www.khronos.org>
data VkCommandBufferAllocateInfo = VkCommandBufferAllocateInfo## Addr##
                                                                ByteArray##

instance Eq VkCommandBufferAllocateInfo where
        (VkCommandBufferAllocateInfo## a _) ==
          x@(VkCommandBufferAllocateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandBufferAllocateInfo where
        (VkCommandBufferAllocateInfo## a _) `compare`
          x@(VkCommandBufferAllocateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandBufferAllocateInfo where
        sizeOf ~_ = #{size VkCommandBufferAllocateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCommandBufferAllocateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandBufferAllocateInfo where
        unsafeAddr (VkCommandBufferAllocateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandBufferAllocateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandBufferAllocateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandBufferAllocateInfo where
        type StructFields VkCommandBufferAllocateInfo =
             '["sType", "pNext", "commandPool", "level", "commandBufferCount"] -- ' closing tick for hsc2hs
        type CUnionType VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandBufferAllocateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandBufferAllocateInfo where
        type FieldType "sType" VkCommandBufferAllocateInfo =
             VkStructureType
        type FieldOptional "sType" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, sType}
        type FieldIsArray "sType" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkCommandBufferAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandBufferAllocateInfo where
        type FieldType "pNext" VkCommandBufferAllocateInfo = Ptr Void
        type FieldOptional "pNext" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, pNext}
        type FieldIsArray "pNext" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkCommandBufferAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "commandPool" VkCommandBufferAllocateInfo where
        type FieldType "commandPool" VkCommandBufferAllocateInfo =
             VkCommandPool
        type FieldOptional "commandPool" VkCommandBufferAllocateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "commandPool" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, commandPool}
        type FieldIsArray "commandPool" VkCommandBufferAllocateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, commandPool}

instance {-# OVERLAPPING #-}
         CanReadField "commandPool" VkCommandBufferAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, commandPool})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, commandPool}

instance {-# OVERLAPPING #-}
         CanWriteField "commandPool" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, commandPool}

instance {-# OVERLAPPING #-}
         HasField "level" VkCommandBufferAllocateInfo where
        type FieldType "level" VkCommandBufferAllocateInfo =
             VkCommandBufferLevel
        type FieldOptional "level" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "level" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, level}
        type FieldIsArray "level" VkCommandBufferAllocateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, level}

instance {-# OVERLAPPING #-}
         CanReadField "level" VkCommandBufferAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, level})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, level}

instance {-# OVERLAPPING #-}
         CanWriteField "level" VkCommandBufferAllocateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, level}

instance {-# OVERLAPPING #-}
         HasField "commandBufferCount" VkCommandBufferAllocateInfo where
        type FieldType "commandBufferCount" VkCommandBufferAllocateInfo =
             Word32
        type FieldOptional "commandBufferCount" VkCommandBufferAllocateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "commandBufferCount" VkCommandBufferAllocateInfo =
             #{offset VkCommandBufferAllocateInfo, commandBufferCount}
        type FieldIsArray "commandBufferCount" VkCommandBufferAllocateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferAllocateInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         CanReadField "commandBufferCount" VkCommandBufferAllocateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferAllocateInfo, commandBufferCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkCommandBufferAllocateInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         CanWriteField "commandBufferCount" VkCommandBufferAllocateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkCommandBufferAllocateInfo, commandBufferCount}

instance Show VkCommandBufferAllocateInfo where
        showsPrec d x
          = showString "VkCommandBufferAllocateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "commandPool = " .
                            showsPrec d (getField @"commandPool" x) .
                              showString ", " .
                                showString "level = " .
                                  showsPrec d (getField @"level" x) .
                                    showString ", " .
                                      showString "commandBufferCount = " .
                                        showsPrec d (getField @"commandBufferCount" x) .
                                          showChar '}'
