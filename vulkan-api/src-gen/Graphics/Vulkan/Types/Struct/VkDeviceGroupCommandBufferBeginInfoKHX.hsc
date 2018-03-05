#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceGroupCommandBufferBeginInfoKHX
       (VkDeviceGroupCommandBufferBeginInfoKHX(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo (VkCommandBufferBeginInfo)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceGroupCommandBufferBeginInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   > } VkDeviceGroupCommandBufferBeginInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceGroupCommandBufferBeginInfoKHX.html VkDeviceGroupCommandBufferBeginInfoKHX registry at www.khronos.org>
data VkDeviceGroupCommandBufferBeginInfoKHX = VkDeviceGroupCommandBufferBeginInfoKHX## Addr##
                                                                                      ByteArray##

instance Eq VkDeviceGroupCommandBufferBeginInfoKHX where
        (VkDeviceGroupCommandBufferBeginInfoKHX## a _) ==
          x@(VkDeviceGroupCommandBufferBeginInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupCommandBufferBeginInfoKHX where
        (VkDeviceGroupCommandBufferBeginInfoKHX## a _) `compare`
          x@(VkDeviceGroupCommandBufferBeginInfoKHX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupCommandBufferBeginInfoKHX where
        sizeOf ~_
          = #{size VkDeviceGroupCommandBufferBeginInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupCommandBufferBeginInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupCommandBufferBeginInfoKHX
         where
        unsafeAddr (VkDeviceGroupCommandBufferBeginInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupCommandBufferBeginInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupCommandBufferBeginInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupCommandBufferBeginInfoKHX where
        type StructFields VkDeviceGroupCommandBufferBeginInfoKHX =
             '["sType", "pNext", "deviceMask"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupCommandBufferBeginInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupCommandBufferBeginInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupCommandBufferBeginInfoKHX =
             '[VkCommandBufferBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupCommandBufferBeginInfoKHX where
        type FieldType "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}
        type FieldIsArray "sType" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupCommandBufferBeginInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupCommandBufferBeginInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupCommandBufferBeginInfoKHX where
        type FieldType "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}
        type FieldIsArray "pNext" VkDeviceGroupCommandBufferBeginInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupCommandBufferBeginInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupCommandBufferBeginInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkDeviceGroupCommandBufferBeginInfoKHX where
        type FieldType "deviceMask" VkDeviceGroupCommandBufferBeginInfoKHX
             = Word32
        type FieldOptional "deviceMask"
               VkDeviceGroupCommandBufferBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask"
               VkDeviceGroupCommandBufferBeginInfoKHX
             =
             #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}
        type FieldIsArray "deviceMask"
               VkDeviceGroupCommandBufferBeginInfoKHX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkDeviceGroupCommandBufferBeginInfoKHX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfoKHX, deviceMask}

instance Show VkDeviceGroupCommandBufferBeginInfoKHX where
        showsPrec d x
          = showString "VkDeviceGroupCommandBufferBeginInfoKHX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceMask = " .
                            showsPrec d (getField @"deviceMask" x) . showChar '}'
