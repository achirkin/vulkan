#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImageViewUsageCreateInfoKHR
       (VkImageViewUsageCreateInfoKHR(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags       (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageViewCreateInfo (VkImageViewCreateInfo)
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkImageViewUsageCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkImageUsageFlags usage;
--   > } VkImageViewUsageCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImageViewUsageCreateInfoKHR.html VkImageViewUsageCreateInfoKHR registry at www.khronos.org>
data VkImageViewUsageCreateInfoKHR = VkImageViewUsageCreateInfoKHR## Addr##
                                                                    ByteArray##

instance Eq VkImageViewUsageCreateInfoKHR where
        (VkImageViewUsageCreateInfoKHR## a _) ==
          x@(VkImageViewUsageCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImageViewUsageCreateInfoKHR where
        (VkImageViewUsageCreateInfoKHR## a _) `compare`
          x@(VkImageViewUsageCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImageViewUsageCreateInfoKHR where
        sizeOf ~_ = #{size VkImageViewUsageCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImageViewUsageCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImageViewUsageCreateInfoKHR where
        unsafeAddr (VkImageViewUsageCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImageViewUsageCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImageViewUsageCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImageViewUsageCreateInfoKHR where
        type StructFields VkImageViewUsageCreateInfoKHR =
             '["sType", "pNext", "usage"] -- ' closing tick for hsc2hs
        type CUnionType VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImageViewUsageCreateInfoKHR =
             '[VkImageViewCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageViewUsageCreateInfoKHR where
        type FieldType "sType" VkImageViewUsageCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImageViewUsageCreateInfoKHR =
             #{offset VkImageViewUsageCreateInfoKHR, sType}
        type FieldIsArray "sType" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImageViewUsageCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewUsageCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImageViewUsageCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageViewUsageCreateInfoKHR where
        type FieldType "pNext" VkImageViewUsageCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImageViewUsageCreateInfoKHR =
             #{offset VkImageViewUsageCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImageViewUsageCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewUsageCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImageViewUsageCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "usage" VkImageViewUsageCreateInfoKHR where
        type FieldType "usage" VkImageViewUsageCreateInfoKHR =
             VkImageUsageFlags
        type FieldOptional "usage" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkImageViewUsageCreateInfoKHR =
             #{offset VkImageViewUsageCreateInfoKHR, usage}
        type FieldIsArray "usage" VkImageViewUsageCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImageViewUsageCreateInfoKHR, usage}

instance {-# OVERLAPPING #-}
         CanReadField "usage" VkImageViewUsageCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageViewUsageCreateInfoKHR, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImageViewUsageCreateInfoKHR, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkImageViewUsageCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImageViewUsageCreateInfoKHR, usage}

instance Show VkImageViewUsageCreateInfoKHR where
        showsPrec d x
          = showString "VkImageViewUsageCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "usage = " .
                            showsPrec d (getField @"usage" x) . showChar '}'
