#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryWin32HandlePropertiesKHR
       (VkMemoryWin32HandlePropertiesKHR(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryWin32HandlePropertiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         memoryTypeBits;
--   > } VkMemoryWin32HandlePropertiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkMemoryWin32HandlePropertiesKHR VkMemoryWin32HandlePropertiesKHR registry at www.khronos.org>
data VkMemoryWin32HandlePropertiesKHR = VkMemoryWin32HandlePropertiesKHR## Addr##
                                                                          ByteArray##

instance Eq VkMemoryWin32HandlePropertiesKHR where
        (VkMemoryWin32HandlePropertiesKHR## a _) ==
          x@(VkMemoryWin32HandlePropertiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryWin32HandlePropertiesKHR where
        (VkMemoryWin32HandlePropertiesKHR## a _) `compare`
          x@(VkMemoryWin32HandlePropertiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryWin32HandlePropertiesKHR where
        sizeOf ~_ = #{size VkMemoryWin32HandlePropertiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryWin32HandlePropertiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryWin32HandlePropertiesKHR where
        unsafeAddr (VkMemoryWin32HandlePropertiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryWin32HandlePropertiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryWin32HandlePropertiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryWin32HandlePropertiesKHR where
        type StructFields VkMemoryWin32HandlePropertiesKHR =
             '["sType", "pNext", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryWin32HandlePropertiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryWin32HandlePropertiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkMemoryWin32HandlePropertiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryWin32HandlePropertiesKHR where
        type FieldType "sType" VkMemoryWin32HandlePropertiesKHR =
             VkStructureType
        type FieldOptional "sType" VkMemoryWin32HandlePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryWin32HandlePropertiesKHR =
             #{offset VkMemoryWin32HandlePropertiesKHR, sType}
        type FieldIsArray "sType" VkMemoryWin32HandlePropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryWin32HandlePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryWin32HandlePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryWin32HandlePropertiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryWin32HandlePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryWin32HandlePropertiesKHR where
        type FieldType "pNext" VkMemoryWin32HandlePropertiesKHR = Ptr Void
        type FieldOptional "pNext" VkMemoryWin32HandlePropertiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryWin32HandlePropertiesKHR =
             #{offset VkMemoryWin32HandlePropertiesKHR, pNext}
        type FieldIsArray "pNext" VkMemoryWin32HandlePropertiesKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryWin32HandlePropertiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryWin32HandlePropertiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryWin32HandlePropertiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR where
        type FieldType "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR =
             Word32
        type FieldOptional "memoryTypeBits"
               VkMemoryWin32HandlePropertiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR
             =
             #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}
        type FieldIsArray "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeBits" VkMemoryWin32HandlePropertiesKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryWin32HandlePropertiesKHR, memoryTypeBits}

instance Show VkMemoryWin32HandlePropertiesKHR where
        showsPrec d x
          = showString "VkMemoryWin32HandlePropertiesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memoryTypeBits = " .
                            showsPrec d (getField @"memoryTypeBits" x) . showChar '}'
