#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryAllocateFlagsInfoKHX
       (VkMemoryAllocateFlagsInfoKHX(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkMemoryAllocateFlagsKHX (VkMemoryAllocateFlagsKHX)
import           Graphics.Vulkan.Types.Enum.VkStructureType          (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo   (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryAllocateFlagsInfoKHX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkMemoryAllocateFlagsKHX flags;
--   >     uint32_t                         deviceMask;
--   > } VkMemoryAllocateFlagsInfoKHX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkMemoryAllocateFlagsInfoKHX.html VkMemoryAllocateFlagsInfoKHX registry at www.khronos.org>
data VkMemoryAllocateFlagsInfoKHX = VkMemoryAllocateFlagsInfoKHX## Addr##
                                                                  ByteArray##

instance Eq VkMemoryAllocateFlagsInfoKHX where
        (VkMemoryAllocateFlagsInfoKHX## a _) ==
          x@(VkMemoryAllocateFlagsInfoKHX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryAllocateFlagsInfoKHX where
        (VkMemoryAllocateFlagsInfoKHX## a _) `compare`
          x@(VkMemoryAllocateFlagsInfoKHX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryAllocateFlagsInfoKHX where
        sizeOf ~_ = #{size VkMemoryAllocateFlagsInfoKHX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryAllocateFlagsInfoKHX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMemoryAllocateFlagsInfoKHX where
        unsafeAddr (VkMemoryAllocateFlagsInfoKHX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryAllocateFlagsInfoKHX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryAllocateFlagsInfoKHX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryAllocateFlagsInfoKHX where
        type StructFields VkMemoryAllocateFlagsInfoKHX =
             '["sType", "pNext", "flags", "deviceMask"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryAllocateFlagsInfoKHX =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "sType" VkMemoryAllocateFlagsInfoKHX =
             VkStructureType
        type FieldOptional "sType" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, sType}
        type FieldIsArray "sType" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryAllocateFlagsInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "pNext" VkMemoryAllocateFlagsInfoKHX = Ptr Void
        type FieldOptional "pNext" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, pNext}
        type FieldIsArray "pNext" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryAllocateFlagsInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "flags" VkMemoryAllocateFlagsInfoKHX =
             VkMemoryAllocateFlagsKHX
        type FieldOptional "flags" VkMemoryAllocateFlagsInfoKHX = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, flags}
        type FieldIsArray "flags" VkMemoryAllocateFlagsInfoKHX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkMemoryAllocateFlagsInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, flags}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkMemoryAllocateFlagsInfoKHX where
        type FieldType "deviceMask" VkMemoryAllocateFlagsInfoKHX = Word32
        type FieldOptional "deviceMask" VkMemoryAllocateFlagsInfoKHX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkMemoryAllocateFlagsInfoKHX =
             #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}
        type FieldIsArray "deviceMask" VkMemoryAllocateFlagsInfoKHX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkMemoryAllocateFlagsInfoKHX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkMemoryAllocateFlagsInfoKHX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryAllocateFlagsInfoKHX, deviceMask}

instance Show VkMemoryAllocateFlagsInfoKHX where
        showsPrec d x
          = showString "VkMemoryAllocateFlagsInfoKHX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "deviceMask = " .
                                  showsPrec d (getField @"deviceMask" x) . showChar '}'
