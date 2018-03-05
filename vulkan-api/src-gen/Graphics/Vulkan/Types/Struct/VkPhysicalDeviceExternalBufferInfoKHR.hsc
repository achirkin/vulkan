#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfoKHR
       (VkPhysicalDeviceExternalBufferInfoKHR(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkBufferCreateFlags                (VkBufferCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkBufferUsageFlags                 (VkBufferUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalBufferInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBufferCreateFlags flags;
--   >     VkBufferUsageFlags               usage;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   > } VkPhysicalDeviceExternalBufferInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceExternalBufferInfoKHR.html VkPhysicalDeviceExternalBufferInfoKHR registry at www.khronos.org>
data VkPhysicalDeviceExternalBufferInfoKHR = VkPhysicalDeviceExternalBufferInfoKHR## Addr##
                                                                                    ByteArray##

instance Eq VkPhysicalDeviceExternalBufferInfoKHR where
        (VkPhysicalDeviceExternalBufferInfoKHR## a _) ==
          x@(VkPhysicalDeviceExternalBufferInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalBufferInfoKHR where
        (VkPhysicalDeviceExternalBufferInfoKHR## a _) `compare`
          x@(VkPhysicalDeviceExternalBufferInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalBufferInfoKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalBufferInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalBufferInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceExternalBufferInfoKHR
         where
        unsafeAddr (VkPhysicalDeviceExternalBufferInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalBufferInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalBufferInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalBufferInfoKHR where
        type StructFields VkPhysicalDeviceExternalBufferInfoKHR =
             '["sType", "pNext", "flags", "usage", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalBufferInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalBufferInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalBufferInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "sType" VkPhysicalDeviceExternalBufferInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalBufferInfoKHR =
             #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceExternalBufferInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceExternalBufferInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "pNext" VkPhysicalDeviceExternalBufferInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalBufferInfoKHR =
             #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceExternalBufferInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceExternalBufferInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "flags" VkPhysicalDeviceExternalBufferInfoKHR =
             VkBufferCreateFlags
        type FieldOptional "flags" VkPhysicalDeviceExternalBufferInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPhysicalDeviceExternalBufferInfoKHR =
             #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}
        type FieldIsArray "flags" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPhysicalDeviceExternalBufferInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPhysicalDeviceExternalBufferInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "usage" VkPhysicalDeviceExternalBufferInfoKHR =
             VkBufferUsageFlags
        type FieldOptional "usage" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkPhysicalDeviceExternalBufferInfoKHR =
             #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}
        type FieldIsArray "usage" VkPhysicalDeviceExternalBufferInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}

instance {-# OVERLAPPING #-}
         CanReadField "usage" VkPhysicalDeviceExternalBufferInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkPhysicalDeviceExternalBufferInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, usage}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalBufferInfoKHR where
        type FieldType "handleType" VkPhysicalDeviceExternalBufferInfoKHR =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType"
               VkPhysicalDeviceExternalBufferInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkPhysicalDeviceExternalBufferInfoKHR
             =
             #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}
        type FieldIsArray "handleType"
               VkPhysicalDeviceExternalBufferInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkPhysicalDeviceExternalBufferInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfoKHR, handleType}

instance Show VkPhysicalDeviceExternalBufferInfoKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalBufferInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "usage = " .
                                  showsPrec d (getField @"usage" x) .
                                    showString ", " .
                                      showString "handleType = " .
                                        showsPrec d (getField @"handleType" x) . showChar '}'
