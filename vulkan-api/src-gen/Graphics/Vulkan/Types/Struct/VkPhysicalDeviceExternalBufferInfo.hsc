#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalBufferInfo
       (VkPhysicalDeviceExternalBufferInfo(..)) where
import           Foreign.Storable                                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkBufferCreateFlags             (VkBufferCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkBufferUsageFlags              (VkBufferUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlags (VkExternalMemoryHandleTypeFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType                 (VkStructureType)
import           System.IO.Unsafe                                           (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalBufferInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBufferCreateFlags flags;
--   >     VkBufferUsageFlags               usage;
--   >     VkExternalMemoryHandleTypeFlagBits handleType;
--   > } VkPhysicalDeviceExternalBufferInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceExternalBufferInfo.html VkPhysicalDeviceExternalBufferInfo registry at www.khronos.org>
data VkPhysicalDeviceExternalBufferInfo = VkPhysicalDeviceExternalBufferInfo## Addr##
                                                                              ByteArray##

instance Eq VkPhysicalDeviceExternalBufferInfo where
        (VkPhysicalDeviceExternalBufferInfo## a _) ==
          x@(VkPhysicalDeviceExternalBufferInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalBufferInfo where
        (VkPhysicalDeviceExternalBufferInfo## a _) `compare`
          x@(VkPhysicalDeviceExternalBufferInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalBufferInfo where
        sizeOf ~_ = #{size VkPhysicalDeviceExternalBufferInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalBufferInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceExternalBufferInfo where
        unsafeAddr (VkPhysicalDeviceExternalBufferInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceExternalBufferInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalBufferInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceExternalBufferInfo where
        type StructFields VkPhysicalDeviceExternalBufferInfo =
             '["sType", "pNext", "flags", "usage", "handleType"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalBufferInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalBufferInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalBufferInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalBufferInfo where
        type FieldType "sType" VkPhysicalDeviceExternalBufferInfo =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceExternalBufferInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceExternalBufferInfo =
             #{offset VkPhysicalDeviceExternalBufferInfo, sType}
        type FieldIsArray "sType" VkPhysicalDeviceExternalBufferInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceExternalBufferInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceExternalBufferInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalBufferInfo where
        type FieldType "pNext" VkPhysicalDeviceExternalBufferInfo =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceExternalBufferInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceExternalBufferInfo =
             #{offset VkPhysicalDeviceExternalBufferInfo, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceExternalBufferInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceExternalBufferInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceExternalBufferInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPhysicalDeviceExternalBufferInfo where
        type FieldType "flags" VkPhysicalDeviceExternalBufferInfo =
             VkBufferCreateFlags
        type FieldOptional "flags" VkPhysicalDeviceExternalBufferInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPhysicalDeviceExternalBufferInfo =
             #{offset VkPhysicalDeviceExternalBufferInfo, flags}
        type FieldIsArray "flags" VkPhysicalDeviceExternalBufferInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPhysicalDeviceExternalBufferInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPhysicalDeviceExternalBufferInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceExternalBufferInfo where
        type FieldType "usage" VkPhysicalDeviceExternalBufferInfo =
             VkBufferUsageFlags
        type FieldOptional "usage" VkPhysicalDeviceExternalBufferInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkPhysicalDeviceExternalBufferInfo =
             #{offset VkPhysicalDeviceExternalBufferInfo, usage}
        type FieldIsArray "usage" VkPhysicalDeviceExternalBufferInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfo, usage}

instance {-# OVERLAPPING #-}
         CanReadField "usage" VkPhysicalDeviceExternalBufferInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfo, usage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfo, usage}

instance {-# OVERLAPPING #-}
         CanWriteField "usage" VkPhysicalDeviceExternalBufferInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfo, usage}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkPhysicalDeviceExternalBufferInfo where
        type FieldType "handleType" VkPhysicalDeviceExternalBufferInfo =
             VkExternalMemoryHandleTypeFlagBits
        type FieldOptional "handleType" VkPhysicalDeviceExternalBufferInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkPhysicalDeviceExternalBufferInfo =
             #{offset VkPhysicalDeviceExternalBufferInfo, handleType}
        type FieldIsArray "handleType" VkPhysicalDeviceExternalBufferInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalBufferInfo, handleType}

instance {-# OVERLAPPING #-}
         CanReadField "handleType" VkPhysicalDeviceExternalBufferInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalBufferInfo, handleType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceExternalBufferInfo, handleType}

instance {-# OVERLAPPING #-}
         CanWriteField "handleType" VkPhysicalDeviceExternalBufferInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalBufferInfo, handleType}

instance Show VkPhysicalDeviceExternalBufferInfo where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalBufferInfo {" .
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
