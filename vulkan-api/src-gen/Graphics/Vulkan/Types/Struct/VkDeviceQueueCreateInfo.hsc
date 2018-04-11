#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo
       (VkDeviceQueueCreateInfo(..)) where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Base                                            (Addr##,
                                                                      ByteArray##,
                                                                      byteArrayContents##,
                                                                      plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDeviceQueueCreateFlags (VkDeviceQueueCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType          (VkStructureType)
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceQueueCreateInfo {
--   >     VkStructureType sType;
--   >     const void*     pNext;
--   >     VkDeviceQueueCreateFlags    flags;
--   >     uint32_t        queueFamilyIndex;
--   >     uint32_t        queueCount;
--   >     const float*    pQueuePriorities;
--   > } VkDeviceQueueCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceQueueCreateInfo VkDeviceQueueCreateInfo registry at www.khronos.org>
data VkDeviceQueueCreateInfo = VkDeviceQueueCreateInfo## Addr##
                                                        ByteArray##

instance Eq VkDeviceQueueCreateInfo where
        (VkDeviceQueueCreateInfo## a _) == x@(VkDeviceQueueCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceQueueCreateInfo where
        (VkDeviceQueueCreateInfo## a _) `compare`
          x@(VkDeviceQueueCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceQueueCreateInfo where
        sizeOf ~_ = #{size VkDeviceQueueCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceQueueCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceQueueCreateInfo where
        unsafeAddr (VkDeviceQueueCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceQueueCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceQueueCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceQueueCreateInfo where
        type StructFields VkDeviceQueueCreateInfo =
             '["sType", "pNext", "flags", "queueFamilyIndex", "queueCount", -- ' closing tick for hsc2hs
               "pQueuePriorities"]
        type CUnionType VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceQueueCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceQueueCreateInfo where
        type FieldType "sType" VkDeviceQueueCreateInfo = VkStructureType
        type FieldOptional "sType" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, sType}
        type FieldIsArray "sType" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceQueueCreateInfo where
        type FieldType "pNext" VkDeviceQueueCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, pNext}
        type FieldIsArray "pNext" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDeviceQueueCreateInfo where
        type FieldType "flags" VkDeviceQueueCreateInfo =
             VkDeviceQueueCreateFlags
        type FieldOptional "flags" VkDeviceQueueCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, flags}
        type FieldIsArray "flags" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndex" VkDeviceQueueCreateInfo where
        type FieldType "queueFamilyIndex" VkDeviceQueueCreateInfo = Word32
        type FieldOptional "queueFamilyIndex" VkDeviceQueueCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndex" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}
        type FieldIsArray "queueFamilyIndex" VkDeviceQueueCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyIndex" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, queueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyIndex" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "queueCount" VkDeviceQueueCreateInfo where
        type FieldType "queueCount" VkDeviceQueueCreateInfo = Word32
        type FieldOptional "queueCount" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueCount" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, queueCount}
        type FieldIsArray "queueCount" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueCreateInfo, queueCount}

instance {-# OVERLAPPING #-}
         CanReadField "queueCount" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, queueCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, queueCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queueCount" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, queueCount}

instance {-# OVERLAPPING #-}
         HasField "pQueuePriorities" VkDeviceQueueCreateInfo where
        type FieldType "pQueuePriorities" VkDeviceQueueCreateInfo =
             Ptr #{type float}
        type FieldOptional "pQueuePriorities" VkDeviceQueueCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pQueuePriorities" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, pQueuePriorities}
        type FieldIsArray "pQueuePriorities" VkDeviceQueueCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueCreateInfo, pQueuePriorities}

instance {-# OVERLAPPING #-}
         CanReadField "pQueuePriorities" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, pQueuePriorities})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, pQueuePriorities}

instance {-# OVERLAPPING #-}
         CanWriteField "pQueuePriorities" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, pQueuePriorities}

instance Show VkDeviceQueueCreateInfo where
        showsPrec d x
          = showString "VkDeviceQueueCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "queueFamilyIndex = " .
                                  showsPrec d (getField @"queueFamilyIndex" x) .
                                    showString ", " .
                                      showString "queueCount = " .
                                        showsPrec d (getField @"queueCount" x) .
                                          showString ", " .
                                            showString "pQueuePriorities = " .
                                              showsPrec d (getField @"pQueuePriorities" x) .
                                                showChar '}'
