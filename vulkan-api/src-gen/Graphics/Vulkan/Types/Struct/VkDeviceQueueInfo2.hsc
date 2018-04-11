#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceQueueInfo2
       (VkDeviceQueueInfo2(..)) where
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

-- | > typedef struct VkDeviceQueueInfo2 {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     VkDeviceQueueCreateFlags            flags;
--   >     uint32_t                            queueFamilyIndex;
--   >     uint32_t                            queueIndex;
--   > } VkDeviceQueueInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceQueueInfo2 VkDeviceQueueInfo2 registry at www.khronos.org>
data VkDeviceQueueInfo2 = VkDeviceQueueInfo2## Addr## ByteArray##

instance Eq VkDeviceQueueInfo2 where
        (VkDeviceQueueInfo2## a _) == x@(VkDeviceQueueInfo2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceQueueInfo2 where
        (VkDeviceQueueInfo2## a _) `compare` x@(VkDeviceQueueInfo2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceQueueInfo2 where
        sizeOf ~_ = #{size VkDeviceQueueInfo2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceQueueInfo2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceQueueInfo2 where
        unsafeAddr (VkDeviceQueueInfo2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceQueueInfo2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceQueueInfo2## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceQueueInfo2 where
        type StructFields VkDeviceQueueInfo2 =
             '["sType", "pNext", "flags", "queueFamilyIndex", "queueIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceQueueInfo2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkDeviceQueueInfo2
         where
        type FieldType "sType" VkDeviceQueueInfo2 = VkStructureType
        type FieldOptional "sType" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceQueueInfo2 =
             #{offset VkDeviceQueueInfo2, sType}
        type FieldIsArray "sType" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueInfo2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceQueueInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueInfo2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueInfo2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceQueueInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueInfo2, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkDeviceQueueInfo2
         where
        type FieldType "pNext" VkDeviceQueueInfo2 = Ptr Void
        type FieldOptional "pNext" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceQueueInfo2 =
             #{offset VkDeviceQueueInfo2, pNext}
        type FieldIsArray "pNext" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceQueueInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueInfo2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceQueueInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueInfo2, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkDeviceQueueInfo2
         where
        type FieldType "flags" VkDeviceQueueInfo2 =
             VkDeviceQueueCreateFlags
        type FieldOptional "flags" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDeviceQueueInfo2 =
             #{offset VkDeviceQueueInfo2, flags}
        type FieldIsArray "flags" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueInfo2, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDeviceQueueInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueInfo2, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueInfo2, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDeviceQueueInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueInfo2, flags}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndex" VkDeviceQueueInfo2 where
        type FieldType "queueFamilyIndex" VkDeviceQueueInfo2 = Word32
        type FieldOptional "queueFamilyIndex" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndex" VkDeviceQueueInfo2 =
             #{offset VkDeviceQueueInfo2, queueFamilyIndex}
        type FieldIsArray "queueFamilyIndex" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueInfo2, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyIndex" VkDeviceQueueInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueInfo2, queueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueInfo2, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyIndex" VkDeviceQueueInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueInfo2, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "queueIndex" VkDeviceQueueInfo2 where
        type FieldType "queueIndex" VkDeviceQueueInfo2 = Word32
        type FieldOptional "queueIndex" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueIndex" VkDeviceQueueInfo2 =
             #{offset VkDeviceQueueInfo2, queueIndex}
        type FieldIsArray "queueIndex" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueInfo2, queueIndex}

instance {-# OVERLAPPING #-}
         CanReadField "queueIndex" VkDeviceQueueInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueInfo2, queueIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueInfo2, queueIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "queueIndex" VkDeviceQueueInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueInfo2, queueIndex}

instance Show VkDeviceQueueInfo2 where
        showsPrec d x
          = showString "VkDeviceQueueInfo2 {" .
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
                                      showString "queueIndex = " .
                                        showsPrec d (getField @"queueIndex" x) . showChar '}'
