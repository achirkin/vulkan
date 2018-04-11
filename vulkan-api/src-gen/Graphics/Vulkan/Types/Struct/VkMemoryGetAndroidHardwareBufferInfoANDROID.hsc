#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkMemoryGetAndroidHardwareBufferInfoANDROID
       (VkMemoryGetAndroidHardwareBufferInfoANDROID(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.Handles              (VkDeviceMemory)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkMemoryGetAndroidHardwareBufferInfoANDROID {
--   >     VkStructureType sType;
--   >     const void*                        pNext;
--   >     VkDeviceMemory                     memory;
--   > } VkMemoryGetAndroidHardwareBufferInfoANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkMemoryGetAndroidHardwareBufferInfoANDROID VkMemoryGetAndroidHardwareBufferInfoANDROID registry at www.khronos.org>
data VkMemoryGetAndroidHardwareBufferInfoANDROID = VkMemoryGetAndroidHardwareBufferInfoANDROID## Addr##
                                                                                                ByteArray##

instance Eq VkMemoryGetAndroidHardwareBufferInfoANDROID where
        (VkMemoryGetAndroidHardwareBufferInfoANDROID## a _) ==
          x@(VkMemoryGetAndroidHardwareBufferInfoANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMemoryGetAndroidHardwareBufferInfoANDROID where
        (VkMemoryGetAndroidHardwareBufferInfoANDROID## a _) `compare`
          x@(VkMemoryGetAndroidHardwareBufferInfoANDROID## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMemoryGetAndroidHardwareBufferInfoANDROID where
        sizeOf ~_
          = #{size VkMemoryGetAndroidHardwareBufferInfoANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkMemoryGetAndroidHardwareBufferInfoANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        unsafeAddr (VkMemoryGetAndroidHardwareBufferInfoANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMemoryGetAndroidHardwareBufferInfoANDROID## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMemoryGetAndroidHardwareBufferInfoANDROID##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        type StructFields VkMemoryGetAndroidHardwareBufferInfoANDROID =
             '["sType", "pNext", "memory"] -- ' closing tick for hsc2hs
        type CUnionType VkMemoryGetAndroidHardwareBufferInfoANDROID =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMemoryGetAndroidHardwareBufferInfoANDROID =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkMemoryGetAndroidHardwareBufferInfoANDROID =
             '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkMemoryGetAndroidHardwareBufferInfoANDROID where
        type FieldType "sType" VkMemoryGetAndroidHardwareBufferInfoANDROID
             = VkStructureType
        type FieldOptional "sType"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             =
             #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType}
        type FieldIsArray "sType"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMemoryGetAndroidHardwareBufferInfoANDROID where
        type FieldType "pNext" VkMemoryGetAndroidHardwareBufferInfoANDROID
             = Ptr Void
        type FieldOptional "pNext"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             =
             #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext}
        type FieldIsArray "pNext"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "memory" VkMemoryGetAndroidHardwareBufferInfoANDROID where
        type FieldType "memory" VkMemoryGetAndroidHardwareBufferInfoANDROID
             = VkDeviceMemory
        type FieldOptional "memory"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memory"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             =
             #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory}
        type FieldIsArray "memory"
               VkMemoryGetAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory}

instance {-# OVERLAPPING #-}
         CanReadField "memory" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory}

instance {-# OVERLAPPING #-}
         CanWriteField "memory" VkMemoryGetAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkMemoryGetAndroidHardwareBufferInfoANDROID, memory}

instance Show VkMemoryGetAndroidHardwareBufferInfoANDROID where
        showsPrec d x
          = showString "VkMemoryGetAndroidHardwareBufferInfoANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "memory = " .
                            showsPrec d (getField @"memory" x) . showChar '}'
