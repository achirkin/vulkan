#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAndroidHardwareBufferPropertiesANDROID
       (VkAndroidHardwareBufferPropertiesANDROID(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Base                                   (Addr##, ByteArray##,
                                                             byteArrayContents##,
                                                             plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes            (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkAndroidHardwareBufferPropertiesANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     VkDeviceSize                       allocationSize;
--   >     uint32_t                           memoryTypeBits;
--   > } VkAndroidHardwareBufferPropertiesANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkAndroidHardwareBufferPropertiesANDROID VkAndroidHardwareBufferPropertiesANDROID registry at www.khronos.org>
data VkAndroidHardwareBufferPropertiesANDROID = VkAndroidHardwareBufferPropertiesANDROID## Addr##
                                                                                          ByteArray##

instance Eq VkAndroidHardwareBufferPropertiesANDROID where
        (VkAndroidHardwareBufferPropertiesANDROID## a _) ==
          x@(VkAndroidHardwareBufferPropertiesANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAndroidHardwareBufferPropertiesANDROID where
        (VkAndroidHardwareBufferPropertiesANDROID## a _) `compare`
          x@(VkAndroidHardwareBufferPropertiesANDROID## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAndroidHardwareBufferPropertiesANDROID where
        sizeOf ~_
          = #{size VkAndroidHardwareBufferPropertiesANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAndroidHardwareBufferPropertiesANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAndroidHardwareBufferPropertiesANDROID
         where
        unsafeAddr (VkAndroidHardwareBufferPropertiesANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAndroidHardwareBufferPropertiesANDROID## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAndroidHardwareBufferPropertiesANDROID##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAndroidHardwareBufferPropertiesANDROID
         where
        type StructFields VkAndroidHardwareBufferPropertiesANDROID =
             '["sType", "pNext", "allocationSize", "memoryTypeBits"] -- ' closing tick for hsc2hs
        type CUnionType VkAndroidHardwareBufferPropertiesANDROID = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAndroidHardwareBufferPropertiesANDROID = 'True -- ' closing tick for hsc2hs
        type StructExtends VkAndroidHardwareBufferPropertiesANDROID = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkAndroidHardwareBufferPropertiesANDROID where
        type FieldType "sType" VkAndroidHardwareBufferPropertiesANDROID =
             VkStructureType
        type FieldOptional "sType" VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkAndroidHardwareBufferPropertiesANDROID =
             #{offset VkAndroidHardwareBufferPropertiesANDROID, sType}
        type FieldIsArray "sType" VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferPropertiesANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkAndroidHardwareBufferPropertiesANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferPropertiesANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkAndroidHardwareBufferPropertiesANDROID where
        type FieldType "pNext" VkAndroidHardwareBufferPropertiesANDROID =
             Ptr Void
        type FieldOptional "pNext" VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkAndroidHardwareBufferPropertiesANDROID =
             #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext}
        type FieldIsArray "pNext" VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkAndroidHardwareBufferPropertiesANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "allocationSize" VkAndroidHardwareBufferPropertiesANDROID
         where
        type FieldType "allocationSize"
               VkAndroidHardwareBufferPropertiesANDROID
             = VkDeviceSize
        type FieldOptional "allocationSize"
               VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "allocationSize"
               VkAndroidHardwareBufferPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize}
        type FieldIsArray "allocationSize"
               VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize}

instance {-# OVERLAPPING #-}
         CanReadField "allocationSize"
           VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize}

instance {-# OVERLAPPING #-}
         CanWriteField "allocationSize"
           VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, allocationSize}

instance {-# OVERLAPPING #-}
         HasField "memoryTypeBits" VkAndroidHardwareBufferPropertiesANDROID
         where
        type FieldType "memoryTypeBits"
               VkAndroidHardwareBufferPropertiesANDROID
             = Word32
        type FieldOptional "memoryTypeBits"
               VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryTypeBits"
               VkAndroidHardwareBufferPropertiesANDROID
             =
             #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits}
        type FieldIsArray "memoryTypeBits"
               VkAndroidHardwareBufferPropertiesANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanReadField "memoryTypeBits"
           VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryTypeBits"
           VkAndroidHardwareBufferPropertiesANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferPropertiesANDROID, memoryTypeBits}

instance Show VkAndroidHardwareBufferPropertiesANDROID where
        showsPrec d x
          = showString "VkAndroidHardwareBufferPropertiesANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "allocationSize = " .
                            showsPrec d (getField @"allocationSize" x) .
                              showString ", " .
                                showString "memoryTypeBits = " .
                                  showsPrec d (getField @"memoryTypeBits" x) . showChar '}'
