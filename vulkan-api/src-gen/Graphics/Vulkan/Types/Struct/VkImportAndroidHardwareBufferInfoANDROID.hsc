#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportAndroidHardwareBufferInfoANDROID
       (VkImportAndroidHardwareBufferInfoANDROID(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Base                                          (Addr##,
                                                                    ByteArray##,
                                                                    byteArrayContents##,
                                                                    plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Defines                     (AHardwareBuffer)
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo (VkMemoryAllocateInfo)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkImportAndroidHardwareBufferInfoANDROID {
--   >     VkStructureType sType;
--   >     const void*                        pNext;
--   >     struct AHardwareBuffer*            buffer;
--   > } VkImportAndroidHardwareBufferInfoANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkImportAndroidHardwareBufferInfoANDROID VkImportAndroidHardwareBufferInfoANDROID registry at www.khronos.org>
data VkImportAndroidHardwareBufferInfoANDROID = VkImportAndroidHardwareBufferInfoANDROID## Addr##
                                                                                          ByteArray##

instance Eq VkImportAndroidHardwareBufferInfoANDROID where
        (VkImportAndroidHardwareBufferInfoANDROID## a _) ==
          x@(VkImportAndroidHardwareBufferInfoANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportAndroidHardwareBufferInfoANDROID where
        (VkImportAndroidHardwareBufferInfoANDROID## a _) `compare`
          x@(VkImportAndroidHardwareBufferInfoANDROID## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportAndroidHardwareBufferInfoANDROID where
        sizeOf ~_
          = #{size VkImportAndroidHardwareBufferInfoANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportAndroidHardwareBufferInfoANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportAndroidHardwareBufferInfoANDROID
         where
        unsafeAddr (VkImportAndroidHardwareBufferInfoANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportAndroidHardwareBufferInfoANDROID## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportAndroidHardwareBufferInfoANDROID##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportAndroidHardwareBufferInfoANDROID
         where
        type StructFields VkImportAndroidHardwareBufferInfoANDROID =
             '["sType", "pNext", "buffer"] -- ' closing tick for hsc2hs
        type CUnionType VkImportAndroidHardwareBufferInfoANDROID = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportAndroidHardwareBufferInfoANDROID = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportAndroidHardwareBufferInfoANDROID =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportAndroidHardwareBufferInfoANDROID where
        type FieldType "sType" VkImportAndroidHardwareBufferInfoANDROID =
             VkStructureType
        type FieldOptional "sType" VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportAndroidHardwareBufferInfoANDROID =
             #{offset VkImportAndroidHardwareBufferInfoANDROID, sType}
        type FieldIsArray "sType" VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkImportAndroidHardwareBufferInfoANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportAndroidHardwareBufferInfoANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkImportAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportAndroidHardwareBufferInfoANDROID where
        type FieldType "pNext" VkImportAndroidHardwareBufferInfoANDROID =
             Ptr Void
        type FieldOptional "pNext" VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportAndroidHardwareBufferInfoANDROID =
             #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext}
        type FieldIsArray "pNext" VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkImportAndroidHardwareBufferInfoANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkImportAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkImportAndroidHardwareBufferInfoANDROID where
        type FieldType "buffer" VkImportAndroidHardwareBufferInfoANDROID =
             Ptr AHardwareBuffer
        type FieldOptional "buffer"
               VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkImportAndroidHardwareBufferInfoANDROID
             =
             #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer}
        type FieldIsArray "buffer" VkImportAndroidHardwareBufferInfoANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkImportAndroidHardwareBufferInfoANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkImportAndroidHardwareBufferInfoANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkImportAndroidHardwareBufferInfoANDROID, buffer}

instance Show VkImportAndroidHardwareBufferInfoANDROID where
        showsPrec d x
          = showString "VkImportAndroidHardwareBufferInfoANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "buffer = " .
                            showsPrec d (getField @"buffer" x) . showChar '}'
