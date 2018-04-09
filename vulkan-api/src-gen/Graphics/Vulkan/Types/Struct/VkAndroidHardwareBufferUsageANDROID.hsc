#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkAndroidHardwareBufferUsageANDROID
       (VkAndroidHardwareBufferUsageANDROID(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Base                                              (Addr##, ByteArray##,
                                                                        byteArrayContents##,
                                                                        plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkImageFormatProperties2 (VkImageFormatProperties2)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

-- | > typedef struct VkAndroidHardwareBufferUsageANDROID {
--   >     VkStructureType sType;
--   >     void*                              pNext;
--   >     uint64_t                           androidHardwareBufferUsage;
--   > } VkAndroidHardwareBufferUsageANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkAndroidHardwareBufferUsageANDROIDVkAndroidHardwareBufferUsageANDROID registry at www.khronos.org>
data VkAndroidHardwareBufferUsageANDROID = VkAndroidHardwareBufferUsageANDROID## Addr##
                                                                                ByteArray##

instance Eq VkAndroidHardwareBufferUsageANDROID where
        (VkAndroidHardwareBufferUsageANDROID## a _) ==
          x@(VkAndroidHardwareBufferUsageANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkAndroidHardwareBufferUsageANDROID where
        (VkAndroidHardwareBufferUsageANDROID## a _) `compare`
          x@(VkAndroidHardwareBufferUsageANDROID## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkAndroidHardwareBufferUsageANDROID where
        sizeOf ~_ = #{size VkAndroidHardwareBufferUsageANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkAndroidHardwareBufferUsageANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkAndroidHardwareBufferUsageANDROID
         where
        unsafeAddr (VkAndroidHardwareBufferUsageANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkAndroidHardwareBufferUsageANDROID## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkAndroidHardwareBufferUsageANDROID##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkAndroidHardwareBufferUsageANDROID where
        type StructFields VkAndroidHardwareBufferUsageANDROID =
             '["sType", "pNext", "androidHardwareBufferUsage"] -- ' closing tick for hsc2hs
        type CUnionType VkAndroidHardwareBufferUsageANDROID = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkAndroidHardwareBufferUsageANDROID = 'True -- ' closing tick for hsc2hs
        type StructExtends VkAndroidHardwareBufferUsageANDROID =
             '[VkImageFormatProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkAndroidHardwareBufferUsageANDROID where
        type FieldType "sType" VkAndroidHardwareBufferUsageANDROID =
             VkStructureType
        type FieldOptional "sType" VkAndroidHardwareBufferUsageANDROID =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkAndroidHardwareBufferUsageANDROID =
             #{offset VkAndroidHardwareBufferUsageANDROID, sType}
        type FieldIsArray "sType" VkAndroidHardwareBufferUsageANDROID =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferUsageANDROID, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkAndroidHardwareBufferUsageANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferUsageANDROID, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkAndroidHardwareBufferUsageANDROID where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkAndroidHardwareBufferUsageANDROID where
        type FieldType "pNext" VkAndroidHardwareBufferUsageANDROID =
             Ptr Void
        type FieldOptional "pNext" VkAndroidHardwareBufferUsageANDROID =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkAndroidHardwareBufferUsageANDROID =
             #{offset VkAndroidHardwareBufferUsageANDROID, pNext}
        type FieldIsArray "pNext" VkAndroidHardwareBufferUsageANDROID =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferUsageANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkAndroidHardwareBufferUsageANDROID where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferUsageANDROID, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkAndroidHardwareBufferUsageANDROID where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, pNext}

instance {-# OVERLAPPING #-}
         HasField "androidHardwareBufferUsage"
           VkAndroidHardwareBufferUsageANDROID
         where
        type FieldType "androidHardwareBufferUsage"
               VkAndroidHardwareBufferUsageANDROID
             = Word64
        type FieldOptional "androidHardwareBufferUsage"
               VkAndroidHardwareBufferUsageANDROID
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "androidHardwareBufferUsage"
               VkAndroidHardwareBufferUsageANDROID
             =
             #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage}
        type FieldIsArray "androidHardwareBufferUsage"
               VkAndroidHardwareBufferUsageANDROID
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage}

instance {-# OVERLAPPING #-}
         CanReadField "androidHardwareBufferUsage"
           VkAndroidHardwareBufferUsageANDROID
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage}

instance {-# OVERLAPPING #-}
         CanWriteField "androidHardwareBufferUsage"
           VkAndroidHardwareBufferUsageANDROID
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkAndroidHardwareBufferUsageANDROID, androidHardwareBufferUsage}

instance Show VkAndroidHardwareBufferUsageANDROID where
        showsPrec d x
          = showString "VkAndroidHardwareBufferUsageANDROID {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "androidHardwareBufferUsage = " .
                            showsPrec d (getField @"androidHardwareBufferUsage" x) .
                              showChar '}'
