#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2
       (-- * Vulkan extension: @VK_KHR_get_physical_device_properties2@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jeff Bolz @jbolz@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @60@
        VkPhysicalDeviceFeatures2KHR(..),
        VkPhysicalDeviceProperties2KHR(..), VkFormatProperties2KHR(..),
        VkImageFormatProperties2KHR(..),
        VkPhysicalDeviceImageFormatInfo2KHR(..),
        VkQueueFamilyProperties2KHR(..),
        VkPhysicalDeviceMemoryProperties2KHR(..),
        VkSparseImageFormatProperties2KHR(..),
        VkPhysicalDeviceSparseImageFormatInfo2KHR(..),
        vkGetPhysicalDeviceFeatures2KHR, vkGetPhysicalDeviceProperties2KHR,
        vkGetPhysicalDeviceFormatProperties2KHR,
        vkGetPhysicalDeviceImageFormatProperties2KHR,
        vkGetPhysicalDeviceQueueFamilyProperties2KHR,
        vkGetPhysicalDeviceMemoryProperties2KHR,
        vkGetPhysicalDeviceSparseImageFormatProperties2KHR,
        VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION,
        pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION,
        VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME,
        pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR,
        pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkFormatProperties,
                                                   VkImageFormatProperties,
                                                   VkPhysicalDeviceFeatures,
                                                   VkPhysicalDeviceMemoryProperties,
                                                   VkPhysicalDeviceProperties,
                                                   VkQueueFamilyProperties,
                                                   VkSparseImageFormatProperties)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceFeatures2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceFeatures         features;
--   > } VkPhysicalDeviceFeatures2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceFeatures2KHR.html VkPhysicalDeviceFeatures2KHR registry at www.khronos.org>
data VkPhysicalDeviceFeatures2KHR = VkPhysicalDeviceFeatures2KHR## ByteArray##

instance Eq VkPhysicalDeviceFeatures2KHR where
        (VkPhysicalDeviceFeatures2KHR## a) ==
          (VkPhysicalDeviceFeatures2KHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceFeatures2KHR where
        (VkPhysicalDeviceFeatures2KHR## a) `compare`
          (VkPhysicalDeviceFeatures2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceFeatures2KHR where
        sizeOf ~_ = #{size VkPhysicalDeviceFeatures2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceFeatures2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceFeatures2KHR),
            I## a <- alignment (undefined :: VkPhysicalDeviceFeatures2KHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceFeatures2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceFeatures2KHR## ba)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceFeatures2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceFeatures2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceFeatures2KHR),
            I## a <- alignment (undefined :: VkPhysicalDeviceFeatures2KHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceFeatures2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceFeatures2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkPhysicalDeviceFeatures2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceFeatures2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceFeatures2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceFeatures2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceFeatures2KHR where
        type VkSTypeMType VkPhysicalDeviceFeatures2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceFeatures2KHR where
        type FieldType "sType" VkPhysicalDeviceFeatures2KHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkPhysicalDeviceFeatures2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPhysicalDeviceFeatures2KHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceFeatures2KHR where
        type VkPNextMType VkPhysicalDeviceFeatures2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceFeatures2KHR where
        type FieldType "pNext" VkPhysicalDeviceFeatures2KHR = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkPhysicalDeviceFeatures2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPhysicalDeviceFeatures2KHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFeatures VkPhysicalDeviceFeatures2KHR where
        type VkFeaturesMType VkPhysicalDeviceFeatures2KHR =
             VkPhysicalDeviceFeatures

        {-# NOINLINE vkFeatures #-}
        vkFeatures x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceFeatures2KHR, features})

        {-# INLINE vkFeaturesByteOffset #-}
        vkFeaturesByteOffset ~_
          = #{offset VkPhysicalDeviceFeatures2KHR, features}

        {-# INLINE readVkFeatures #-}
        readVkFeatures p
          = peekByteOff p #{offset VkPhysicalDeviceFeatures2KHR, features}

        {-# INLINE writeVkFeatures #-}
        writeVkFeatures p
          = pokeByteOff p #{offset VkPhysicalDeviceFeatures2KHR, features}

instance {-# OVERLAPPING #-}
         HasField "features" VkPhysicalDeviceFeatures2KHR where
        type FieldType "features" VkPhysicalDeviceFeatures2KHR =
             VkPhysicalDeviceFeatures
        type FieldOptional "features" VkPhysicalDeviceFeatures2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "features" VkPhysicalDeviceFeatures2KHR where
        {-# INLINE getField #-}
        getField = vkFeatures

        {-# INLINE readField #-}
        readField = readVkFeatures

instance CanWriteField "features" VkPhysicalDeviceFeatures2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFeatures

instance Show VkPhysicalDeviceFeatures2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceFeatures2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFeatures = " .
                            showsPrec d (vkFeatures x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceProperties       properties;
--   > } VkPhysicalDeviceProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceProperties2KHR.html VkPhysicalDeviceProperties2KHR registry at www.khronos.org>
data VkPhysicalDeviceProperties2KHR = VkPhysicalDeviceProperties2KHR## ByteArray##

instance Eq VkPhysicalDeviceProperties2KHR where
        (VkPhysicalDeviceProperties2KHR## a) ==
          (VkPhysicalDeviceProperties2KHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceProperties2KHR where
        (VkPhysicalDeviceProperties2KHR## a) `compare`
          (VkPhysicalDeviceProperties2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceProperties2KHR where
        sizeOf ~_ = #{size VkPhysicalDeviceProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceProperties2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceProperties2KHR),
            I## a <- alignment (undefined :: VkPhysicalDeviceProperties2KHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceProperties2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceProperties2KHR## ba)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceProperties2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceProperties2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceProperties2KHR),
            I## a <- alignment (undefined :: VkPhysicalDeviceProperties2KHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceProperties2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceProperties2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkPhysicalDeviceProperties2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceProperties2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceProperties2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceProperties2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceProperties2KHR where
        type VkSTypeMType VkPhysicalDeviceProperties2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceProperties2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceProperties2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceProperties2KHR where
        type FieldType "sType" VkPhysicalDeviceProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceProperties2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkPhysicalDeviceProperties2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceProperties2KHR where
        type VkPNextMType VkPhysicalDeviceProperties2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceProperties2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceProperties2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceProperties2KHR where
        type FieldType "pNext" VkPhysicalDeviceProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceProperties2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkPhysicalDeviceProperties2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkProperties VkPhysicalDeviceProperties2KHR where
        type VkPropertiesMType VkPhysicalDeviceProperties2KHR =
             VkPhysicalDeviceProperties

        {-# NOINLINE vkProperties #-}
        vkProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceProperties2KHR, properties})

        {-# INLINE vkPropertiesByteOffset #-}
        vkPropertiesByteOffset ~_
          = #{offset VkPhysicalDeviceProperties2KHR, properties}

        {-# INLINE readVkProperties #-}
        readVkProperties p
          = peekByteOff p #{offset VkPhysicalDeviceProperties2KHR, properties}

        {-# INLINE writeVkProperties #-}
        writeVkProperties p
          = pokeByteOff p #{offset VkPhysicalDeviceProperties2KHR, properties}

instance {-# OVERLAPPING #-}
         HasField "properties" VkPhysicalDeviceProperties2KHR where
        type FieldType "properties" VkPhysicalDeviceProperties2KHR =
             VkPhysicalDeviceProperties
        type FieldOptional "properties" VkPhysicalDeviceProperties2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "properties" VkPhysicalDeviceProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkProperties

        {-# INLINE readField #-}
        readField = readVkProperties

instance Show VkPhysicalDeviceProperties2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceProperties2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkProperties = " .
                            showsPrec d (vkProperties x) . showChar '}'

-- | > typedef struct VkFormatProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkFormatProperties               formatProperties;
--   > } VkFormatProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkFormatProperties2KHR.html VkFormatProperties2KHR registry at www.khronos.org>
data VkFormatProperties2KHR = VkFormatProperties2KHR## ByteArray##

instance Eq VkFormatProperties2KHR where
        (VkFormatProperties2KHR## a) == (VkFormatProperties2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkFormatProperties2KHR where
        (VkFormatProperties2KHR## a) `compare` (VkFormatProperties2KHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkFormatProperties2KHR where
        sizeOf ~_ = #{size VkFormatProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkFormatProperties2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkFormatProperties2KHR),
            I## a <- alignment (undefined :: VkFormatProperties2KHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkFormatProperties2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkFormatProperties2KHR## ba)
          | I## n <- sizeOf (undefined :: VkFormatProperties2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkFormatProperties2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkFormatProperties2KHR),
            I## a <- alignment (undefined :: VkFormatProperties2KHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkFormatProperties2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkFormatProperties2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkFormatProperties2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkFormatProperties2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkFormatProperties2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkFormatProperties2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkFormatProperties2KHR
         where
        type VkSTypeMType VkFormatProperties2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkFormatProperties2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkFormatProperties2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkFormatProperties2KHR where
        type FieldType "sType" VkFormatProperties2KHR = VkStructureType
        type FieldOptional "sType" VkFormatProperties2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkFormatProperties2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkFormatProperties2KHR
         where
        type VkPNextMType VkFormatProperties2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkFormatProperties2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkFormatProperties2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkFormatProperties2KHR where
        type FieldType "pNext" VkFormatProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkFormatProperties2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkFormatProperties2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkFormatProperties VkFormatProperties2KHR where
        type VkFormatPropertiesMType VkFormatProperties2KHR =
             VkFormatProperties

        {-# NOINLINE vkFormatProperties #-}
        vkFormatProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkFormatProperties2KHR, formatProperties})

        {-# INLINE vkFormatPropertiesByteOffset #-}
        vkFormatPropertiesByteOffset ~_
          = #{offset VkFormatProperties2KHR, formatProperties}

        {-# INLINE readVkFormatProperties #-}
        readVkFormatProperties p
          = peekByteOff p #{offset VkFormatProperties2KHR, formatProperties}

        {-# INLINE writeVkFormatProperties #-}
        writeVkFormatProperties p
          = pokeByteOff p #{offset VkFormatProperties2KHR, formatProperties}

instance {-# OVERLAPPING #-}
         HasField "formatProperties" VkFormatProperties2KHR where
        type FieldType "formatProperties" VkFormatProperties2KHR =
             VkFormatProperties
        type FieldOptional "formatProperties" VkFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "formatProperties" VkFormatProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkFormatProperties

        {-# INLINE readField #-}
        readField = readVkFormatProperties

instance Show VkFormatProperties2KHR where
        showsPrec d x
          = showString "VkFormatProperties2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFormatProperties = " .
                            showsPrec d (vkFormatProperties x) . showChar '}'

-- | > typedef struct VkImageFormatProperties2KHR {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkImageFormatProperties          imageFormatProperties;
--   > } VkImageFormatProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkImageFormatProperties2KHR.html VkImageFormatProperties2KHR registry at www.khronos.org>
data VkImageFormatProperties2KHR = VkImageFormatProperties2KHR## ByteArray##

instance Eq VkImageFormatProperties2KHR where
        (VkImageFormatProperties2KHR## a) ==
          (VkImageFormatProperties2KHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkImageFormatProperties2KHR where
        (VkImageFormatProperties2KHR## a) `compare`
          (VkImageFormatProperties2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkImageFormatProperties2KHR where
        sizeOf ~_ = #{size VkImageFormatProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkImageFormatProperties2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkImageFormatProperties2KHR),
            I## a <- alignment (undefined :: VkImageFormatProperties2KHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkImageFormatProperties2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkImageFormatProperties2KHR## ba)
          | I## n <- sizeOf (undefined :: VkImageFormatProperties2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkImageFormatProperties2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkImageFormatProperties2KHR),
            I## a <- alignment (undefined :: VkImageFormatProperties2KHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkImageFormatProperties2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkImageFormatProperties2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkImageFormatProperties2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkImageFormatProperties2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkImageFormatProperties2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkImageFormatProperties2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkImageFormatProperties2KHR
         where
        type VkSTypeMType VkImageFormatProperties2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImageFormatProperties2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImageFormatProperties2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImageFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImageFormatProperties2KHR where
        type FieldType "sType" VkImageFormatProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkImageFormatProperties2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkImageFormatProperties2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkImageFormatProperties2KHR
         where
        type VkPNextMType VkImageFormatProperties2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImageFormatProperties2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImageFormatProperties2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImageFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImageFormatProperties2KHR where
        type FieldType "pNext" VkImageFormatProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkImageFormatProperties2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkImageFormatProperties2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkImageFormatProperties VkImageFormatProperties2KHR where
        type VkImageFormatPropertiesMType VkImageFormatProperties2KHR =
             VkImageFormatProperties

        {-# NOINLINE vkImageFormatProperties #-}
        vkImageFormatProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImageFormatProperties2KHR, imageFormatProperties})

        {-# INLINE vkImageFormatPropertiesByteOffset #-}
        vkImageFormatPropertiesByteOffset ~_
          = #{offset VkImageFormatProperties2KHR, imageFormatProperties}

        {-# INLINE readVkImageFormatProperties #-}
        readVkImageFormatProperties p
          = peekByteOff p #{offset VkImageFormatProperties2KHR, imageFormatProperties}

        {-# INLINE writeVkImageFormatProperties #-}
        writeVkImageFormatProperties p
          = pokeByteOff p #{offset VkImageFormatProperties2KHR, imageFormatProperties}

instance {-# OVERLAPPING #-}
         HasField "imageFormatProperties" VkImageFormatProperties2KHR where
        type FieldType "imageFormatProperties" VkImageFormatProperties2KHR
             = VkImageFormatProperties
        type FieldOptional "imageFormatProperties"
               VkImageFormatProperties2KHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "imageFormatProperties"
           VkImageFormatProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkImageFormatProperties

        {-# INLINE readField #-}
        readField = readVkImageFormatProperties

instance Show VkImageFormatProperties2KHR where
        showsPrec d x
          = showString "VkImageFormatProperties2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkImageFormatProperties = " .
                            showsPrec d (vkImageFormatProperties x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceImageFormatInfo2KHR {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkFormat                         format;
--   >     VkImageType                      type;
--   >     VkImageTiling                    tiling;
--   >     VkImageUsageFlags                usage;
--   >     VkImageCreateFlags flags;
--   > } VkPhysicalDeviceImageFormatInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceImageFormatInfo2KHR.html VkPhysicalDeviceImageFormatInfo2KHR registry at www.khronos.org>
data VkPhysicalDeviceImageFormatInfo2KHR = VkPhysicalDeviceImageFormatInfo2KHR## ByteArray##

instance Eq VkPhysicalDeviceImageFormatInfo2KHR where
        (VkPhysicalDeviceImageFormatInfo2KHR## a) ==
          (VkPhysicalDeviceImageFormatInfo2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceImageFormatInfo2KHR where
        (VkPhysicalDeviceImageFormatInfo2KHR## a) `compare`
          (VkPhysicalDeviceImageFormatInfo2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceImageFormatInfo2KHR where
        sizeOf ~_ = #{size VkPhysicalDeviceImageFormatInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceImageFormatInfo2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceImageFormatInfo2KHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceImageFormatInfo2KHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceImageFormatInfo2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceImageFormatInfo2KHR## ba)
          | I## n <- sizeOf (undefined :: VkPhysicalDeviceImageFormatInfo2KHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceImageFormatInfo2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceImageFormatInfo2KHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceImageFormatInfo2KHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceImageFormatInfo2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceImageFormatInfo2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceImageFormatInfo2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceImageFormatInfo2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceImageFormatInfo2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceImageFormatInfo2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceImageFormatInfo2KHR where
        type VkSTypeMType VkPhysicalDeviceImageFormatInfo2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "sType" VkPhysicalDeviceImageFormatInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceImageFormatInfo2KHR where
        type VkPNextMType VkPhysicalDeviceImageFormatInfo2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "pNext" VkPhysicalDeviceImageFormatInfo2KHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFormat VkPhysicalDeviceImageFormatInfo2KHR where
        type VkFormatMType VkPhysicalDeviceImageFormatInfo2KHR = VkFormat

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}

instance {-# OVERLAPPING #-}
         HasField "format" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "format" VkPhysicalDeviceImageFormatInfo2KHR =
             VkFormat
        type FieldOptional "format" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "format" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-}
         HasVkType VkPhysicalDeviceImageFormatInfo2KHR where
        type VkTypeMType VkPhysicalDeviceImageFormatInfo2KHR = VkImageType

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}

instance {-# OVERLAPPING #-}
         HasField "type" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "type" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageType
        type FieldOptional "type" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "type" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkType

        {-# INLINE readField #-}
        readField = readVkType

instance CanWriteField "type" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkType

instance {-# OVERLAPPING #-}
         HasVkTiling VkPhysicalDeviceImageFormatInfo2KHR where
        type VkTilingMType VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageTiling

        {-# NOINLINE vkTiling #-}
        vkTiling x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling})

        {-# INLINE vkTilingByteOffset #-}
        vkTilingByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}

        {-# INLINE readVkTiling #-}
        readVkTiling p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}

        {-# INLINE writeVkTiling #-}
        writeVkTiling p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}

instance {-# OVERLAPPING #-}
         HasField "tiling" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "tiling" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageTiling
        type FieldOptional "tiling" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "tiling" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkTiling

        {-# INLINE readField #-}
        readField = readVkTiling

instance CanWriteField "tiling" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkTiling

instance {-# OVERLAPPING #-}
         HasVkUsage VkPhysicalDeviceImageFormatInfo2KHR where
        type VkUsageMType VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageUsageFlags

        {-# NOINLINE vkUsage #-}
        vkUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage})

        {-# INLINE vkUsageByteOffset #-}
        vkUsageByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}

        {-# INLINE readVkUsage #-}
        readVkUsage p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}

        {-# INLINE writeVkUsage #-}
        writeVkUsage p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "usage" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageUsageFlags
        type FieldOptional "usage" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "usage" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkUsage

        {-# INLINE readField #-}
        readField = readVkUsage

instance CanWriteField "usage" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkUsage

instance {-# OVERLAPPING #-}
         HasVkFlags VkPhysicalDeviceImageFormatInfo2KHR where
        type VkFlagsMType VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "flags" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageCreateFlags
        type FieldOptional "flags" VkPhysicalDeviceImageFormatInfo2KHR =
             'True -- ' closing tick for hsc2hs

instance CanReadField "flags" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance Show VkPhysicalDeviceImageFormatInfo2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceImageFormatInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFormat = " .
                            showsPrec d (vkFormat x) .
                              showString ", " .
                                showString "vkType = " .
                                  showsPrec d (vkType x) .
                                    showString ", " .
                                      showString "vkTiling = " .
                                        showsPrec d (vkTiling x) .
                                          showString ", " .
                                            showString "vkUsage = " .
                                              showsPrec d (vkUsage x) .
                                                showString ", " .
                                                  showString "vkFlags = " .
                                                    showsPrec d (vkFlags x) . showChar '}'

-- | > typedef struct VkQueueFamilyProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkQueueFamilyProperties          queueFamilyProperties;
--   > } VkQueueFamilyProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkQueueFamilyProperties2KHR.html VkQueueFamilyProperties2KHR registry at www.khronos.org>
data VkQueueFamilyProperties2KHR = VkQueueFamilyProperties2KHR## ByteArray##

instance Eq VkQueueFamilyProperties2KHR where
        (VkQueueFamilyProperties2KHR## a) ==
          (VkQueueFamilyProperties2KHR## b) = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkQueueFamilyProperties2KHR where
        (VkQueueFamilyProperties2KHR## a) `compare`
          (VkQueueFamilyProperties2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkQueueFamilyProperties2KHR where
        sizeOf ~_ = #{size VkQueueFamilyProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkQueueFamilyProperties2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkQueueFamilyProperties2KHR),
            I## a <- alignment (undefined :: VkQueueFamilyProperties2KHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkQueueFamilyProperties2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkQueueFamilyProperties2KHR## ba)
          | I## n <- sizeOf (undefined :: VkQueueFamilyProperties2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkQueueFamilyProperties2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkQueueFamilyProperties2KHR),
            I## a <- alignment (undefined :: VkQueueFamilyProperties2KHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkQueueFamilyProperties2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkQueueFamilyProperties2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkQueueFamilyProperties2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkQueueFamilyProperties2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkQueueFamilyProperties2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkQueueFamilyProperties2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkQueueFamilyProperties2KHR
         where
        type VkSTypeMType VkQueueFamilyProperties2KHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkQueueFamilyProperties2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkQueueFamilyProperties2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkQueueFamilyProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkQueueFamilyProperties2KHR where
        type FieldType "sType" VkQueueFamilyProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkQueueFamilyProperties2KHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkQueueFamilyProperties2KHR
         where
        type VkPNextMType VkQueueFamilyProperties2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkQueueFamilyProperties2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkQueueFamilyProperties2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkQueueFamilyProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkQueueFamilyProperties2KHR where
        type FieldType "pNext" VkQueueFamilyProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkQueueFamilyProperties2KHR = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkQueueFamilyProperties2KHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkQueueFamilyProperties VkQueueFamilyProperties2KHR where
        type VkQueueFamilyPropertiesMType VkQueueFamilyProperties2KHR =
             VkQueueFamilyProperties

        {-# NOINLINE vkQueueFamilyProperties #-}
        vkQueueFamilyProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties})

        {-# INLINE vkQueueFamilyPropertiesByteOffset #-}
        vkQueueFamilyPropertiesByteOffset ~_
          = #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}

        {-# INLINE readVkQueueFamilyProperties #-}
        readVkQueueFamilyProperties p
          = peekByteOff p #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}

        {-# INLINE writeVkQueueFamilyProperties #-}
        writeVkQueueFamilyProperties p
          = pokeByteOff p #{offset VkQueueFamilyProperties2KHR, queueFamilyProperties}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyProperties" VkQueueFamilyProperties2KHR where
        type FieldType "queueFamilyProperties" VkQueueFamilyProperties2KHR
             = VkQueueFamilyProperties
        type FieldOptional "queueFamilyProperties"
               VkQueueFamilyProperties2KHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "queueFamilyProperties"
           VkQueueFamilyProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkQueueFamilyProperties

        {-# INLINE readField #-}
        readField = readVkQueueFamilyProperties

instance Show VkQueueFamilyProperties2KHR where
        showsPrec d x
          = showString "VkQueueFamilyProperties2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkQueueFamilyProperties = " .
                            showsPrec d (vkQueueFamilyProperties x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceMemoryProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkPhysicalDeviceMemoryProperties memoryProperties;
--   > } VkPhysicalDeviceMemoryProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceMemoryProperties2KHR.html VkPhysicalDeviceMemoryProperties2KHR registry at www.khronos.org>
data VkPhysicalDeviceMemoryProperties2KHR = VkPhysicalDeviceMemoryProperties2KHR## ByteArray##

instance Eq VkPhysicalDeviceMemoryProperties2KHR where
        (VkPhysicalDeviceMemoryProperties2KHR## a) ==
          (VkPhysicalDeviceMemoryProperties2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceMemoryProperties2KHR where
        (VkPhysicalDeviceMemoryProperties2KHR## a) `compare`
          (VkPhysicalDeviceMemoryProperties2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceMemoryProperties2KHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceMemoryProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceMemoryProperties2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceMemoryProperties2KHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceMemoryProperties2KHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceMemoryProperties2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceMemoryProperties2KHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceMemoryProperties2KHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceMemoryProperties2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceMemoryProperties2KHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceMemoryProperties2KHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceMemoryProperties2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceMemoryProperties2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceMemoryProperties2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceMemoryProperties2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceMemoryProperties2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceMemoryProperties2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceMemoryProperties2KHR where
        type VkSTypeMType VkPhysicalDeviceMemoryProperties2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceMemoryProperties2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceMemoryProperties2KHR where
        type FieldType "sType" VkPhysicalDeviceMemoryProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceMemoryProperties2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkPhysicalDeviceMemoryProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceMemoryProperties2KHR where
        type VkPNextMType VkPhysicalDeviceMemoryProperties2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceMemoryProperties2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceMemoryProperties2KHR where
        type FieldType "pNext" VkPhysicalDeviceMemoryProperties2KHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceMemoryProperties2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkPhysicalDeviceMemoryProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkMemoryProperties VkPhysicalDeviceMemoryProperties2KHR where
        type VkMemoryPropertiesMType VkPhysicalDeviceMemoryProperties2KHR =
             VkPhysicalDeviceMemoryProperties

        {-# NOINLINE vkMemoryProperties #-}
        vkMemoryProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceMemoryProperties2KHR, memoryProperties})

        {-# INLINE vkMemoryPropertiesByteOffset #-}
        vkMemoryPropertiesByteOffset ~_
          = #{offset VkPhysicalDeviceMemoryProperties2KHR, memoryProperties}

        {-# INLINE readVkMemoryProperties #-}
        readVkMemoryProperties p
          = peekByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, memoryProperties}

        {-# INLINE writeVkMemoryProperties #-}
        writeVkMemoryProperties p
          = pokeByteOff p #{offset VkPhysicalDeviceMemoryProperties2KHR, memoryProperties}

instance {-# OVERLAPPING #-}
         HasField "memoryProperties" VkPhysicalDeviceMemoryProperties2KHR
         where
        type FieldType "memoryProperties"
               VkPhysicalDeviceMemoryProperties2KHR
             = VkPhysicalDeviceMemoryProperties
        type FieldOptional "memoryProperties"
               VkPhysicalDeviceMemoryProperties2KHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "memoryProperties"
           VkPhysicalDeviceMemoryProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkMemoryProperties

        {-# INLINE readField #-}
        readField = readVkMemoryProperties

instance Show VkPhysicalDeviceMemoryProperties2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceMemoryProperties2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMemoryProperties = " .
                            showsPrec d (vkMemoryProperties x) . showChar '}'

-- | > typedef struct VkSparseImageFormatProperties2KHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkSparseImageFormatProperties    properties;
--   > } VkSparseImageFormatProperties2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSparseImageFormatProperties2KHR.html VkSparseImageFormatProperties2KHR registry at www.khronos.org>
data VkSparseImageFormatProperties2KHR = VkSparseImageFormatProperties2KHR## ByteArray##

instance Eq VkSparseImageFormatProperties2KHR where
        (VkSparseImageFormatProperties2KHR## a) ==
          (VkSparseImageFormatProperties2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSparseImageFormatProperties2KHR where
        (VkSparseImageFormatProperties2KHR## a) `compare`
          (VkSparseImageFormatProperties2KHR## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSparseImageFormatProperties2KHR where
        sizeOf ~_ = #{size VkSparseImageFormatProperties2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSparseImageFormatProperties2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkSparseImageFormatProperties2KHR),
            I## a <- alignment (undefined :: VkSparseImageFormatProperties2KHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSparseImageFormatProperties2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSparseImageFormatProperties2KHR## ba)
          | I## n <- sizeOf (undefined :: VkSparseImageFormatProperties2KHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSparseImageFormatProperties2KHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkSparseImageFormatProperties2KHR),
            I## a <- alignment (undefined :: VkSparseImageFormatProperties2KHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSparseImageFormatProperties2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSparseImageFormatProperties2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkSparseImageFormatProperties2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSparseImageFormatProperties2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSparseImageFormatProperties2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSparseImageFormatProperties2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkSparseImageFormatProperties2KHR where
        type VkSTypeMType VkSparseImageFormatProperties2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSparseImageFormatProperties2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSparseImageFormatProperties2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSparseImageFormatProperties2KHR where
        type FieldType "sType" VkSparseImageFormatProperties2KHR =
             VkStructureType
        type FieldOptional "sType" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "sType" VkSparseImageFormatProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSparseImageFormatProperties2KHR where
        type VkPNextMType VkSparseImageFormatProperties2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSparseImageFormatProperties2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSparseImageFormatProperties2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSparseImageFormatProperties2KHR where
        type FieldType "pNext" VkSparseImageFormatProperties2KHR = Ptr Void
        type FieldOptional "pNext" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "pNext" VkSparseImageFormatProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkProperties VkSparseImageFormatProperties2KHR where
        type VkPropertiesMType VkSparseImageFormatProperties2KHR =
             VkSparseImageFormatProperties

        {-# NOINLINE vkProperties #-}
        vkProperties x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSparseImageFormatProperties2KHR, properties})

        {-# INLINE vkPropertiesByteOffset #-}
        vkPropertiesByteOffset ~_
          = #{offset VkSparseImageFormatProperties2KHR, properties}

        {-# INLINE readVkProperties #-}
        readVkProperties p
          = peekByteOff p #{offset VkSparseImageFormatProperties2KHR, properties}

        {-# INLINE writeVkProperties #-}
        writeVkProperties p
          = pokeByteOff p #{offset VkSparseImageFormatProperties2KHR, properties}

instance {-# OVERLAPPING #-}
         HasField "properties" VkSparseImageFormatProperties2KHR where
        type FieldType "properties" VkSparseImageFormatProperties2KHR =
             VkSparseImageFormatProperties
        type FieldOptional "properties" VkSparseImageFormatProperties2KHR =
             'False -- ' closing tick for hsc2hs

instance CanReadField "properties"
           VkSparseImageFormatProperties2KHR
         where
        {-# INLINE getField #-}
        getField = vkProperties

        {-# INLINE readField #-}
        readField = readVkProperties

instance Show VkSparseImageFormatProperties2KHR where
        showsPrec d x
          = showString "VkSparseImageFormatProperties2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkProperties = " .
                            showsPrec d (vkProperties x) . showChar '}'

-- | > typedef struct VkPhysicalDeviceSparseImageFormatInfo2KHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkFormat                         format;
--   >     VkImageType                      type;
--   >     VkSampleCountFlagBits            samples;
--   >     VkImageUsageFlags                usage;
--   >     VkImageTiling                    tiling;
--   > } VkPhysicalDeviceSparseImageFormatInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPhysicalDeviceSparseImageFormatInfo2KHR.html VkPhysicalDeviceSparseImageFormatInfo2KHR registry at www.khronos.org>
data VkPhysicalDeviceSparseImageFormatInfo2KHR = VkPhysicalDeviceSparseImageFormatInfo2KHR## ByteArray##

instance Eq VkPhysicalDeviceSparseImageFormatInfo2KHR where
        (VkPhysicalDeviceSparseImageFormatInfo2KHR## a) ==
          (VkPhysicalDeviceSparseImageFormatInfo2KHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSparseImageFormatInfo2KHR where
        (VkPhysicalDeviceSparseImageFormatInfo2KHR## a) `compare`
          (VkPhysicalDeviceSparseImageFormatInfo2KHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSparseImageFormatInfo2KHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceSparseImageFormatInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSparseImageFormatInfo2KHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceSparseImageFormatInfo2KHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceSparseImageFormatInfo2KHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceSparseImageFormatInfo2KHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceSparseImageFormatInfo2KHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceSparseImageFormatInfo2KHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceSparseImageFormatInfo2KHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceSparseImageFormatInfo2KHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceSparseImageFormatInfo2KHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceSparseImageFormatInfo2KHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceSparseImageFormatInfo2KHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceSparseImageFormatInfo2KHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceSparseImageFormatInfo2KHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceSparseImageFormatInfo2KHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkSTypeMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "sType" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "sType"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkPNextMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "pNext" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "pNext"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFormat VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkFormatMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkFormat

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, format}

instance {-# OVERLAPPING #-}
         HasField "format" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "format" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkFormat
        type FieldOptional "format"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "format"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-}
         HasVkType VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkTypeMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageType

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, type}

instance {-# OVERLAPPING #-}
         HasField "type" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "type" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageType
        type FieldOptional "type" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "type"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkType

        {-# INLINE readField #-}
        readField = readVkType

instance CanWriteField "type"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkType

instance {-# OVERLAPPING #-}
         HasVkSamples VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkSamplesMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkSampleCountFlagBits

        {-# NOINLINE vkSamples #-}
        vkSamples x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples})

        {-# INLINE vkSamplesByteOffset #-}
        vkSamplesByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}

        {-# INLINE readVkSamples #-}
        readVkSamples p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}

        {-# INLINE writeVkSamples #-}
        writeVkSamples p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, samples}

instance {-# OVERLAPPING #-}
         HasField "samples" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "samples" VkPhysicalDeviceSparseImageFormatInfo2KHR
             = VkSampleCountFlagBits
        type FieldOptional "samples"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "samples"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSamples

        {-# INLINE readField #-}
        readField = readVkSamples

instance CanWriteField "samples"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSamples

instance {-# OVERLAPPING #-}
         HasVkUsage VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkUsageMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageUsageFlags

        {-# NOINLINE vkUsage #-}
        vkUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage})

        {-# INLINE vkUsageByteOffset #-}
        vkUsageByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}

        {-# INLINE readVkUsage #-}
        readVkUsage p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}

        {-# INLINE writeVkUsage #-}
        writeVkUsage p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, usage}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "usage" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageUsageFlags
        type FieldOptional "usage"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "usage"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkUsage

        {-# INLINE readField #-}
        readField = readVkUsage

instance CanWriteField "usage"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkUsage

instance {-# OVERLAPPING #-}
         HasVkTiling VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type VkTilingMType VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageTiling

        {-# NOINLINE vkTiling #-}
        vkTiling x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling})

        {-# INLINE vkTilingByteOffset #-}
        vkTilingByteOffset ~_
          = #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}

        {-# INLINE readVkTiling #-}
        readVkTiling p
          = peekByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}

        {-# INLINE writeVkTiling #-}
        writeVkTiling p
          = pokeByteOff p #{offset VkPhysicalDeviceSparseImageFormatInfo2KHR, tiling}

instance {-# OVERLAPPING #-}
         HasField "tiling" VkPhysicalDeviceSparseImageFormatInfo2KHR where
        type FieldType "tiling" VkPhysicalDeviceSparseImageFormatInfo2KHR =
             VkImageTiling
        type FieldOptional "tiling"
               VkPhysicalDeviceSparseImageFormatInfo2KHR
             = 'False -- ' closing tick for hsc2hs

instance CanReadField "tiling"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkTiling

        {-# INLINE readField #-}
        readField = readVkTiling

instance CanWriteField "tiling"
           VkPhysicalDeviceSparseImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkTiling

instance Show VkPhysicalDeviceSparseImageFormatInfo2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceSparseImageFormatInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFormat = " .
                            showsPrec d (vkFormat x) .
                              showString ", " .
                                showString "vkType = " .
                                  showsPrec d (vkType x) .
                                    showString ", " .
                                      showString "vkSamples = " .
                                        showsPrec d (vkSamples x) .
                                          showString ", " .
                                            showString "vkUsage = " .
                                              showsPrec d (vkUsage x) .
                                                showString ", " .
                                                  showString "vkTiling = " .
                                                    showsPrec d (vkTiling x) . showChar '}'

-- | > void vkGetPhysicalDeviceFeatures2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceFeatures2KHR* pFeatures
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceFeatures2KHR.html vkGetPhysicalDeviceFeatures2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceFeatures2KHR"
               vkGetPhysicalDeviceFeatures2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceFeatures2KHR -- ^ pFeatures
                                                                    -> IO ()

-- | > void vkGetPhysicalDeviceProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceProperties2KHR.html vkGetPhysicalDeviceProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetPhysicalDeviceProperties2KHR"
               vkGetPhysicalDeviceProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> Ptr VkPhysicalDeviceProperties2KHR -- ^ pProperties
                                                                      -> IO ()

-- | > void vkGetPhysicalDeviceFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkFormat format
--   >     , VkFormatProperties2KHR* pFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceFormatProperties2KHR.html vkGetPhysicalDeviceFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceFormatProperties2KHR"
               vkGetPhysicalDeviceFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                -> VkFormat -- ^ format
                                            -> Ptr VkFormatProperties2KHR -- ^ pFormatProperties
                                                                          -> IO ()

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
--   > VkResult vkGetPhysicalDeviceImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceImageFormatInfo2KHR* pImageFormatInfo
--   >     , VkImageFormatProperties2KHR* pImageFormatProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceImageFormatProperties2KHR.html vkGetPhysicalDeviceImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceImageFormatProperties2KHR"
               vkGetPhysicalDeviceImageFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceImageFormatInfo2KHR -- ^ pImageFormatInfo
                                                         ->
                   Ptr VkImageFormatProperties2KHR -- ^ pImageFormatProperties
                                                   -> IO VkResult

-- | > void vkGetPhysicalDeviceQueueFamilyProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t* pQueueFamilyPropertyCount
--   >     , VkQueueFamilyProperties2KHR* pQueueFamilyProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceQueueFamilyProperties2KHR.html vkGetPhysicalDeviceQueueFamilyProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
               vkGetPhysicalDeviceQueueFamilyProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr Word32 -- ^ pQueueFamilyPropertyCount
                            -> Ptr VkQueueFamilyProperties2KHR -- ^ pQueueFamilyProperties
                                                               -> IO ()

-- | > void vkGetPhysicalDeviceMemoryProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , VkPhysicalDeviceMemoryProperties2KHR* pMemoryProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceMemoryProperties2KHR.html vkGetPhysicalDeviceMemoryProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceMemoryProperties2KHR"
               vkGetPhysicalDeviceMemoryProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceMemoryProperties2KHR -- ^ pMemoryProperties
                                                          -> IO ()

-- | > void vkGetPhysicalDeviceSparseImageFormatProperties2KHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , const VkPhysicalDeviceSparseImageFormatInfo2KHR* pFormatInfo
--   >     , uint32_t* pPropertyCount
--   >     , VkSparseImageFormatProperties2KHR* pProperties
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceSparseImageFormatProperties2KHR.html vkGetPhysicalDeviceSparseImageFormatProperties2KHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
               vkGetPhysicalDeviceSparseImageFormatProperties2KHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Ptr VkPhysicalDeviceSparseImageFormatInfo2KHR -- ^ pFormatInfo
                                                               ->
                   Ptr Word32 -- ^ pPropertyCount
                              -> Ptr VkSparseImageFormatProperties2KHR -- ^ pProperties
                                                                       -> IO ()

pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 1

type VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 1

pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME ::
        CString

pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME <-
        (is_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME -> True)
  where VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
          = _VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME

_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
           #-}
_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  = Ptr "VK_KHR_get_physical_device_properties2\NUL"##

is_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME ::
                                                          CString -> Bool

{-# INLINE is_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
           #-}
is_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  = (_VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME ==)

type VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME =
     "VK_KHR_get_physical_device_properties2"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR =
        VkStructureType 1000059000

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR =
        VkStructureType 1000059001

pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR =
        VkStructureType 1000059002

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR =
        VkStructureType 1000059003

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR =
        VkStructureType 1000059004

pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR =
        VkStructureType 1000059005

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR =
        VkStructureType 1000059006

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR =
        VkStructureType 1000059007

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
        = VkStructureType 1000059008
