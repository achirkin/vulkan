#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedFFITypes     #-}
{-# LANGUAGE ViewPatterns         #-}
module Graphics.Vulkan.Ext.VK_KHR_variable_pointers
       (-- * Vulkan extension: @VK_KHR_variable_pointers@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @121@
        --
        -- Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_storage_buffer_storage_class'.
        --

        -- ** Required extensions: 'VK_KHR_get_physical_device_properties2', 'VK_KHR_storage_buffer_storage_class'.
        VkPhysicalDeviceVariablePointerFeaturesKHR(..),
        VK_KHR_VARIABLE_POINTERS_SPEC_VERSION,
        pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION,
        VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME,
        pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR)
       where
import           Data.Int
import           Data.Void                        (Void)
import           Data.Word
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Core
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkPhysicalDeviceVariablePointerFeaturesKHR = VkPhysicalDeviceVariablePointerFeaturesKHR## ByteArray##

instance Eq VkPhysicalDeviceVariablePointerFeaturesKHR where
        (VkPhysicalDeviceVariablePointerFeaturesKHR## a) ==
          (VkPhysicalDeviceVariablePointerFeaturesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceVariablePointerFeaturesKHR where
        (VkPhysicalDeviceVariablePointerFeaturesKHR## a) `compare`
          (VkPhysicalDeviceVariablePointerFeaturesKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceVariablePointerFeaturesKHR where
        sizeOf ~_
          = #{size VkPhysicalDeviceVariablePointerFeaturesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceVariablePointerFeaturesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceVariablePointerFeaturesKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceVariablePointerFeaturesKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPhysicalDeviceVariablePointerFeaturesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkPhysicalDeviceVariablePointerFeaturesKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceVariablePointerFeaturesKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPhysicalDeviceVariablePointerFeaturesKHR),
            I## a <- alignment
                      (undefined :: VkPhysicalDeviceVariablePointerFeaturesKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPhysicalDeviceVariablePointerFeaturesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPhysicalDeviceVariablePointerFeaturesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkPhysicalDeviceVariablePointerFeaturesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkPhysicalDeviceVariablePointerFeaturesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkPhysicalDeviceVariablePointerFeaturesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkPhysicalDeviceVariablePointerFeaturesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceVariablePointerFeaturesKHR where
        type VkSTypeMType VkPhysicalDeviceVariablePointerFeaturesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceVariablePointerFeaturesKHR where
        type VkPNextMType VkPhysicalDeviceVariablePointerFeaturesKHR =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasVkVariablePointersStorageBuffer
           VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        type VkVariablePointersStorageBufferMType
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = VkBool32

        {-# NOINLINE vkVariablePointersStorageBuffer #-}
        vkVariablePointersStorageBuffer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer})

        {-# INLINE vkVariablePointersStorageBufferByteOffset #-}
        vkVariablePointersStorageBufferByteOffset ~_
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer}

        {-# INLINE readVkVariablePointersStorageBuffer #-}
        readVkVariablePointersStorageBuffer p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer}

        {-# INLINE writeVkVariablePointersStorageBuffer #-}
        writeVkVariablePointersStorageBuffer p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointersStorageBuffer}

instance {-# OVERLAPPING #-}
         HasVkVariablePointers VkPhysicalDeviceVariablePointerFeaturesKHR
         where
        type VkVariablePointersMType
               VkPhysicalDeviceVariablePointerFeaturesKHR
             = VkBool32

        {-# NOINLINE vkVariablePointers #-}
        vkVariablePointers x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers})

        {-# INLINE vkVariablePointersByteOffset #-}
        vkVariablePointersByteOffset ~_
          = #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers}

        {-# INLINE readVkVariablePointers #-}
        readVkVariablePointers p
          = peekByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers}

        {-# INLINE writeVkVariablePointers #-}
        writeVkVariablePointers p
          = pokeByteOff p #{offset VkPhysicalDeviceVariablePointerFeaturesKHR, variablePointers}

instance Show VkPhysicalDeviceVariablePointerFeaturesKHR where
        showsPrec d x
          = showString "VkPhysicalDeviceVariablePointerFeaturesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkVariablePointersStorageBuffer = " .
                            showsPrec d (vkVariablePointersStorageBuffer x) .
                              showString ", " .
                                showString "vkVariablePointers = " .
                                  showsPrec d (vkVariablePointers x) . showChar '}'

pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION = 1

type VK_KHR_VARIABLE_POINTERS_SPEC_VERSION = 1

pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: CString

pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME <-
        (is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME -> True)
  where VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
          = _VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME

_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME #-}
_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  = Ptr "VK_KHR_variable_pointers\NUL"##

is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME #-}
is_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME
  = (_VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME ==)

type VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME =
     "VK_KHR_variable_pointers"

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
        = VkStructureType 1000120000
