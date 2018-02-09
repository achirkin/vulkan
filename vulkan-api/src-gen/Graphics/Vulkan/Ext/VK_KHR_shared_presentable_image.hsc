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
module Graphics.Vulkan.Ext.VK_KHR_shared_presentable_image
       (-- * Vulkan extension: @VK_KHR_shared_presentable_image@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Alon Or-bach @alonorbach@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @112@
        --
        -- Required extensions: 'VK_KHR_swapchain', 'VK_KHR_get_physical_device_properties2', 'VK_KHR_get_surface_capabilities2'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain', 'VK_KHR_get_physical_device_properties2', 'VK_KHR_get_surface_capabilities2'.
        VkSharedPresentSurfaceCapabilitiesKHR(..),
        vkGetSwapchainStatusKHR,
        VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION,
        pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION,
        VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME,
        pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR,
        pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR,
        pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR,
        pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkSharedPresentSurfaceCapabilitiesKHR {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     VkImageUsageFlags sharedPresentSupportedUsageFlags;
--   > } VkSharedPresentSurfaceCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkSharedPresentSurfaceCapabilitiesKHR.html VkSharedPresentSurfaceCapabilitiesKHR registry at www.khronos.org>
data VkSharedPresentSurfaceCapabilitiesKHR = VkSharedPresentSurfaceCapabilitiesKHR## ByteArray##

instance Eq VkSharedPresentSurfaceCapabilitiesKHR where
        (VkSharedPresentSurfaceCapabilitiesKHR## a) ==
          (VkSharedPresentSurfaceCapabilitiesKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkSharedPresentSurfaceCapabilitiesKHR where
        (VkSharedPresentSurfaceCapabilitiesKHR## a) `compare`
          (VkSharedPresentSurfaceCapabilitiesKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkSharedPresentSurfaceCapabilitiesKHR where
        sizeOf ~_
          = #{size VkSharedPresentSurfaceCapabilitiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkSharedPresentSurfaceCapabilitiesKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkSharedPresentSurfaceCapabilitiesKHR),
            I## a <- alignment
                      (undefined :: VkSharedPresentSurfaceCapabilitiesKHR)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkSharedPresentSurfaceCapabilitiesKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkSharedPresentSurfaceCapabilitiesKHR## ba)
          | I## n <- sizeOf
                      (undefined :: VkSharedPresentSurfaceCapabilitiesKHR)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkSharedPresentSurfaceCapabilitiesKHR where
        type StructFields VkSharedPresentSurfaceCapabilitiesKHR =
             '["sType", "pNext", "sharedPresentSupportedUsageFlags"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkSharedPresentSurfaceCapabilitiesKHR),
            I## a <- alignment
                      (undefined :: VkSharedPresentSurfaceCapabilitiesKHR)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkSharedPresentSurfaceCapabilitiesKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkSharedPresentSurfaceCapabilitiesKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkSharedPresentSurfaceCapabilitiesKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkSharedPresentSurfaceCapabilitiesKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkSharedPresentSurfaceCapabilitiesKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkSharedPresentSurfaceCapabilitiesKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkSharedPresentSurfaceCapabilitiesKHR where
        type VkSTypeMType VkSharedPresentSurfaceCapabilitiesKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkSharedPresentSurfaceCapabilitiesKHR where
        type FieldType "sType" VkSharedPresentSurfaceCapabilitiesKHR =
             VkStructureType
        type FieldOptional "sType" VkSharedPresentSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkSharedPresentSurfaceCapabilitiesKHR =
             #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, sType}

instance CanReadField "sType" VkSharedPresentSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkSharedPresentSurfaceCapabilitiesKHR where
        type VkPNextMType VkSharedPresentSurfaceCapabilitiesKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkSharedPresentSurfaceCapabilitiesKHR where
        type FieldType "pNext" VkSharedPresentSurfaceCapabilitiesKHR =
             Ptr Void
        type FieldOptional "pNext" VkSharedPresentSurfaceCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkSharedPresentSurfaceCapabilitiesKHR =
             #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, pNext}

instance CanReadField "pNext" VkSharedPresentSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance {-# OVERLAPPING #-}
         HasVkSharedPresentSupportedUsageFlags
           VkSharedPresentSurfaceCapabilitiesKHR
         where
        type VkSharedPresentSupportedUsageFlagsMType
               VkSharedPresentSurfaceCapabilitiesKHR
             = VkImageUsageFlags

        {-# NOINLINE vkSharedPresentSupportedUsageFlags #-}
        vkSharedPresentSupportedUsageFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags})

        {-# INLINE vkSharedPresentSupportedUsageFlagsByteOffset #-}
        vkSharedPresentSupportedUsageFlagsByteOffset ~_
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

        {-# INLINE readVkSharedPresentSupportedUsageFlags #-}
        readVkSharedPresentSupportedUsageFlags p
          = peekByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

        {-# INLINE writeVkSharedPresentSupportedUsageFlags #-}
        writeVkSharedPresentSupportedUsageFlags p
          = pokeByteOff p #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

instance {-# OVERLAPPING #-}
         HasField "sharedPresentSupportedUsageFlags"
           VkSharedPresentSurfaceCapabilitiesKHR
         where
        type FieldType "sharedPresentSupportedUsageFlags"
               VkSharedPresentSurfaceCapabilitiesKHR
             = VkImageUsageFlags
        type FieldOptional "sharedPresentSupportedUsageFlags"
               VkSharedPresentSurfaceCapabilitiesKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "sharedPresentSupportedUsageFlags"
               VkSharedPresentSurfaceCapabilitiesKHR
             =
             #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkSharedPresentSurfaceCapabilitiesKHR, sharedPresentSupportedUsageFlags}

instance CanReadField "sharedPresentSupportedUsageFlags"
           VkSharedPresentSurfaceCapabilitiesKHR
         where
        {-# INLINE getField #-}
        getField = vkSharedPresentSupportedUsageFlags

        {-# INLINE readField #-}
        readField = readVkSharedPresentSupportedUsageFlags

instance Show VkSharedPresentSurfaceCapabilitiesKHR where
        showsPrec d x
          = showString "VkSharedPresentSurfaceCapabilitiesKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSharedPresentSupportedUsageFlags = " .
                            showsPrec d (vkSharedPresentSupportedUsageFlags x) . showChar '}'

-- | Success codes: 'VK_SUCCESS', 'VK_SUBOPTIMAL_KHR'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_OUT_OF_DATE_KHR', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkGetSwapchainStatusKHR
--   >     ( VkDevice device
--   >     , VkSwapchainKHR swapchain
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSwapchainStatusKHR.html vkGetSwapchainStatusKHR registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainStatusKHR"
               vkGetSwapchainStatusKHR ::
               VkDevice -- ^ device
                        -> VkSwapchainKHR -- ^ swapchain
                                          -> IO VkResult

pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION ::
        (Num a, Eq a) => a

pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1

type VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1

pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: CString

pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME <-
        (is_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME -> True)
  where VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
          = _VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME

_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME #-}
_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  = Ptr "VK_KHR_shared_presentable_image\NUL"##

is_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME ::
                                                  CString -> Bool

{-# INLINE is_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME #-}
is_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  = (_VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME ==)

type VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME =
     "VK_KHR_shared_presentable_image"

pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR =
        VkStructureType 1000111000

pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR ::
        VkPresentModeKHR

pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR =
        VkPresentModeKHR 1000111000

pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR ::
        VkPresentModeKHR

pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR =
        VkPresentModeKHR 1000111001

pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR :: VkImageLayout

pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR =
        VkImageLayout 1000111000
