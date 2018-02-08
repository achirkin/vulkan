#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_KHR_display_swapchain
       (-- * Vulkan extension: @VK_KHR_display_swapchain@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @James Jones @cubanismo@
        --
        -- author: @KHR@
        --
        -- type: @device@
        --
        -- Extension number: @4@
        --
        -- Required extensions: 'VK_KHR_swapchain', 'VK_KHR_display'.
        --

        -- ** Required extensions: 'VK_KHR_swapchain', 'VK_KHR_display'.
        VkDisplayPresentInfoKHR(..), vkCreateSharedSwapchainsKHR,
        VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION,
        pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION,
        VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR,
        pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..),
                                                   VkRect2D,
                                                   VkSwapchainCreateInfoKHR (..))
import           Graphics.Vulkan.Common           (VkBool32, VkDevice,
                                                   VkResult (..),
                                                   VkStructureType,
                                                   VkStructureType (..),
                                                   VkSwapchainKHR, Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

data VkDisplayPresentInfoKHR = VkDisplayPresentInfoKHR## ByteArray##

instance Eq VkDisplayPresentInfoKHR where
        (VkDisplayPresentInfoKHR## a) == (VkDisplayPresentInfoKHR## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPresentInfoKHR where
        (VkDisplayPresentInfoKHR## a) `compare` (VkDisplayPresentInfoKHR## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDisplayPresentInfoKHR where
        sizeOf ~_ = #{size VkDisplayPresentInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPresentInfoKHR}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDisplayPresentInfoKHR),
            I## a <- alignment (undefined :: VkDisplayPresentInfoKHR) =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDisplayPresentInfoKHR##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDisplayPresentInfoKHR## ba)
          | I## n <- sizeOf (undefined :: VkDisplayPresentInfoKHR) =
            IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDisplayPresentInfoKHR where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDisplayPresentInfoKHR),
            I## a <- alignment (undefined :: VkDisplayPresentInfoKHR) =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDisplayPresentInfoKHR##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDisplayPresentInfoKHR## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr = fromForeignPtr## VkDisplayPresentInfoKHR##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDisplayPresentInfoKHR## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDisplayPresentInfoKHR## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDisplayPresentInfoKHR## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-} HasVkSType VkDisplayPresentInfoKHR
         where
        type VkSTypeMType VkDisplayPresentInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDisplayPresentInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, sType}

instance {-# OVERLAPPING #-} HasVkPNext VkDisplayPresentInfoKHR
         where
        type VkPNextMType VkDisplayPresentInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDisplayPresentInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-} HasVkSrcRect VkDisplayPresentInfoKHR
         where
        type VkSrcRectMType VkDisplayPresentInfoKHR = VkRect2D

        {-# NOINLINE vkSrcRect #-}
        vkSrcRect x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, srcRect})

        {-# INLINE vkSrcRectByteOffset #-}
        vkSrcRectByteOffset ~_
          = #{offset VkDisplayPresentInfoKHR, srcRect}

        {-# INLINE readVkSrcRect #-}
        readVkSrcRect p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, srcRect}

        {-# INLINE writeVkSrcRect #-}
        writeVkSrcRect p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, srcRect}

instance {-# OVERLAPPING #-} HasVkDstRect VkDisplayPresentInfoKHR
         where
        type VkDstRectMType VkDisplayPresentInfoKHR = VkRect2D

        {-# NOINLINE vkDstRect #-}
        vkDstRect x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, dstRect})

        {-# INLINE vkDstRectByteOffset #-}
        vkDstRectByteOffset ~_
          = #{offset VkDisplayPresentInfoKHR, dstRect}

        {-# INLINE readVkDstRect #-}
        readVkDstRect p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, dstRect}

        {-# INLINE writeVkDstRect #-}
        writeVkDstRect p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, dstRect}

instance {-# OVERLAPPING #-}
         HasVkPersistent VkDisplayPresentInfoKHR where
        type VkPersistentMType VkDisplayPresentInfoKHR = VkBool32

        {-# NOINLINE vkPersistent #-}
        vkPersistent x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDisplayPresentInfoKHR, persistent})

        {-# INLINE vkPersistentByteOffset #-}
        vkPersistentByteOffset ~_
          = #{offset VkDisplayPresentInfoKHR, persistent}

        {-# INLINE readVkPersistent #-}
        readVkPersistent p
          = peekByteOff p #{offset VkDisplayPresentInfoKHR, persistent}

        {-# INLINE writeVkPersistent #-}
        writeVkPersistent p
          = pokeByteOff p #{offset VkDisplayPresentInfoKHR, persistent}

instance Show VkDisplayPresentInfoKHR where
        showsPrec d x
          = showString "VkDisplayPresentInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSrcRect = " .
                            showsPrec d (vkSrcRect x) .
                              showString ", " .
                                showString "vkDstRect = " .
                                  showsPrec d (vkDstRect x) .
                                    showString ", " .
                                      showString "vkPersistent = " .
                                        showsPrec d (vkPersistent x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_INCOMPATIBLE_DISPLAY_KHR', 'VK_ERROR_DEVICE_LOST', 'VK_ERROR_SURFACE_LOST_KHR'.
--
--   > VkResult vkCreateSharedSwapchainsKHR
--   >     ( VkDevice device
--   >     , uint32_t swapchainCount
--   >     , const VkSwapchainCreateInfoKHR* pCreateInfos
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSwapchainKHR* pSwapchains
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateSharedSwapchainsKHR.html vkCreateSharedSwapchainsKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateSharedSwapchainsKHR"
               vkCreateSharedSwapchainsKHR ::
               VkDevice -- ^ device
                        ->
                 Word32 -- ^ swapchainCount
                        ->
                   Ptr VkSwapchainCreateInfoKHR -- ^ pCreateInfos
                                                ->
                     Ptr VkAllocationCallbacks -- ^ pAllocator
                                               -> Ptr VkSwapchainKHR -- ^ pSwapchains
                                                                     -> IO VkResult

pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 9

type VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 9

pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: CString

pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME <-
        (is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME -> True)
  where VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
          = _VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME

_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: CString

{-# INLINE _VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME #-}
_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  = Ptr "VK_KHR_display_swapchain\NUL"##

is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME #-}
is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  = (_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME ==)

type VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME =
     "VK_KHR_display_swapchain"

pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR =
        VkStructureType 1000003000

pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR :: VkResult

pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR = VkResult (-1000003001)
