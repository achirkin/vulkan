#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
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
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..),
                                                   VkPresentInfoKHR, VkRect2D,
                                                   VkSwapchainCreateInfoKHR (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDisplayPresentInfoKHR {
--   >     VkStructureType sType;
--   >     const void*  pNext;
--   >     VkRect2D                         srcRect;
--   >     VkRect2D                         dstRect;
--   >     VkBool32                         persistent;
--   > } VkDisplayPresentInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDisplayPresentInfoKHR.html VkDisplayPresentInfoKHR registry at www.khronos.org>
data VkDisplayPresentInfoKHR = VkDisplayPresentInfoKHR## Addr##
                                                        ByteArray##

instance Eq VkDisplayPresentInfoKHR where
        (VkDisplayPresentInfoKHR## a _) == x@(VkDisplayPresentInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDisplayPresentInfoKHR where
        (VkDisplayPresentInfoKHR## a _) `compare`
          x@(VkDisplayPresentInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDisplayPresentInfoKHR where
        sizeOf ~_ = #{size VkDisplayPresentInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDisplayPresentInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDisplayPresentInfoKHR where
        unsafeAddr (VkDisplayPresentInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDisplayPresentInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDisplayPresentInfoKHR## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDisplayPresentInfoKHR where
        type StructFields VkDisplayPresentInfoKHR =
             '["sType", "pNext", "srcRect", "dstRect", "persistent"] -- ' closing tick for hsc2hs
        type CUnionType VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDisplayPresentInfoKHR = '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         HasField "sType" VkDisplayPresentInfoKHR where
        type FieldType "sType" VkDisplayPresentInfoKHR = VkStructureType
        type FieldOptional "sType" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPresentInfoKHR, sType}

instance CanReadField "sType" VkDisplayPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDisplayPresentInfoKHR where
        type FieldType "pNext" VkDisplayPresentInfoKHR = Ptr Void
        type FieldOptional "pNext" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDisplayPresentInfoKHR, pNext}

instance CanReadField "pNext" VkDisplayPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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

instance {-# OVERLAPPING #-}
         HasField "srcRect" VkDisplayPresentInfoKHR where
        type FieldType "srcRect" VkDisplayPresentInfoKHR = VkRect2D
        type FieldOptional "srcRect" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcRect" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, srcRect}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPresentInfoKHR, srcRect}

instance CanReadField "srcRect" VkDisplayPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkSrcRect

        {-# INLINE readField #-}
        readField = readVkSrcRect

instance CanWriteField "srcRect" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSrcRect

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
         HasField "dstRect" VkDisplayPresentInfoKHR where
        type FieldType "dstRect" VkDisplayPresentInfoKHR = VkRect2D
        type FieldOptional "dstRect" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstRect" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, dstRect}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPresentInfoKHR, dstRect}

instance CanReadField "dstRect" VkDisplayPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkDstRect

        {-# INLINE readField #-}
        readField = readVkDstRect

instance CanWriteField "dstRect" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkDstRect

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

instance {-# OVERLAPPING #-}
         HasField "persistent" VkDisplayPresentInfoKHR where
        type FieldType "persistent" VkDisplayPresentInfoKHR = VkBool32
        type FieldOptional "persistent" VkDisplayPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "persistent" VkDisplayPresentInfoKHR =
             #{offset VkDisplayPresentInfoKHR, persistent}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDisplayPresentInfoKHR, persistent}

instance CanReadField "persistent" VkDisplayPresentInfoKHR where
        {-# INLINE getField #-}
        getField = vkPersistent

        {-# INLINE readField #-}
        readField = readVkPersistent

instance CanWriteField "persistent" VkDisplayPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPersistent

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

{-# INLINE _VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME #-}

_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: CString
_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  = Ptr "VK_KHR_display_swapchain\NUL"##

{-# INLINE is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME #-}

is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  = eqCStrings _VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME

type VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME =
     "VK_KHR_display_swapchain"

pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR =
        VkStructureType 1000003000

pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR :: VkResult

pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR = VkResult (-1000003001)
