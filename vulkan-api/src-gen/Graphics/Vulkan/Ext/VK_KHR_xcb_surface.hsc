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
module Graphics.Vulkan.Ext.VK_KHR_xcb_surface
       (-- * Vulkan extension: @VK_KHR_xcb_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jesse Hall @jessehall,Ian Elliott ianelliott@google.com@
        --
        -- author: @KHR@
        --
        -- type: @instance@
        --
        -- Extension number: @6@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_XCB_KHR@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkXcbSurfaceCreateInfoKHR(..), vkCreateXcbSurfaceKHR,
        vkGetPhysicalDeviceXcbPresentationSupportKHR,
        VK_KHR_XCB_SURFACE_SPEC_VERSION,
        pattern VK_KHR_XCB_SURFACE_SPEC_VERSION,
        VK_KHR_XCB_SURFACE_EXTENSION_NAME,
        pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkXcbSurfaceCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkXcbSurfaceCreateFlagsKHR   flags;
--   >     xcb_connection_t*                connection;
--   >     xcb_window_t                     window;
--   > } VkXcbSurfaceCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkXcbSurfaceCreateInfoKHR.html VkXcbSurfaceCreateInfoKHR registry at www.khronos.org>
data VkXcbSurfaceCreateInfoKHR = VkXcbSurfaceCreateInfoKHR## Addr##
                                                            ByteArray##

instance Eq VkXcbSurfaceCreateInfoKHR where
        (VkXcbSurfaceCreateInfoKHR## a _) ==
          x@(VkXcbSurfaceCreateInfoKHR## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkXcbSurfaceCreateInfoKHR where
        (VkXcbSurfaceCreateInfoKHR## a _) `compare`
          x@(VkXcbSurfaceCreateInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkXcbSurfaceCreateInfoKHR where
        sizeOf ~_ = #{size VkXcbSurfaceCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkXcbSurfaceCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkXcbSurfaceCreateInfoKHR where
        unsafeAddr (VkXcbSurfaceCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkXcbSurfaceCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkXcbSurfaceCreateInfoKHR## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkXcbSurfaceCreateInfoKHR where
        type StructFields VkXcbSurfaceCreateInfoKHR =
             '["sType", "pNext", "flags", "connection", "window"] -- ' closing tick for hsc2hs
        type CUnionType VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkXcbSurfaceCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkXcbSurfaceCreateInfoKHR
         where
        type VkSTypeMType VkXcbSurfaceCreateInfoKHR = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkXcbSurfaceCreateInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkXcbSurfaceCreateInfoKHR where
        type FieldType "sType" VkXcbSurfaceCreateInfoKHR = VkStructureType
        type FieldOptional "sType" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkXcbSurfaceCreateInfoKHR =
             #{offset VkXcbSurfaceCreateInfoKHR, sType}
        type FieldIsArray "sType" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXcbSurfaceCreateInfoKHR, sType}

instance CanReadField "sType" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkXcbSurfaceCreateInfoKHR
         where
        type VkPNextMType VkXcbSurfaceCreateInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkXcbSurfaceCreateInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkXcbSurfaceCreateInfoKHR where
        type FieldType "pNext" VkXcbSurfaceCreateInfoKHR = Ptr Void
        type FieldOptional "pNext" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkXcbSurfaceCreateInfoKHR =
             #{offset VkXcbSurfaceCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXcbSurfaceCreateInfoKHR, pNext}

instance CanReadField "pNext" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkXcbSurfaceCreateInfoKHR
         where
        type VkFlagsMType VkXcbSurfaceCreateInfoKHR =
             VkXcbSurfaceCreateFlagsKHR

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkXcbSurfaceCreateInfoKHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkXcbSurfaceCreateInfoKHR where
        type FieldType "flags" VkXcbSurfaceCreateInfoKHR =
             VkXcbSurfaceCreateFlagsKHR
        type FieldOptional "flags" VkXcbSurfaceCreateInfoKHR = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkXcbSurfaceCreateInfoKHR =
             #{offset VkXcbSurfaceCreateInfoKHR, flags}
        type FieldIsArray "flags" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXcbSurfaceCreateInfoKHR, flags}

instance CanReadField "flags" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkConnection VkXcbSurfaceCreateInfoKHR where
        type VkConnectionMType VkXcbSurfaceCreateInfoKHR =
             Ptr XcbConnectionT

        {-# NOINLINE vkConnection #-}
        vkConnection x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, connection})

        {-# INLINE vkConnectionByteOffset #-}
        vkConnectionByteOffset ~_
          = #{offset VkXcbSurfaceCreateInfoKHR, connection}

        {-# INLINE readVkConnection #-}
        readVkConnection p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, connection}

        {-# INLINE writeVkConnection #-}
        writeVkConnection p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, connection}

instance {-# OVERLAPPING #-}
         HasField "connection" VkXcbSurfaceCreateInfoKHR where
        type FieldType "connection" VkXcbSurfaceCreateInfoKHR =
             Ptr XcbConnectionT
        type FieldOptional "connection" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "connection" VkXcbSurfaceCreateInfoKHR =
             #{offset VkXcbSurfaceCreateInfoKHR, connection}
        type FieldIsArray "connection" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXcbSurfaceCreateInfoKHR, connection}

instance CanReadField "connection" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkConnection

        {-# INLINE readField #-}
        readField = readVkConnection

instance CanWriteField "connection" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkConnection

instance {-# OVERLAPPING #-} HasVkWindow VkXcbSurfaceCreateInfoKHR
         where
        type VkWindowMType VkXcbSurfaceCreateInfoKHR = XcbWindowT

        {-# NOINLINE vkWindow #-}
        vkWindow x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkXcbSurfaceCreateInfoKHR, window})

        {-# INLINE vkWindowByteOffset #-}
        vkWindowByteOffset ~_
          = #{offset VkXcbSurfaceCreateInfoKHR, window}

        {-# INLINE readVkWindow #-}
        readVkWindow p
          = peekByteOff p #{offset VkXcbSurfaceCreateInfoKHR, window}

        {-# INLINE writeVkWindow #-}
        writeVkWindow p
          = pokeByteOff p #{offset VkXcbSurfaceCreateInfoKHR, window}

instance {-# OVERLAPPING #-}
         HasField "window" VkXcbSurfaceCreateInfoKHR where
        type FieldType "window" VkXcbSurfaceCreateInfoKHR = XcbWindowT
        type FieldOptional "window" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "window" VkXcbSurfaceCreateInfoKHR =
             #{offset VkXcbSurfaceCreateInfoKHR, window}
        type FieldIsArray "window" VkXcbSurfaceCreateInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkXcbSurfaceCreateInfoKHR, window}

instance CanReadField "window" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE getField #-}
        getField = vkWindow

        {-# INLINE readField #-}
        readField = readVkWindow

instance CanWriteField "window" VkXcbSurfaceCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField = writeVkWindow

instance Show VkXcbSurfaceCreateInfoKHR where
        showsPrec d x
          = showString "VkXcbSurfaceCreateInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkConnection = " .
                                  showsPrec d (vkConnection x) .
                                    showString ", " .
                                      showString "vkWindow = " .
                                        showsPrec d (vkWindow x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkCreateXcbSurfaceKHR
--   >     ( VkInstance instance
--   >     , const VkXcbSurfaceCreateInfoKHR* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateXcbSurfaceKHR.html vkCreateXcbSurfaceKHR registry at www.khronos.org>
foreign import ccall unsafe "vkCreateXcbSurfaceKHR"
               vkCreateXcbSurfaceKHR ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkXcbSurfaceCreateInfoKHR -- ^ pCreateInfo
                                               ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

-- | > VkBool32 vkGetPhysicalDeviceXcbPresentationSupportKHR
--   >     ( VkPhysicalDevice physicalDevice
--   >     , uint32_t queueFamilyIndex
--   >     , xcb_connection_t* connection
--   >     , xcb_visualid_t visual_id
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetPhysicalDeviceXcbPresentationSupportKHR.html vkGetPhysicalDeviceXcbPresentationSupportKHR registry at www.khronos.org>
foreign import ccall unsafe
               "vkGetPhysicalDeviceXcbPresentationSupportKHR"
               vkGetPhysicalDeviceXcbPresentationSupportKHR ::
               VkPhysicalDevice -- ^ physicalDevice
                                ->
                 Word32 -- ^ queueFamilyIndex
                        -> Ptr XcbConnectionT -- ^ connection
                                              -> XcbVisualidT -- ^ visual_id
                                                              -> IO VkBool32

pattern VK_KHR_XCB_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_KHR_XCB_SURFACE_SPEC_VERSION = 6

type VK_KHR_XCB_SURFACE_SPEC_VERSION = 6

pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString

pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME <-
        (is_VK_KHR_XCB_SURFACE_EXTENSION_NAME -> True)
  where VK_KHR_XCB_SURFACE_EXTENSION_NAME
          = _VK_KHR_XCB_SURFACE_EXTENSION_NAME

{-# INLINE _VK_KHR_XCB_SURFACE_EXTENSION_NAME #-}

_VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString
_VK_KHR_XCB_SURFACE_EXTENSION_NAME = Ptr "VK_KHR_xcb_surface\NUL"##

{-# INLINE is_VK_KHR_XCB_SURFACE_EXTENSION_NAME #-}

is_VK_KHR_XCB_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_KHR_XCB_SURFACE_EXTENSION_NAME
  = eqCStrings _VK_KHR_XCB_SURFACE_EXTENSION_NAME

type VK_KHR_XCB_SURFACE_EXTENSION_NAME = "VK_KHR_xcb_surface"

pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR =
        VkStructureType 1000005000
