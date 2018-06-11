{-# OPTIONS_GHC -fno-warn-orphans#-}
{-# OPTIONS_HADDOCK not-home#-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_debug_marker
       (-- * Vulkan extension: @VK_EXT_debug_marker@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Baldur Karlsson @baldurk@
        --
        -- author: @Baldur Karlsson@
        --
        -- type: @device@
        --
        -- Extension number: @23@
        --
        -- Required extensions: 'VK_EXT_debug_report'.
        --

        -- ** Required extensions: 'VK_EXT_debug_report'.
        module Graphics.Vulkan.Marshal,
        module Graphics.Vulkan.Types.Struct.Debug,
        module Graphics.Vulkan.Types.Enum.Debug,
        module Graphics.Vulkan.Types.Enum.StructureType,
        -- > #include "vk_platform.h"
        VkDebugMarkerSetObjectTagEXT, pattern VkDebugMarkerSetObjectTagEXT,
        HS_vkDebugMarkerSetObjectTagEXT, PFN_vkDebugMarkerSetObjectTagEXT,
        VkDebugMarkerSetObjectNameEXT,
        pattern VkDebugMarkerSetObjectNameEXT,
        HS_vkDebugMarkerSetObjectNameEXT,
        PFN_vkDebugMarkerSetObjectNameEXT, VkCmdDebugMarkerBeginEXT,
        pattern VkCmdDebugMarkerBeginEXT, HS_vkCmdDebugMarkerBeginEXT,
        PFN_vkCmdDebugMarkerBeginEXT, VkCmdDebugMarkerEndEXT,
        pattern VkCmdDebugMarkerEndEXT, HS_vkCmdDebugMarkerEndEXT,
        PFN_vkCmdDebugMarkerEndEXT, VkCmdDebugMarkerInsertEXT,
        pattern VkCmdDebugMarkerInsertEXT, HS_vkCmdDebugMarkerInsertEXT,
        PFN_vkCmdDebugMarkerInsertEXT,
        module Graphics.Vulkan.Types.Enum.Result,
        module Graphics.Vulkan.Types.Handles,
        VK_EXT_DEBUG_MARKER_SPEC_VERSION,
        pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION,
        VK_EXT_DEBUG_MARKER_EXTENSION_NAME,
        pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT)
       where
import           GHC.Ptr                                  (Ptr (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Proc             (VulkanProc (..))
import           Graphics.Vulkan.Types.Enum.Debug
import           Graphics.Vulkan.Types.Enum.Result
import           Graphics.Vulkan.Types.Enum.StructureType
import           Graphics.Vulkan.Types.Handles
import           Graphics.Vulkan.Types.Struct.Debug

pattern VkDebugMarkerSetObjectTagEXT :: CString

pattern VkDebugMarkerSetObjectTagEXT <-
        (is_VkDebugMarkerSetObjectTagEXT -> True)
  where VkDebugMarkerSetObjectTagEXT = _VkDebugMarkerSetObjectTagEXT

{-# INLINE _VkDebugMarkerSetObjectTagEXT #-}

_VkDebugMarkerSetObjectTagEXT :: CString
_VkDebugMarkerSetObjectTagEXT
  = Ptr "vkDebugMarkerSetObjectTagEXT\NUL"#

{-# INLINE is_VkDebugMarkerSetObjectTagEXT #-}

is_VkDebugMarkerSetObjectTagEXT :: CString -> Bool
is_VkDebugMarkerSetObjectTagEXT
  = (EQ ==) . cmpCStrings _VkDebugMarkerSetObjectTagEXT

type VkDebugMarkerSetObjectTagEXT = "vkDebugMarkerSetObjectTagEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDebugMarkerSetObjectTagEXT
--   >     ( VkDevice device
--   >     , const VkDebugMarkerObjectTagInfoEXT* pTagInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDebugMarkerSetObjectTagEXT vkDebugMarkerSetObjectTagEXT registry at www.khronos.org>
type HS_vkDebugMarkerSetObjectTagEXT =
     VkDevice -- ^ device
              -> Ptr VkDebugMarkerObjectTagInfoEXT -- ^ pTagInfo
                                                   -> IO VkResult

type PFN_vkDebugMarkerSetObjectTagEXT =
     FunPtr HS_vkDebugMarkerSetObjectTagEXT

foreign import ccall unsafe "dynamic"
               unwrapVkDebugMarkerSetObjectTagEXTUnsafe ::
               PFN_vkDebugMarkerSetObjectTagEXT -> HS_vkDebugMarkerSetObjectTagEXT

foreign import ccall safe "dynamic"
               unwrapVkDebugMarkerSetObjectTagEXTSafe ::
               PFN_vkDebugMarkerSetObjectTagEXT -> HS_vkDebugMarkerSetObjectTagEXT

instance VulkanProc "vkDebugMarkerSetObjectTagEXT" where
        type VkProcType "vkDebugMarkerSetObjectTagEXT" =
             HS_vkDebugMarkerSetObjectTagEXT
        vkProcSymbol = _VkDebugMarkerSetObjectTagEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkDebugMarkerSetObjectTagEXTUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkDebugMarkerSetObjectTagEXTSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkDebugMarkerSetObjectNameEXT :: CString

pattern VkDebugMarkerSetObjectNameEXT <-
        (is_VkDebugMarkerSetObjectNameEXT -> True)
  where VkDebugMarkerSetObjectNameEXT
          = _VkDebugMarkerSetObjectNameEXT

{-# INLINE _VkDebugMarkerSetObjectNameEXT #-}

_VkDebugMarkerSetObjectNameEXT :: CString
_VkDebugMarkerSetObjectNameEXT
  = Ptr "vkDebugMarkerSetObjectNameEXT\NUL"#

{-# INLINE is_VkDebugMarkerSetObjectNameEXT #-}

is_VkDebugMarkerSetObjectNameEXT :: CString -> Bool
is_VkDebugMarkerSetObjectNameEXT
  = (EQ ==) . cmpCStrings _VkDebugMarkerSetObjectNameEXT

type VkDebugMarkerSetObjectNameEXT =
     "vkDebugMarkerSetObjectNameEXT"

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY'.
--
--   > VkResult vkDebugMarkerSetObjectNameEXT
--   >     ( VkDevice device
--   >     , const VkDebugMarkerObjectNameInfoEXT* pNameInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkDebugMarkerSetObjectNameEXT vkDebugMarkerSetObjectNameEXT registry at www.khronos.org>
type HS_vkDebugMarkerSetObjectNameEXT =
     VkDevice -- ^ device
              -> Ptr VkDebugMarkerObjectNameInfoEXT -- ^ pNameInfo
                                                    -> IO VkResult

type PFN_vkDebugMarkerSetObjectNameEXT =
     FunPtr HS_vkDebugMarkerSetObjectNameEXT

foreign import ccall unsafe "dynamic"
               unwrapVkDebugMarkerSetObjectNameEXTUnsafe ::
               PFN_vkDebugMarkerSetObjectNameEXT ->
                 HS_vkDebugMarkerSetObjectNameEXT

foreign import ccall safe "dynamic"
               unwrapVkDebugMarkerSetObjectNameEXTSafe ::
               PFN_vkDebugMarkerSetObjectNameEXT ->
                 HS_vkDebugMarkerSetObjectNameEXT

instance VulkanProc "vkDebugMarkerSetObjectNameEXT" where
        type VkProcType "vkDebugMarkerSetObjectNameEXT" =
             HS_vkDebugMarkerSetObjectNameEXT
        vkProcSymbol = _VkDebugMarkerSetObjectNameEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkDebugMarkerSetObjectNameEXTUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkDebugMarkerSetObjectNameEXTSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdDebugMarkerBeginEXT :: CString

pattern VkCmdDebugMarkerBeginEXT <-
        (is_VkCmdDebugMarkerBeginEXT -> True)
  where VkCmdDebugMarkerBeginEXT = _VkCmdDebugMarkerBeginEXT

{-# INLINE _VkCmdDebugMarkerBeginEXT #-}

_VkCmdDebugMarkerBeginEXT :: CString
_VkCmdDebugMarkerBeginEXT = Ptr "vkCmdDebugMarkerBeginEXT\NUL"#

{-# INLINE is_VkCmdDebugMarkerBeginEXT #-}

is_VkCmdDebugMarkerBeginEXT :: CString -> Bool
is_VkCmdDebugMarkerBeginEXT
  = (EQ ==) . cmpCStrings _VkCmdDebugMarkerBeginEXT

type VkCmdDebugMarkerBeginEXT = "vkCmdDebugMarkerBeginEXT"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdDebugMarkerBeginEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugMarkerMarkerInfoEXT* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDebugMarkerBeginEXT vkCmdDebugMarkerBeginEXT registry at www.khronos.org>
type HS_vkCmdDebugMarkerBeginEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkDebugMarkerMarkerInfoEXT -- ^ pMarkerInfo
                                                       -> IO ()

type PFN_vkCmdDebugMarkerBeginEXT =
     FunPtr HS_vkCmdDebugMarkerBeginEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDebugMarkerBeginEXTUnsafe ::
               PFN_vkCmdDebugMarkerBeginEXT -> HS_vkCmdDebugMarkerBeginEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdDebugMarkerBeginEXTSafe ::
               PFN_vkCmdDebugMarkerBeginEXT -> HS_vkCmdDebugMarkerBeginEXT

instance VulkanProc "vkCmdDebugMarkerBeginEXT" where
        type VkProcType "vkCmdDebugMarkerBeginEXT" =
             HS_vkCmdDebugMarkerBeginEXT
        vkProcSymbol = _VkCmdDebugMarkerBeginEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCmdDebugMarkerBeginEXTUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDebugMarkerBeginEXTSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdDebugMarkerEndEXT :: CString

pattern VkCmdDebugMarkerEndEXT <-
        (is_VkCmdDebugMarkerEndEXT -> True)
  where VkCmdDebugMarkerEndEXT = _VkCmdDebugMarkerEndEXT

{-# INLINE _VkCmdDebugMarkerEndEXT #-}

_VkCmdDebugMarkerEndEXT :: CString
_VkCmdDebugMarkerEndEXT = Ptr "vkCmdDebugMarkerEndEXT\NUL"#

{-# INLINE is_VkCmdDebugMarkerEndEXT #-}

is_VkCmdDebugMarkerEndEXT :: CString -> Bool
is_VkCmdDebugMarkerEndEXT
  = (EQ ==) . cmpCStrings _VkCmdDebugMarkerEndEXT

type VkCmdDebugMarkerEndEXT = "vkCmdDebugMarkerEndEXT"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdDebugMarkerEndEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDebugMarkerEndEXT vkCmdDebugMarkerEndEXT registry at www.khronos.org>
type HS_vkCmdDebugMarkerEndEXT = VkCommandBuffer -- ^ commandBuffer
                                                 -> IO ()

type PFN_vkCmdDebugMarkerEndEXT = FunPtr HS_vkCmdDebugMarkerEndEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDebugMarkerEndEXTUnsafe ::
               PFN_vkCmdDebugMarkerEndEXT -> HS_vkCmdDebugMarkerEndEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdDebugMarkerEndEXTSafe ::
               PFN_vkCmdDebugMarkerEndEXT -> HS_vkCmdDebugMarkerEndEXT

instance VulkanProc "vkCmdDebugMarkerEndEXT" where
        type VkProcType "vkCmdDebugMarkerEndEXT" =
             HS_vkCmdDebugMarkerEndEXT
        vkProcSymbol = _VkCmdDebugMarkerEndEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCmdDebugMarkerEndEXTUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDebugMarkerEndEXTSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VkCmdDebugMarkerInsertEXT :: CString

pattern VkCmdDebugMarkerInsertEXT <-
        (is_VkCmdDebugMarkerInsertEXT -> True)
  where VkCmdDebugMarkerInsertEXT = _VkCmdDebugMarkerInsertEXT

{-# INLINE _VkCmdDebugMarkerInsertEXT #-}

_VkCmdDebugMarkerInsertEXT :: CString
_VkCmdDebugMarkerInsertEXT = Ptr "vkCmdDebugMarkerInsertEXT\NUL"#

{-# INLINE is_VkCmdDebugMarkerInsertEXT #-}

is_VkCmdDebugMarkerInsertEXT :: CString -> Bool
is_VkCmdDebugMarkerInsertEXT
  = (EQ ==) . cmpCStrings _VkCmdDebugMarkerInsertEXT

type VkCmdDebugMarkerInsertEXT = "vkCmdDebugMarkerInsertEXT"

-- | Queues: 'graphics', 'compute'.
--
--   Renderpass: @both@
--
--   > void vkCmdDebugMarkerInsertEXT
--   >     ( VkCommandBuffer commandBuffer
--   >     , const VkDebugMarkerMarkerInfoEXT* pMarkerInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkCmdDebugMarkerInsertEXT vkCmdDebugMarkerInsertEXT registry at www.khronos.org>
type HS_vkCmdDebugMarkerInsertEXT =
     VkCommandBuffer -- ^ commandBuffer
                     -> Ptr VkDebugMarkerMarkerInfoEXT -- ^ pMarkerInfo
                                                       -> IO ()

type PFN_vkCmdDebugMarkerInsertEXT =
     FunPtr HS_vkCmdDebugMarkerInsertEXT

foreign import ccall unsafe "dynamic"
               unwrapVkCmdDebugMarkerInsertEXTUnsafe ::
               PFN_vkCmdDebugMarkerInsertEXT -> HS_vkCmdDebugMarkerInsertEXT

foreign import ccall safe "dynamic"
               unwrapVkCmdDebugMarkerInsertEXTSafe ::
               PFN_vkCmdDebugMarkerInsertEXT -> HS_vkCmdDebugMarkerInsertEXT

instance VulkanProc "vkCmdDebugMarkerInsertEXT" where
        type VkProcType "vkCmdDebugMarkerInsertEXT" =
             HS_vkCmdDebugMarkerInsertEXT
        vkProcSymbol = _VkCmdDebugMarkerInsertEXT

        {-# INLINE vkProcSymbol #-}
        unwrapVkProcPtrUnsafe = unwrapVkCmdDebugMarkerInsertEXTUnsafe

        {-# INLINE unwrapVkProcPtrUnsafe #-}
        unwrapVkProcPtrSafe = unwrapVkCmdDebugMarkerInsertEXTSafe

        {-# INLINE unwrapVkProcPtrSafe #-}

pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4

type VK_EXT_DEBUG_MARKER_SPEC_VERSION = 4

pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString

pattern VK_EXT_DEBUG_MARKER_EXTENSION_NAME <-
        (is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME -> True)
  where VK_EXT_DEBUG_MARKER_EXTENSION_NAME
          = _VK_EXT_DEBUG_MARKER_EXTENSION_NAME

{-# INLINE _VK_EXT_DEBUG_MARKER_EXTENSION_NAME #-}

_VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString
_VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  = Ptr "VK_EXT_debug_marker\NUL"#

{-# INLINE is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME #-}

is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DEBUG_MARKER_EXTENSION_NAME
  = (EQ ==) . cmpCStrings _VK_EXT_DEBUG_MARKER_EXTENSION_NAME

type VK_EXT_DEBUG_MARKER_EXTENSION_NAME = "VK_EXT_debug_marker"

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT =
        VkStructureType 1000022000

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT =
        VkStructureType 1000022001

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT =
        VkStructureType 1000022002
