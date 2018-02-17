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
module Graphics.Vulkan.Ext.VK_MVK_macos_surface
       (-- * Vulkan extension: @VK_MVK_macos_surface@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Bill Hollings @billhollings@
        --
        -- author: @MVK@
        --
        -- type: @instance@
        --
        -- Extension number: @124@
        --
        -- Required extensions: 'VK_KHR_surface'.
        --
        -- Protected by CPP ifdef: @VK_USE_PLATFORM_MACOS_MVK@
        --

        -- ** Required extensions: 'VK_KHR_surface'.
        VkMacOSSurfaceCreateInfoMVK(..), vkCreateMacOSSurfaceMVK,
        VK_MVK_MACOS_SURFACE_SPEC_VERSION,
        pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION,
        VK_MVK_MACOS_SURFACE_EXTENSION_NAME,
        pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK)
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

-- | > typedef struct VkMacOSSurfaceCreateInfoMVK {
--   >     VkStructureType sType;
--   >     const void*                                    pNext;
--   >     VkMacOSSurfaceCreateFlagsMVK   flags;
--   >     const void*                                    pView;
--   > } VkMacOSSurfaceCreateInfoMVK;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkMacOSSurfaceCreateInfoMVK.html VkMacOSSurfaceCreateInfoMVK registry at www.khronos.org>
data VkMacOSSurfaceCreateInfoMVK = VkMacOSSurfaceCreateInfoMVK## Addr##
                                                                ByteArray##

instance Eq VkMacOSSurfaceCreateInfoMVK where
        (VkMacOSSurfaceCreateInfoMVK## a _) ==
          x@(VkMacOSSurfaceCreateInfoMVK## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkMacOSSurfaceCreateInfoMVK where
        (VkMacOSSurfaceCreateInfoMVK## a _) `compare`
          x@(VkMacOSSurfaceCreateInfoMVK## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkMacOSSurfaceCreateInfoMVK where
        sizeOf ~_ = #{size VkMacOSSurfaceCreateInfoMVK}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkMacOSSurfaceCreateInfoMVK}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkMacOSSurfaceCreateInfoMVK where
        unsafeAddr (VkMacOSSurfaceCreateInfoMVK## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkMacOSSurfaceCreateInfoMVK## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkMacOSSurfaceCreateInfoMVK##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkMacOSSurfaceCreateInfoMVK where
        type StructFields VkMacOSSurfaceCreateInfoMVK =
             '["sType", "pNext", "flags", "pView"] -- ' closing tick for hsc2hs
        type CUnionType VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type StructExtends VkMacOSSurfaceCreateInfoMVK = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkMacOSSurfaceCreateInfoMVK
         where
        type VkSTypeMType VkMacOSSurfaceCreateInfoMVK = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkMacOSSurfaceCreateInfoMVK, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkMacOSSurfaceCreateInfoMVK where
        type FieldType "sType" VkMacOSSurfaceCreateInfoMVK =
             VkStructureType
        type FieldOptional "sType" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkMacOSSurfaceCreateInfoMVK =
             #{offset VkMacOSSurfaceCreateInfoMVK, sType}
        type FieldIsArray "sType" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMacOSSurfaceCreateInfoMVK, sType}

instance CanReadField "sType" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkMacOSSurfaceCreateInfoMVK
         where
        type VkPNextMType VkMacOSSurfaceCreateInfoMVK = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkMacOSSurfaceCreateInfoMVK, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkMacOSSurfaceCreateInfoMVK where
        type FieldType "pNext" VkMacOSSurfaceCreateInfoMVK = Ptr Void
        type FieldOptional "pNext" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkMacOSSurfaceCreateInfoMVK =
             #{offset VkMacOSSurfaceCreateInfoMVK, pNext}
        type FieldIsArray "pNext" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMacOSSurfaceCreateInfoMVK, pNext}

instance CanReadField "pNext" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkMacOSSurfaceCreateInfoMVK
         where
        type VkFlagsMType VkMacOSSurfaceCreateInfoMVK =
             VkMacOSSurfaceCreateFlagsMVK

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkMacOSSurfaceCreateInfoMVK, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkMacOSSurfaceCreateInfoMVK where
        type FieldType "flags" VkMacOSSurfaceCreateInfoMVK =
             VkMacOSSurfaceCreateFlagsMVK
        type FieldOptional "flags" VkMacOSSurfaceCreateInfoMVK = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkMacOSSurfaceCreateInfoMVK =
             #{offset VkMacOSSurfaceCreateInfoMVK, flags}
        type FieldIsArray "flags" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMacOSSurfaceCreateInfoMVK, flags}

instance CanReadField "flags" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-} HasVkPView VkMacOSSurfaceCreateInfoMVK
         where
        type VkPViewMType VkMacOSSurfaceCreateInfoMVK = Ptr Void

        {-# NOINLINE vkPView #-}
        vkPView x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkMacOSSurfaceCreateInfoMVK, pView})

        {-# INLINE vkPViewByteOffset #-}
        vkPViewByteOffset ~_
          = #{offset VkMacOSSurfaceCreateInfoMVK, pView}

        {-# INLINE readVkPView #-}
        readVkPView p
          = peekByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pView}

        {-# INLINE writeVkPView #-}
        writeVkPView p
          = pokeByteOff p #{offset VkMacOSSurfaceCreateInfoMVK, pView}

instance {-# OVERLAPPING #-}
         HasField "pView" VkMacOSSurfaceCreateInfoMVK where
        type FieldType "pView" VkMacOSSurfaceCreateInfoMVK = Ptr Void
        type FieldOptional "pView" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pView" VkMacOSSurfaceCreateInfoMVK =
             #{offset VkMacOSSurfaceCreateInfoMVK, pView}
        type FieldIsArray "pView" VkMacOSSurfaceCreateInfoMVK = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkMacOSSurfaceCreateInfoMVK, pView}

instance CanReadField "pView" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE getField #-}
        getField = vkPView

        {-# INLINE readField #-}
        readField = readVkPView

instance CanWriteField "pView" VkMacOSSurfaceCreateInfoMVK where
        {-# INLINE writeField #-}
        writeField = writeVkPView

instance Show VkMacOSSurfaceCreateInfoMVK where
        showsPrec d x
          = showString "VkMacOSSurfaceCreateInfoMVK {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkPView = " . showsPrec d (vkPView x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY', 'VK_ERROR_OUT_OF_DEVICE_MEMORY', 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'.
--
--   > VkResult vkCreateMacOSSurfaceMVK
--   >     ( VkInstance instance
--   >     , const VkMacOSSurfaceCreateInfoMVK* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkSurfaceKHR* pSurface
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateMacOSSurfaceMVK.html vkCreateMacOSSurfaceMVK registry at www.khronos.org>
foreign import ccall unsafe "vkCreateMacOSSurfaceMVK"
               vkCreateMacOSSurfaceMVK ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkMacOSSurfaceCreateInfoMVK -- ^ pCreateInfo
                                                 ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             -> Ptr VkSurfaceKHR -- ^ pSurface
                                                                 -> IO VkResult

pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION = 2

type VK_MVK_MACOS_SURFACE_SPEC_VERSION = 2

pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString

pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME <-
        (is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME -> True)
  where VK_MVK_MACOS_SURFACE_EXTENSION_NAME
          = _VK_MVK_MACOS_SURFACE_EXTENSION_NAME

{-# INLINE _VK_MVK_MACOS_SURFACE_EXTENSION_NAME #-}

_VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString
_VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  = Ptr "VK_MVK_macos_surface\NUL"##

{-# INLINE is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME #-}

is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME :: CString -> Bool
is_VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  = eqCStrings _VK_MVK_MACOS_SURFACE_EXTENSION_NAME

type VK_MVK_MACOS_SURFACE_EXTENSION_NAME = "VK_MVK_macos_surface"

pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK =
        VkStructureType 1000123000
