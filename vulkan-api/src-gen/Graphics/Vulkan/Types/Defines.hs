{-# LANGUAGE CPP             #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Strict          #-}
module Graphics.Vulkan.Types.Defines
       (AHardwareBuffer(), ANativeWindow(), -- | ===== @VK_API_VERSION@
                                            -- > // DEPRECATED: This define has been removed. Specific version defines (e.g. VK_API_VERSION_1_0), or the VK_MAKE_VERSION macro, should be used instead.
                                            -- > //#define VK_API_VERSION VK_MAKE_VERSION(1, 0, 0) // Patch version should always be set to 0
                                            VK_API_VERSION_1_0,
        pattern VK_API_VERSION_1_0, VK_API_VERSION_1_1,
        pattern VK_API_VERSION_1_1, Ptr(), -- | ===== @VK_DEFINE_HANDLE@
                                           -- Dispatchable handles are represented as `Foreign.Ptr`
                                           --
                                           -- >
                                           -- > #define VK_DEFINE_HANDLE(object) typedef struct object##_T* object;
                                           VkPtr(..), -- | ===== @VK_DEFINE_NON_DISPATCHABLE_HANDLE@
                                                      -- Non-dispatchable handles are represented as `VkPtr`
                                                      --
                                                      -- >
                                                      -- > #if !defined(VK_DEFINE_NON_DISPATCHABLE_HANDLE)
                                                      -- > #if defined(__LP64__) || defined(_WIN64) || (defined(__x86_64__) && !defined(__ILP32__) ) || defined(_M_X64) || defined(__ia64) || defined (_M_IA64) || defined(__aarch64__) || defined(__powerpc64__)
                                                      -- >         #define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef struct object##_T *object;
                                                      -- > #else
                                                      -- >         #define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object) typedef uint64_t object;
                                                      -- > #endif
                                                      -- > #endif
                                                      -- >
                                                      VK_HEADER_VERSION,
        pattern VK_HEADER_VERSION, _VK_MAKE_VERSION, VulkanPtr(..),
        pattern VK_NULL_HANDLE, _VK_VERSION_MAJOR, _VK_VERSION_MINOR,
        _VK_VERSION_PATCH)
       where
import           Data.Bits               (Bits (..))
import           Graphics.Vulkan.Marshal

-- | > struct AHardwareBuffer;
data AHardwareBuffer

-- | > struct ANativeWindow;
data ANativeWindow

-- | > // Vulkan 1.0 version number
--   > #define VK_API_VERSION_1_0 VK_MAKE_VERSION(1, 0, 0)// Patch version should always be set to 0
pattern VK_API_VERSION_1_0 :: (Num a, Eq a) => a

pattern VK_API_VERSION_1_0 = 4194304

type VK_API_VERSION_1_0 = 4194304

-- | > // Vulkan 1.1 version number
--   > #define VK_API_VERSION_1_1 VK_MAKE_VERSION(1, 1, 0)// Patch version should always be set to 0
pattern VK_API_VERSION_1_1 :: (Num a, Eq a) => a

pattern VK_API_VERSION_1_1 = 4198400

type VK_API_VERSION_1_1 = 4198400

-- | > // Version of this file
--   > #define VK_HEADER_VERSION 77
pattern VK_HEADER_VERSION :: (Num a, Eq a) => a

pattern VK_HEADER_VERSION = 77

type VK_HEADER_VERSION = 77

-- | > #define VK_MAKE_VERSION(major, minor, patch) \
--   >     (((major) << 22) | ((minor) << 12) | (patch))
_VK_MAKE_VERSION :: Bits a => a -> a -> a -> a
_VK_MAKE_VERSION major minor patch
  = unsafeShiftL major 22 .|. unsafeShiftL minor 12 .|. patch

{-# INLINE _VK_MAKE_VERSION #-}
#define VK_MAKE_VERSION(major, minor, patch) _VK_MAKE_VERSION major minor patch

-- | > #define VK_VERSION_MAJOR(version) ((uint32_t)(version) >> 22)
_VK_VERSION_MAJOR :: Bits a => a -> a
_VK_VERSION_MAJOR version = unsafeShiftR version 22

{-# INLINE _VK_VERSION_MAJOR #-}
#define VK_VERSION_MAJOR(version) _VK_VERSION_MAJOR version

-- | > #define VK_VERSION_MINOR(version) (((uint32_t)(version) >> 12) & 0x3ff)
_VK_VERSION_MINOR :: (Bits a, Num a) => a -> a
_VK_VERSION_MINOR version = unsafeShiftR version 12 .&. 1023

{-# INLINE _VK_VERSION_MINOR #-}
#define VK_VERSION_MINOR(version) _VK_VERSION_MINOR version

-- | > #define VK_VERSION_PATCH(version) ((uint32_t)(version) & 0xfff)
_VK_VERSION_PATCH :: (Bits a, Num a) => a -> a
_VK_VERSION_PATCH = (.&. 4095)

{-# INLINE _VK_VERSION_PATCH #-}
