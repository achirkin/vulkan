#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UnboxedTuples            #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_EXT_debug_report
       (-- * Vulkan extension: @VK_EXT_debug_report@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Courtney Goeltzenleuchter @courtney@
        --
        -- author: @GOOGLE@
        --
        -- type: @instance@
        --
        -- Extension number: @12@
        VkDebugReportCallbackCreateInfoEXT(..),
        vkCreateDebugReportCallbackEXT, vkDestroyDebugReportCallbackEXT,
        vkDebugReportMessageEXT, VK_EXT_DEBUG_REPORT_SPEC_VERSION,
        pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION,
        VK_EXT_DEBUG_REPORT_EXTENSION_NAME,
        pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT,
        pattern VK_ERROR_VALIDATION_FAILED_EXT,
        pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT,
        pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT,
        pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDebugReportCallbackCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDebugReportFlagsEXT            flags;
--   >     PFN_vkDebugReportCallbackEXT     pfnCallback;
--   >     void*            pUserData;
--   > } VkDebugReportCallbackCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkDebugReportCallbackCreateInfoEXT.html VkDebugReportCallbackCreateInfoEXT registry at www.khronos.org>
data VkDebugReportCallbackCreateInfoEXT = VkDebugReportCallbackCreateInfoEXT## ByteArray##

instance Eq VkDebugReportCallbackCreateInfoEXT where
        (VkDebugReportCallbackCreateInfoEXT## a) ==
          (VkDebugReportCallbackCreateInfoEXT## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkDebugReportCallbackCreateInfoEXT where
        (VkDebugReportCallbackCreateInfoEXT## a) `compare`
          (VkDebugReportCallbackCreateInfoEXT## b) = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkDebugReportCallbackCreateInfoEXT where
        sizeOf ~_ = #{size VkDebugReportCallbackCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugReportCallbackCreateInfoEXT}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf (undefined :: VkDebugReportCallbackCreateInfoEXT),
            I## a <- alignment (undefined :: VkDebugReportCallbackCreateInfoEXT)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkDebugReportCallbackCreateInfoEXT##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr) (VkDebugReportCallbackCreateInfoEXT## ba)
          | I## n <- sizeOf (undefined :: VkDebugReportCallbackCreateInfoEXT)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal VkDebugReportCallbackCreateInfoEXT where
        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf (undefined :: VkDebugReportCallbackCreateInfoEXT),
            I## a <- alignment (undefined :: VkDebugReportCallbackCreateInfoEXT)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkDebugReportCallbackCreateInfoEXT##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkDebugReportCallbackCreateInfoEXT## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr## VkDebugReportCallbackCreateInfoEXT##

        {-# INLINE toForeignPtr #-}
        toForeignPtr (VkDebugReportCallbackCreateInfoEXT## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr (VkDebugReportCallbackCreateInfoEXT## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData x@(VkDebugReportCallbackCreateInfoEXT## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkDebugReportCallbackCreateInfoEXT where
        type VkSTypeMType VkDebugReportCallbackCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDebugReportCallbackCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasVkPNext VkDebugReportCallbackCreateInfoEXT where
        type VkPNextMType VkDebugReportCallbackCreateInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasVkFlags VkDebugReportCallbackCreateInfoEXT where
        type VkFlagsMType VkDebugReportCallbackCreateInfoEXT =
             VkDebugReportFlagsEXT

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDebugReportCallbackCreateInfoEXT, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasVkPfnCallback VkDebugReportCallbackCreateInfoEXT where
        type VkPfnCallbackMType VkDebugReportCallbackCreateInfoEXT =
             PFN_vkDebugReportCallbackEXT

        {-# NOINLINE vkPfnCallback #-}
        vkPfnCallback x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback})

        {-# INLINE vkPfnCallbackByteOffset #-}
        vkPfnCallbackByteOffset ~_
          = #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

        {-# INLINE readVkPfnCallback #-}
        readVkPfnCallback p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

        {-# INLINE writeVkPfnCallback #-}
        writeVkPfnCallback p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

instance {-# OVERLAPPING #-}
         HasVkPUserData VkDebugReportCallbackCreateInfoEXT where
        type VkPUserDataMType VkDebugReportCallbackCreateInfoEXT = Ptr Void

        {-# NOINLINE vkPUserData #-}
        vkPUserData x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDebugReportCallbackCreateInfoEXT, pUserData})

        {-# INLINE vkPUserDataByteOffset #-}
        vkPUserDataByteOffset ~_
          = #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

        {-# INLINE readVkPUserData #-}
        readVkPUserData p
          = peekByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

        {-# INLINE writeVkPUserData #-}
        writeVkPUserData p
          = pokeByteOff p #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

instance Show VkDebugReportCallbackCreateInfoEXT where
        showsPrec d x
          = showString "VkDebugReportCallbackCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkPfnCallback = " .
                                  showsPrec d (vkPfnCallback x) .
                                    showString ", " .
                                      showString "vkPUserData = " .
                                        showsPrec d (vkPUserData x) . showChar '}'

-- | Success codes: 'VK_SUCCESS'.
--
--   Error codes: 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkCreateDebugReportCallbackEXT
--   >     ( VkInstance instance
--   >     , const VkDebugReportCallbackCreateInfoEXT* pCreateInfo
--   >     , const VkAllocationCallbacks* pAllocator
--   >     , VkDebugReportCallbackEXT* pCallback
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCreateDebugReportCallbackEXT.html vkCreateDebugReportCallbackEXT registry at www.khronos.org>
foreign import ccall unsafe "vkCreateDebugReportCallbackEXT"
               vkCreateDebugReportCallbackEXT ::
               VkInstance -- ^ instance
                          ->
                 Ptr VkDebugReportCallbackCreateInfoEXT -- ^ pCreateInfo
                                                        ->
                   Ptr VkAllocationCallbacks -- ^ pAllocator
                                             ->
                     Ptr VkDebugReportCallbackEXT -- ^ pCallback
                                                  -> IO VkResult

-- | > void vkDestroyDebugReportCallbackEXT
--   >     ( VkInstance instance
--   >     , VkDebugReportCallbackEXT callback
--   >     , const VkAllocationCallbacks* pAllocator
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDestroyDebugReportCallbackEXT.html vkDestroyDebugReportCallbackEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDestroyDebugReportCallbackEXT"
               vkDestroyDebugReportCallbackEXT ::
               VkInstance -- ^ instance
                          ->
                 VkDebugReportCallbackEXT -- ^ callback
                                          -> Ptr VkAllocationCallbacks -- ^ pAllocator
                                                                       -> IO ()

-- | > void vkDebugReportMessageEXT
--   >     ( VkInstance instance
--   >     , VkDebugReportFlagsEXT flags
--   >     , VkDebugReportObjectTypeEXT objectType
--   >     , uint64_t object
--   >     , size_t location
--   >     , int32_t messageCode
--   >     , const char* pLayerPrefix
--   >     , const char* pMessage
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkDebugReportMessageEXT.html vkDebugReportMessageEXT registry at www.khronos.org>
foreign import ccall unsafe "vkDebugReportMessageEXT"
               vkDebugReportMessageEXT ::
               VkInstance -- ^ instance
                          ->
                 VkDebugReportFlagsEXT -- ^ flags
                                       ->
                   VkDebugReportObjectTypeEXT -- ^ objectType
                                              ->
                     Word64 -- ^ object
                            ->
                       #{type size_t} -> Int32 -- ^ messageCode
                                                           -> CString -- ^ pLayerPrefix
                                                                      -> CString -- ^ pMessage
                                                                                 -> IO ()

pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_EXT_DEBUG_REPORT_SPEC_VERSION = 9

type VK_EXT_DEBUG_REPORT_SPEC_VERSION = 9

pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: CString

pattern VK_EXT_DEBUG_REPORT_EXTENSION_NAME <-
        (is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME -> True)
  where VK_EXT_DEBUG_REPORT_EXTENSION_NAME
          = _VK_EXT_DEBUG_REPORT_EXTENSION_NAME

_VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: CString

{-# INLINE _VK_EXT_DEBUG_REPORT_EXTENSION_NAME #-}
_VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  = Ptr "VK_EXT_debug_report\NUL"##

is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: CString -> Bool

{-# INLINE is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME #-}
is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  = (_VK_EXT_DEBUG_REPORT_EXTENSION_NAME ==)

type VK_EXT_DEBUG_REPORT_EXTENSION_NAME = "VK_EXT_debug_report"

pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT ::
        VkStructureType

pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT =
        VkStructureType 1000011000

pattern VK_ERROR_VALIDATION_FAILED_EXT :: VkResult

pattern VK_ERROR_VALIDATION_FAILED_EXT = VkResult (-1000011001)

pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT =
        VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT

-- | VkDebugReportCallbackEXT
pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT :: VkObjectType

pattern VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT =
        VkObjectType 1000011000

pattern VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT =
        VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
