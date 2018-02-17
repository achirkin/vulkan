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
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkAllocationCallbacks (..),
                                                   VkInstanceCreateInfo)
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
data VkDebugReportCallbackCreateInfoEXT = VkDebugReportCallbackCreateInfoEXT## Addr##
                                                                              ByteArray##

instance Eq VkDebugReportCallbackCreateInfoEXT where
        (VkDebugReportCallbackCreateInfoEXT## a _) ==
          x@(VkDebugReportCallbackCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDebugReportCallbackCreateInfoEXT where
        (VkDebugReportCallbackCreateInfoEXT## a _) `compare`
          x@(VkDebugReportCallbackCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDebugReportCallbackCreateInfoEXT where
        sizeOf ~_ = #{size VkDebugReportCallbackCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDebugReportCallbackCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDebugReportCallbackCreateInfoEXT where
        unsafeAddr (VkDebugReportCallbackCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDebugReportCallbackCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDebugReportCallbackCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDebugReportCallbackCreateInfoEXT where
        type StructFields VkDebugReportCallbackCreateInfoEXT =
             '["sType", "pNext", "flags", "pfnCallback", "pUserData"] -- ' closing tick for hsc2hs
        type CUnionType VkDebugReportCallbackCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDebugReportCallbackCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDebugReportCallbackCreateInfoEXT =
             '[VkInstanceCreateInfo] -- ' closing tick for hsc2hs

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
         HasField "sType" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "sType" VkDebugReportCallbackCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, sType}
        type FieldIsArray "sType" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, sType}

instance CanReadField "sType" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

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
         HasField "pNext" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "pNext" VkDebugReportCallbackCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, pNext}

instance CanReadField "pNext" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

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
         HasField "flags" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "flags" VkDebugReportCallbackCreateInfoEXT =
             VkDebugReportFlagsEXT
        type FieldOptional "flags" VkDebugReportCallbackCreateInfoEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, flags}
        type FieldIsArray "flags" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, flags}

instance CanReadField "flags" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

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
         HasField "pfnCallback" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "pfnCallback" VkDebugReportCallbackCreateInfoEXT =
             PFN_vkDebugReportCallbackEXT
        type FieldOptional "pfnCallback" VkDebugReportCallbackCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pfnCallback" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}
        type FieldIsArray "pfnCallback" VkDebugReportCallbackCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, pfnCallback}

instance CanReadField "pfnCallback"
           VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPfnCallback

        {-# INLINE readField #-}
        readField = readVkPfnCallback

instance CanWriteField "pfnCallback"
           VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPfnCallback

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

instance {-# OVERLAPPING #-}
         HasField "pUserData" VkDebugReportCallbackCreateInfoEXT where
        type FieldType "pUserData" VkDebugReportCallbackCreateInfoEXT =
             Ptr Void
        type FieldOptional "pUserData" VkDebugReportCallbackCreateInfoEXT =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pUserData" VkDebugReportCallbackCreateInfoEXT =
             #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}
        type FieldIsArray "pUserData" VkDebugReportCallbackCreateInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDebugReportCallbackCreateInfoEXT, pUserData}

instance CanReadField "pUserData"
           VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPUserData

        {-# INLINE readField #-}
        readField = readVkPUserData

instance CanWriteField "pUserData"
           VkDebugReportCallbackCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPUserData

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

{-# INLINE _VK_EXT_DEBUG_REPORT_EXTENSION_NAME #-}

_VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: CString
_VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  = Ptr "VK_EXT_debug_report\NUL"##

{-# INLINE is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME #-}

is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME :: CString -> Bool
is_VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  = eqCStrings _VK_EXT_DEBUG_REPORT_EXTENSION_NAME

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
