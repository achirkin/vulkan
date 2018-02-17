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
module Graphics.Vulkan.Ext.VK_ANDROID_native_buffer
       (-- * Vulkan extension: @VK_ANDROID_native_buffer@
        -- |
        --
        -- supported: @disabled@
        --
        -- Extension number: @11@
        VkNativeBufferANDROID(..), vkGetSwapchainGrallocUsageANDROID,
        vkAcquireImageANDROID, vkQueueSignalReleaseImageANDROID,
        VK_ANDROID_NATIVE_BUFFER_SPEC_VERSION,
        pattern VK_ANDROID_NATIVE_BUFFER_SPEC_VERSION,
        VK_ANDROID_NATIVE_BUFFER_NUMBER,
        pattern VK_ANDROID_NATIVE_BUFFER_NUMBER,
        VK_ANDROID_NATIVE_BUFFER_NAME,
        pattern VK_ANDROID_NATIVE_BUFFER_NAME,
        pattern VK_STRUCTURE_TYPE_NATIVE_BUFFER_ANDROID)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkNativeBufferANDROID {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     const void* handle;
--   >     int stride;
--   >     int format;
--   >     int usage;
--   > } VkNativeBufferANDROID;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkNativeBufferANDROID.html VkNativeBufferANDROID registry at www.khronos.org>
data VkNativeBufferANDROID = VkNativeBufferANDROID## Addr##
                                                    ByteArray##

instance Eq VkNativeBufferANDROID where
        (VkNativeBufferANDROID## a _) == x@(VkNativeBufferANDROID## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkNativeBufferANDROID where
        (VkNativeBufferANDROID## a _) `compare`
          x@(VkNativeBufferANDROID## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkNativeBufferANDROID where
        sizeOf ~_ = #{size VkNativeBufferANDROID}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkNativeBufferANDROID}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkNativeBufferANDROID where
        unsafeAddr (VkNativeBufferANDROID## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkNativeBufferANDROID## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkNativeBufferANDROID## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkNativeBufferANDROID where
        type StructFields VkNativeBufferANDROID =
             '["sType", "pNext", "handle", "stride", "format", "usage"] -- ' closing tick for hsc2hs
        type CUnionType VkNativeBufferANDROID = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkNativeBufferANDROID = 'False -- ' closing tick for hsc2hs
        type StructExtends VkNativeBufferANDROID = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkNativeBufferANDROID where
        type VkSTypeMType VkNativeBufferANDROID = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkNativeBufferANDROID, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkNativeBufferANDROID, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkNativeBufferANDROID, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkNativeBufferANDROID, sType}

instance {-# OVERLAPPING #-} HasField "sType" VkNativeBufferANDROID
         where
        type FieldType "sType" VkNativeBufferANDROID = VkStructureType
        type FieldOptional "sType" VkNativeBufferANDROID = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkNativeBufferANDROID =
             #{offset VkNativeBufferANDROID, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkNativeBufferANDROID, sType}

instance CanReadField "sType" VkNativeBufferANDROID where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkNativeBufferANDROID where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkNativeBufferANDROID where
        type VkPNextMType VkNativeBufferANDROID = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkNativeBufferANDROID, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkNativeBufferANDROID, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkNativeBufferANDROID, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkNativeBufferANDROID, pNext}

instance {-# OVERLAPPING #-} HasField "pNext" VkNativeBufferANDROID
         where
        type FieldType "pNext" VkNativeBufferANDROID = Ptr Void
        type FieldOptional "pNext" VkNativeBufferANDROID = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkNativeBufferANDROID =
             #{offset VkNativeBufferANDROID, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkNativeBufferANDROID, pNext}

instance CanReadField "pNext" VkNativeBufferANDROID where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkNativeBufferANDROID where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkHandle VkNativeBufferANDROID
         where
        type VkHandleMType VkNativeBufferANDROID = Ptr Void

        {-# NOINLINE vkHandle #-}
        vkHandle x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkNativeBufferANDROID, handle})

        {-# INLINE vkHandleByteOffset #-}
        vkHandleByteOffset ~_
          = #{offset VkNativeBufferANDROID, handle}

        {-# INLINE readVkHandle #-}
        readVkHandle p
          = peekByteOff p #{offset VkNativeBufferANDROID, handle}

        {-# INLINE writeVkHandle #-}
        writeVkHandle p
          = pokeByteOff p #{offset VkNativeBufferANDROID, handle}

instance {-# OVERLAPPING #-}
         HasField "handle" VkNativeBufferANDROID where
        type FieldType "handle" VkNativeBufferANDROID = Ptr Void
        type FieldOptional "handle" VkNativeBufferANDROID = 'False -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkNativeBufferANDROID =
             #{offset VkNativeBufferANDROID, handle}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkNativeBufferANDROID, handle}

instance CanReadField "handle" VkNativeBufferANDROID where
        {-# INLINE getField #-}
        getField = vkHandle

        {-# INLINE readField #-}
        readField = readVkHandle

instance CanWriteField "handle" VkNativeBufferANDROID where
        {-# INLINE writeField #-}
        writeField = writeVkHandle

instance {-# OVERLAPPING #-} HasVkStride VkNativeBufferANDROID
         where
        type VkStrideMType VkNativeBufferANDROID = #{type int}

        {-# NOINLINE vkStride #-}
        vkStride x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkNativeBufferANDROID, stride})

        {-# INLINE vkStrideByteOffset #-}
        vkStrideByteOffset ~_
          = #{offset VkNativeBufferANDROID, stride}

        {-# INLINE readVkStride #-}
        readVkStride p
          = peekByteOff p #{offset VkNativeBufferANDROID, stride}

        {-# INLINE writeVkStride #-}
        writeVkStride p
          = pokeByteOff p #{offset VkNativeBufferANDROID, stride}

instance {-# OVERLAPPING #-}
         HasField "stride" VkNativeBufferANDROID where
        type FieldType "stride" VkNativeBufferANDROID =
             #{type int}
        type FieldOptional "stride" VkNativeBufferANDROID = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stride" VkNativeBufferANDROID =
             #{offset VkNativeBufferANDROID, stride}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkNativeBufferANDROID, stride}

instance CanReadField "stride" VkNativeBufferANDROID where
        {-# INLINE getField #-}
        getField = vkStride

        {-# INLINE readField #-}
        readField = readVkStride

instance CanWriteField "stride" VkNativeBufferANDROID where
        {-# INLINE writeField #-}
        writeField = writeVkStride

instance {-# OVERLAPPING #-} HasVkFormat VkNativeBufferANDROID
         where
        type VkFormatMType VkNativeBufferANDROID = #{type int}

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkNativeBufferANDROID, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkNativeBufferANDROID, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkNativeBufferANDROID, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkNativeBufferANDROID, format}

instance {-# OVERLAPPING #-}
         HasField "format" VkNativeBufferANDROID where
        type FieldType "format" VkNativeBufferANDROID =
             #{type int}
        type FieldOptional "format" VkNativeBufferANDROID = 'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkNativeBufferANDROID =
             #{offset VkNativeBufferANDROID, format}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkNativeBufferANDROID, format}

instance CanReadField "format" VkNativeBufferANDROID where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format" VkNativeBufferANDROID where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-} HasVkUsage VkNativeBufferANDROID where
        type VkUsageMType VkNativeBufferANDROID = #{type int}

        {-# NOINLINE vkUsage #-}
        vkUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkNativeBufferANDROID, usage})

        {-# INLINE vkUsageByteOffset #-}
        vkUsageByteOffset ~_
          = #{offset VkNativeBufferANDROID, usage}

        {-# INLINE readVkUsage #-}
        readVkUsage p
          = peekByteOff p #{offset VkNativeBufferANDROID, usage}

        {-# INLINE writeVkUsage #-}
        writeVkUsage p
          = pokeByteOff p #{offset VkNativeBufferANDROID, usage}

instance {-# OVERLAPPING #-} HasField "usage" VkNativeBufferANDROID
         where
        type FieldType "usage" VkNativeBufferANDROID =
             #{type int}
        type FieldOptional "usage" VkNativeBufferANDROID = 'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkNativeBufferANDROID =
             #{offset VkNativeBufferANDROID, usage}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkNativeBufferANDROID, usage}

instance CanReadField "usage" VkNativeBufferANDROID where
        {-# INLINE getField #-}
        getField = vkUsage

        {-# INLINE readField #-}
        readField = readVkUsage

instance CanWriteField "usage" VkNativeBufferANDROID where
        {-# INLINE writeField #-}
        writeField = writeVkUsage

instance Show VkNativeBufferANDROID where
        showsPrec d x
          = showString "VkNativeBufferANDROID {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandle = " .
                            showsPrec d (vkHandle x) .
                              showString ", " .
                                showString "vkStride = " .
                                  showsPrec d (vkStride x) .
                                    showString ", " .
                                      showString "vkFormat = " .
                                        showsPrec d (vkFormat x) .
                                          showString ", " .
                                            showString "vkUsage = " .
                                              showsPrec d (vkUsage x) . showChar '}'

-- | > VkResult vkGetSwapchainGrallocUsageANDROID
--   >     ( VkDevice device
--   >     , VkFormat format
--   >     , VkImageUsageFlags imageUsage
--   >     , int* grallocUsage
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkGetSwapchainGrallocUsageANDROID.html vkGetSwapchainGrallocUsageANDROID registry at www.khronos.org>
foreign import ccall unsafe "vkGetSwapchainGrallocUsageANDROID"
               vkGetSwapchainGrallocUsageANDROID ::
               VkDevice -- ^ device
                        ->
                 VkFormat -- ^ format
                          ->
                   VkImageUsageFlags -- ^ imageUsage
                                     -> Ptr #{type int} -- ^ grallocUsage
                                                                    -> IO VkResult

-- | > VkResult vkAcquireImageANDROID
--   >     ( VkDevice device
--   >     , VkImage image
--   >     , int nativeFenceFd
--   >     , VkSemaphore semaphore
--   >     , VkFence fence
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkAcquireImageANDROID.html vkAcquireImageANDROID registry at www.khronos.org>
foreign import ccall unsafe "vkAcquireImageANDROID"
               vkAcquireImageANDROID ::
               VkDevice -- ^ device
                        ->
                 VkImage -- ^ image
                         ->
                   #{type int} -> VkSemaphore -- ^ semaphore
                                                          -> VkFence -- ^ fence
                                                                     -> IO VkResult

-- | > VkResult vkQueueSignalReleaseImageANDROID
--   >     ( VkQueue queue
--   >     , uint32_t waitSemaphoreCount
--   >     , const VkSemaphore* pWaitSemaphores
--   >     , VkImage image
--   >     , int* pNativeFenceFd
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkQueueSignalReleaseImageANDROID.html vkQueueSignalReleaseImageANDROID registry at www.khronos.org>
foreign import ccall unsafe "vkQueueSignalReleaseImageANDROID"
               vkQueueSignalReleaseImageANDROID ::
               VkQueue -- ^ queue
                       ->
                 Word32 -- ^ waitSemaphoreCount
                        ->
                   Ptr VkSemaphore -- ^ pWaitSemaphores
                                   ->
                     VkImage -- ^ image
                             -> Ptr #{type int} -- ^ pNativeFenceFd
                                                            -> IO VkResult

pattern VK_ANDROID_NATIVE_BUFFER_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_ANDROID_NATIVE_BUFFER_SPEC_VERSION = 5

type VK_ANDROID_NATIVE_BUFFER_SPEC_VERSION = 5

pattern VK_ANDROID_NATIVE_BUFFER_NUMBER :: (Num a, Eq a) => a

pattern VK_ANDROID_NATIVE_BUFFER_NUMBER = 11

type VK_ANDROID_NATIVE_BUFFER_NUMBER = 11

pattern VK_ANDROID_NATIVE_BUFFER_NAME :: CString

pattern VK_ANDROID_NATIVE_BUFFER_NAME <-
        (is_VK_ANDROID_NATIVE_BUFFER_NAME -> True)
  where VK_ANDROID_NATIVE_BUFFER_NAME
          = _VK_ANDROID_NATIVE_BUFFER_NAME

{-# INLINE _VK_ANDROID_NATIVE_BUFFER_NAME #-}

_VK_ANDROID_NATIVE_BUFFER_NAME :: CString
_VK_ANDROID_NATIVE_BUFFER_NAME
  = Ptr "VK_ANDROID_native_buffer\NUL"##

{-# INLINE is_VK_ANDROID_NATIVE_BUFFER_NAME #-}

is_VK_ANDROID_NATIVE_BUFFER_NAME :: CString -> Bool
is_VK_ANDROID_NATIVE_BUFFER_NAME
  = eqCStrings _VK_ANDROID_NATIVE_BUFFER_NAME

type VK_ANDROID_NATIVE_BUFFER_NAME = "VK_ANDROID_native_buffer"

pattern VK_STRUCTURE_TYPE_NATIVE_BUFFER_ANDROID :: VkStructureType

pattern VK_STRUCTURE_TYPE_NATIVE_BUFFER_ANDROID =
        VkStructureType 1000010000
