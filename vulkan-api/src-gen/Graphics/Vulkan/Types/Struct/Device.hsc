#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.Device
       (VkDeviceCreateInfo(..), VkDeviceEventInfoEXT(..),
        VkDeviceGeneratedCommandsFeaturesNVX(..),
        VkDeviceGeneratedCommandsLimitsNVX(..),
        VkDeviceGroupBindSparseInfo(..), VkDeviceGroupBindSparseInfoKHR,
        VkDeviceGroupCommandBufferBeginInfo(..),
        VkDeviceGroupCommandBufferBeginInfoKHR,
        VkDeviceGroupDeviceCreateInfo(..),
        VkDeviceGroupDeviceCreateInfoKHR,
        VkDeviceGroupPresentCapabilitiesKHR(..),
        VkDeviceGroupPresentInfoKHR(..),
        VkDeviceGroupRenderPassBeginInfo(..),
        VkDeviceGroupRenderPassBeginInfoKHR, VkDeviceGroupSubmitInfo(..),
        VkDeviceGroupSubmitInfoKHR,
        VkDeviceGroupSwapchainCreateInfoKHR(..),
        VkDeviceQueueCreateInfo(..),
        VkDeviceQueueGlobalPriorityCreateInfoEXT(..),
        VkDeviceQueueInfo2(..))
       where
import           Foreign.Storable                                    (Storable (..))
import           GHC.Base                                            (Addr##,
                                                                      ByteArray##,
                                                                      Proxy##,
                                                                      byteArrayContents##,
                                                                      plusAddr##,
                                                                      proxy##)
import           GHC.TypeLits                                        (KnownNat,
                                                                      natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants                           (VK_MAX_DEVICE_GROUP_SIZE,
                                                                      pattern VK_MAX_DEVICE_GROUP_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                     (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks                      (VkDeviceCreateFlags)
import           Graphics.Vulkan.Types.Enum.Device                   (VkDeviceEventTypeEXT,
                                                                      VkDeviceGroupPresentModeFlagBitsKHR,
                                                                      VkDeviceGroupPresentModeFlagsKHR,
                                                                      VkDeviceQueueCreateFlags)
import           Graphics.Vulkan.Types.Enum.Queue                    (VkQueueGlobalPriorityEXT)
import           Graphics.Vulkan.Types.Enum.StructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Handles                       (VkPhysicalDevice)
import           Graphics.Vulkan.Types.Struct.Bind                   (VkBindSparseInfo)
import           Graphics.Vulkan.Types.Struct.Command                (VkCommandBufferBeginInfo)
import           Graphics.Vulkan.Types.Struct.PhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import           Graphics.Vulkan.Types.Struct.Present                (VkPresentInfoKHR)
import           Graphics.Vulkan.Types.Struct.Rect                   (VkRect2D)
import           Graphics.Vulkan.Types.Struct.RenderPass             (VkRenderPassBeginInfo)
import           Graphics.Vulkan.Types.Struct.SubmitInfo             (VkSubmitInfo)
import           Graphics.Vulkan.Types.Struct.SwapchainC             (VkSwapchainCreateInfoKHR)
import           System.IO.Unsafe                                    (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*     pNext;
--   >     VkDeviceCreateFlags    flags;
--   >     uint32_t        queueCreateInfoCount;
--   >     const VkDeviceQueueCreateInfo* pQueueCreateInfos;
--   >     uint32_t               enabledLayerCount;
--   >     const char* const*      ppEnabledLayerNames;
--   >     uint32_t               enabledExtensionCount;
--   >     const char* const*      ppEnabledExtensionNames;
--   >     const VkPhysicalDeviceFeatures* pEnabledFeatures;
--   > } VkDeviceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceCreateInfo VkDeviceCreateInfo registry at www.khronos.org>
data VkDeviceCreateInfo = VkDeviceCreateInfo## Addr## ByteArray##

instance Eq VkDeviceCreateInfo where
        (VkDeviceCreateInfo## a _) == x@(VkDeviceCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceCreateInfo where
        (VkDeviceCreateInfo## a _) `compare` x@(VkDeviceCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceCreateInfo where
        sizeOf ~_ = #{size VkDeviceCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceCreateInfo where
        unsafeAddr (VkDeviceCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceCreateInfo where
        type StructFields VkDeviceCreateInfo =
             '["sType", "pNext", "flags", "queueCreateInfoCount", -- ' closing tick for hsc2hs
               "pQueueCreateInfos", "enabledLayerCount", "ppEnabledLayerNames",
               "enabledExtensionCount", "ppEnabledExtensionNames",
               "pEnabledFeatures"]
        type CUnionType VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkDeviceCreateInfo
         where
        type FieldType "sType" VkDeviceCreateInfo = VkStructureType
        type FieldOptional "sType" VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceCreateInfo =
             #{offset VkDeviceCreateInfo, sType}
        type FieldIsArray "sType" VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceCreateInfo, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkDeviceCreateInfo
         where
        type FieldType "pNext" VkDeviceCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceCreateInfo =
             #{offset VkDeviceCreateInfo, pNext}
        type FieldIsArray "pNext" VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceCreateInfo, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkDeviceCreateInfo
         where
        type FieldType "flags" VkDeviceCreateInfo = VkDeviceCreateFlags
        type FieldOptional "flags" VkDeviceCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDeviceCreateInfo =
             #{offset VkDeviceCreateInfo, flags}
        type FieldIsArray "flags" VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "queueCreateInfoCount" VkDeviceCreateInfo where
        type FieldType "queueCreateInfoCount" VkDeviceCreateInfo = Word32
        type FieldOptional "queueCreateInfoCount" VkDeviceCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "queueCreateInfoCount" VkDeviceCreateInfo =
             #{offset VkDeviceCreateInfo, queueCreateInfoCount}
        type FieldIsArray "queueCreateInfoCount" VkDeviceCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceCreateInfo, queueCreateInfoCount}

instance {-# OVERLAPPING #-}
         CanReadField "queueCreateInfoCount" VkDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, queueCreateInfoCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceCreateInfo, queueCreateInfoCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queueCreateInfoCount" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceCreateInfo, queueCreateInfoCount}

instance {-# OVERLAPPING #-}
         HasField "pQueueCreateInfos" VkDeviceCreateInfo where
        type FieldType "pQueueCreateInfos" VkDeviceCreateInfo =
             Ptr VkDeviceQueueCreateInfo
        type FieldOptional "pQueueCreateInfos" VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pQueueCreateInfos" VkDeviceCreateInfo =
             #{offset VkDeviceCreateInfo, pQueueCreateInfos}
        type FieldIsArray "pQueueCreateInfos" VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceCreateInfo, pQueueCreateInfos}

instance {-# OVERLAPPING #-}
         CanReadField "pQueueCreateInfos" VkDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, pQueueCreateInfos})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceCreateInfo, pQueueCreateInfos}

instance {-# OVERLAPPING #-}
         CanWriteField "pQueueCreateInfos" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceCreateInfo, pQueueCreateInfos}

instance {-# OVERLAPPING #-}
         HasField "enabledLayerCount" VkDeviceCreateInfo where
        type FieldType "enabledLayerCount" VkDeviceCreateInfo = Word32
        type FieldOptional "enabledLayerCount" VkDeviceCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "enabledLayerCount" VkDeviceCreateInfo =
             #{offset VkDeviceCreateInfo, enabledLayerCount}
        type FieldIsArray "enabledLayerCount" VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceCreateInfo, enabledLayerCount}

instance {-# OVERLAPPING #-}
         CanReadField "enabledLayerCount" VkDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, enabledLayerCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceCreateInfo, enabledLayerCount}

instance {-# OVERLAPPING #-}
         CanWriteField "enabledLayerCount" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceCreateInfo, enabledLayerCount}

instance {-# OVERLAPPING #-}
         HasField "ppEnabledLayerNames" VkDeviceCreateInfo where
        type FieldType "ppEnabledLayerNames" VkDeviceCreateInfo =
             Ptr CString
        type FieldOptional "ppEnabledLayerNames" VkDeviceCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "ppEnabledLayerNames" VkDeviceCreateInfo =
             #{offset VkDeviceCreateInfo, ppEnabledLayerNames}
        type FieldIsArray "ppEnabledLayerNames" VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceCreateInfo, ppEnabledLayerNames}

instance {-# OVERLAPPING #-}
         CanReadField "ppEnabledLayerNames" VkDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, ppEnabledLayerNames})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceCreateInfo, ppEnabledLayerNames}

instance {-# OVERLAPPING #-}
         CanWriteField "ppEnabledLayerNames" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceCreateInfo, ppEnabledLayerNames}

instance {-# OVERLAPPING #-}
         HasField "enabledExtensionCount" VkDeviceCreateInfo where
        type FieldType "enabledExtensionCount" VkDeviceCreateInfo = Word32
        type FieldOptional "enabledExtensionCount" VkDeviceCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "enabledExtensionCount" VkDeviceCreateInfo =
             #{offset VkDeviceCreateInfo, enabledExtensionCount}
        type FieldIsArray "enabledExtensionCount" VkDeviceCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceCreateInfo, enabledExtensionCount}

instance {-# OVERLAPPING #-}
         CanReadField "enabledExtensionCount" VkDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, enabledExtensionCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceCreateInfo, enabledExtensionCount}

instance {-# OVERLAPPING #-}
         CanWriteField "enabledExtensionCount" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceCreateInfo, enabledExtensionCount}

instance {-# OVERLAPPING #-}
         HasField "ppEnabledExtensionNames" VkDeviceCreateInfo where
        type FieldType "ppEnabledExtensionNames" VkDeviceCreateInfo =
             Ptr CString
        type FieldOptional "ppEnabledExtensionNames" VkDeviceCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "ppEnabledExtensionNames" VkDeviceCreateInfo =
             #{offset VkDeviceCreateInfo, ppEnabledExtensionNames}
        type FieldIsArray "ppEnabledExtensionNames" VkDeviceCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceCreateInfo, ppEnabledExtensionNames}

instance {-# OVERLAPPING #-}
         CanReadField "ppEnabledExtensionNames" VkDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, ppEnabledExtensionNames})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceCreateInfo, ppEnabledExtensionNames}

instance {-# OVERLAPPING #-}
         CanWriteField "ppEnabledExtensionNames" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceCreateInfo, ppEnabledExtensionNames}

instance {-# OVERLAPPING #-}
         HasField "pEnabledFeatures" VkDeviceCreateInfo where
        type FieldType "pEnabledFeatures" VkDeviceCreateInfo =
             Ptr VkPhysicalDeviceFeatures
        type FieldOptional "pEnabledFeatures" VkDeviceCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pEnabledFeatures" VkDeviceCreateInfo =
             #{offset VkDeviceCreateInfo, pEnabledFeatures}
        type FieldIsArray "pEnabledFeatures" VkDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceCreateInfo, pEnabledFeatures}

instance {-# OVERLAPPING #-}
         CanReadField "pEnabledFeatures" VkDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, pEnabledFeatures})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceCreateInfo, pEnabledFeatures}

instance {-# OVERLAPPING #-}
         CanWriteField "pEnabledFeatures" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceCreateInfo, pEnabledFeatures}

instance Show VkDeviceCreateInfo where
        showsPrec d x
          = showString "VkDeviceCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "queueCreateInfoCount = " .
                                  showsPrec d (getField @"queueCreateInfoCount" x) .
                                    showString ", " .
                                      showString "pQueueCreateInfos = " .
                                        showsPrec d (getField @"pQueueCreateInfos" x) .
                                          showString ", " .
                                            showString "enabledLayerCount = " .
                                              showsPrec d (getField @"enabledLayerCount" x) .
                                                showString ", " .
                                                  showString "ppEnabledLayerNames = " .
                                                    showsPrec d (getField @"ppEnabledLayerNames" x)
                                                      .
                                                      showString ", " .
                                                        showString "enabledExtensionCount = " .
                                                          showsPrec d
                                                            (getField @"enabledExtensionCount" x)
                                                            .
                                                            showString ", " .
                                                              showString
                                                                "ppEnabledExtensionNames = "
                                                                .
                                                                showsPrec d
                                                                  (getField
                                                                     @"ppEnabledExtensionNames"
                                                                     x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "pEnabledFeatures = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"pEnabledFeatures"
                                                                           x)
                                                                        . showChar '}'

-- | > typedef struct VkDeviceEventInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceEventTypeEXT             deviceEvent;
--   > } VkDeviceEventInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceEventInfoEXT VkDeviceEventInfoEXT registry at www.khronos.org>
data VkDeviceEventInfoEXT = VkDeviceEventInfoEXT## Addr## ByteArray##

instance Eq VkDeviceEventInfoEXT where
        (VkDeviceEventInfoEXT## a _) == x@(VkDeviceEventInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceEventInfoEXT where
        (VkDeviceEventInfoEXT## a _) `compare` x@(VkDeviceEventInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceEventInfoEXT where
        sizeOf ~_ = #{size VkDeviceEventInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceEventInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceEventInfoEXT where
        unsafeAddr (VkDeviceEventInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceEventInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceEventInfoEXT## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceEventInfoEXT where
        type StructFields VkDeviceEventInfoEXT =
             '["sType", "pNext", "deviceEvent"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceEventInfoEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkDeviceEventInfoEXT
         where
        type FieldType "sType" VkDeviceEventInfoEXT = VkStructureType
        type FieldOptional "sType" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceEventInfoEXT =
             #{offset VkDeviceEventInfoEXT, sType}
        type FieldIsArray "sType" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceEventInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceEventInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceEventInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceEventInfoEXT, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkDeviceEventInfoEXT
         where
        type FieldType "pNext" VkDeviceEventInfoEXT = Ptr Void
        type FieldOptional "pNext" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceEventInfoEXT =
             #{offset VkDeviceEventInfoEXT, pNext}
        type FieldIsArray "pNext" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceEventInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceEventInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceEvent" VkDeviceEventInfoEXT where
        type FieldType "deviceEvent" VkDeviceEventInfoEXT =
             VkDeviceEventTypeEXT
        type FieldOptional "deviceEvent" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceEvent" VkDeviceEventInfoEXT =
             #{offset VkDeviceEventInfoEXT, deviceEvent}
        type FieldIsArray "deviceEvent" VkDeviceEventInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceEventInfoEXT, deviceEvent}

instance {-# OVERLAPPING #-}
         CanReadField "deviceEvent" VkDeviceEventInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceEventInfoEXT, deviceEvent})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceEventInfoEXT, deviceEvent}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceEvent" VkDeviceEventInfoEXT where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceEventInfoEXT, deviceEvent}

instance Show VkDeviceEventInfoEXT where
        showsPrec d x
          = showString "VkDeviceEventInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceEvent = " .
                            showsPrec d (getField @"deviceEvent" x) . showChar '}'

-- | > typedef struct VkDeviceGeneratedCommandsFeaturesNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         computeBindingPointSupport;
--   > } VkDeviceGeneratedCommandsFeaturesNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGeneratedCommandsFeaturesNVX VkDeviceGeneratedCommandsFeaturesNVX registry at www.khronos.org>
data VkDeviceGeneratedCommandsFeaturesNVX = VkDeviceGeneratedCommandsFeaturesNVX## Addr##
                                                                                  ByteArray##

instance Eq VkDeviceGeneratedCommandsFeaturesNVX where
        (VkDeviceGeneratedCommandsFeaturesNVX## a _) ==
          x@(VkDeviceGeneratedCommandsFeaturesNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGeneratedCommandsFeaturesNVX where
        (VkDeviceGeneratedCommandsFeaturesNVX## a _) `compare`
          x@(VkDeviceGeneratedCommandsFeaturesNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGeneratedCommandsFeaturesNVX where
        sizeOf ~_
          = #{size VkDeviceGeneratedCommandsFeaturesNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGeneratedCommandsFeaturesNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGeneratedCommandsFeaturesNVX
         where
        unsafeAddr (VkDeviceGeneratedCommandsFeaturesNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGeneratedCommandsFeaturesNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGeneratedCommandsFeaturesNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGeneratedCommandsFeaturesNVX where
        type StructFields VkDeviceGeneratedCommandsFeaturesNVX =
             '["sType", "pNext", "computeBindingPointSupport"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGeneratedCommandsFeaturesNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGeneratedCommandsFeaturesNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGeneratedCommandsFeaturesNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGeneratedCommandsFeaturesNVX where
        type FieldType "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}
        type FieldIsArray "sType" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGeneratedCommandsFeaturesNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGeneratedCommandsFeaturesNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGeneratedCommandsFeaturesNVX where
        type FieldType "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}
        type FieldIsArray "pNext" VkDeviceGeneratedCommandsFeaturesNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGeneratedCommandsFeaturesNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGeneratedCommandsFeaturesNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "computeBindingPointSupport"
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        type FieldType "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             = VkBool32
        type FieldOptional "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             =
             #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}
        type FieldIsArray "computeBindingPointSupport"
               VkDeviceGeneratedCommandsFeaturesNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

instance {-# OVERLAPPING #-}
         CanReadField "computeBindingPointSupport"
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

instance {-# OVERLAPPING #-}
         CanWriteField "computeBindingPointSupport"
           VkDeviceGeneratedCommandsFeaturesNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsFeaturesNVX, computeBindingPointSupport}

instance Show VkDeviceGeneratedCommandsFeaturesNVX where
        showsPrec d x
          = showString "VkDeviceGeneratedCommandsFeaturesNVX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "computeBindingPointSupport = " .
                            showsPrec d (getField @"computeBindingPointSupport" x) .
                              showChar '}'

-- | > typedef struct VkDeviceGeneratedCommandsLimitsNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         maxIndirectCommandsLayoutTokenCount;
--   >     uint32_t                         maxObjectEntryCounts;
--   >     uint32_t                         minSequenceCountBufferOffsetAlignment;
--   >     uint32_t                         minSequenceIndexBufferOffsetAlignment;
--   >     uint32_t                         minCommandsTokenBufferOffsetAlignment;
--   > } VkDeviceGeneratedCommandsLimitsNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGeneratedCommandsLimitsNVX VkDeviceGeneratedCommandsLimitsNVX registry at www.khronos.org>
data VkDeviceGeneratedCommandsLimitsNVX = VkDeviceGeneratedCommandsLimitsNVX## Addr##
                                                                              ByteArray##

instance Eq VkDeviceGeneratedCommandsLimitsNVX where
        (VkDeviceGeneratedCommandsLimitsNVX## a _) ==
          x@(VkDeviceGeneratedCommandsLimitsNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGeneratedCommandsLimitsNVX where
        (VkDeviceGeneratedCommandsLimitsNVX## a _) `compare`
          x@(VkDeviceGeneratedCommandsLimitsNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGeneratedCommandsLimitsNVX where
        sizeOf ~_ = #{size VkDeviceGeneratedCommandsLimitsNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGeneratedCommandsLimitsNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGeneratedCommandsLimitsNVX where
        unsafeAddr (VkDeviceGeneratedCommandsLimitsNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGeneratedCommandsLimitsNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGeneratedCommandsLimitsNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGeneratedCommandsLimitsNVX where
        type StructFields VkDeviceGeneratedCommandsLimitsNVX =
             '["sType", "pNext", "maxIndirectCommandsLayoutTokenCount", -- ' closing tick for hsc2hs
               "maxObjectEntryCounts", "minSequenceCountBufferOffsetAlignment",
               "minSequenceIndexBufferOffsetAlignment",
               "minCommandsTokenBufferOffsetAlignment"]
        type CUnionType VkDeviceGeneratedCommandsLimitsNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGeneratedCommandsLimitsNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGeneratedCommandsLimitsNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGeneratedCommandsLimitsNVX where
        type FieldType "sType" VkDeviceGeneratedCommandsLimitsNVX =
             VkStructureType
        type FieldOptional "sType" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGeneratedCommandsLimitsNVX =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}
        type FieldIsArray "sType" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGeneratedCommandsLimitsNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGeneratedCommandsLimitsNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGeneratedCommandsLimitsNVX where
        type FieldType "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}
        type FieldIsArray "pNext" VkDeviceGeneratedCommandsLimitsNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGeneratedCommandsLimitsNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGeneratedCommandsLimitsNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "maxIndirectCommandsLayoutTokenCount"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}
        type FieldIsArray "maxIndirectCommandsLayoutTokenCount"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}

instance {-# OVERLAPPING #-}
         CanReadField "maxIndirectCommandsLayoutTokenCount"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}

instance {-# OVERLAPPING #-}
         CanWriteField "maxIndirectCommandsLayoutTokenCount"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxIndirectCommandsLayoutTokenCount}

instance {-# OVERLAPPING #-}
         HasField "maxObjectEntryCounts" VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}
        type FieldIsArray "maxObjectEntryCounts"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}

instance {-# OVERLAPPING #-}
         CanReadField "maxObjectEntryCounts"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}

instance {-# OVERLAPPING #-}
         CanWriteField "maxObjectEntryCounts"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, maxObjectEntryCounts}

instance {-# OVERLAPPING #-}
         HasField "minSequenceCountBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}
        type FieldIsArray "minSequenceCountBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanReadField "minSequenceCountBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minSequenceCountBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceCountBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "minSequenceIndexBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}
        type FieldIsArray "minSequenceIndexBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanReadField "minSequenceIndexBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minSequenceIndexBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minSequenceIndexBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         HasField "minCommandsTokenBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        type FieldType "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = Word32
        type FieldOptional "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             =
             #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}
        type FieldIsArray "minCommandsTokenBufferOffsetAlignment"
               VkDeviceGeneratedCommandsLimitsNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanReadField "minCommandsTokenBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}

instance {-# OVERLAPPING #-}
         CanWriteField "minCommandsTokenBufferOffsetAlignment"
           VkDeviceGeneratedCommandsLimitsNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGeneratedCommandsLimitsNVX, minCommandsTokenBufferOffsetAlignment}

instance Show VkDeviceGeneratedCommandsLimitsNVX where
        showsPrec d x
          = showString "VkDeviceGeneratedCommandsLimitsNVX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "maxIndirectCommandsLayoutTokenCount = " .
                            showsPrec d (getField @"maxIndirectCommandsLayoutTokenCount" x) .
                              showString ", " .
                                showString "maxObjectEntryCounts = " .
                                  showsPrec d (getField @"maxObjectEntryCounts" x) .
                                    showString ", " .
                                      showString "minSequenceCountBufferOffsetAlignment = " .
                                        showsPrec d
                                          (getField @"minSequenceCountBufferOffsetAlignment" x)
                                          .
                                          showString ", " .
                                            showString "minSequenceIndexBufferOffsetAlignment = " .
                                              showsPrec d
                                                (getField @"minSequenceIndexBufferOffsetAlignment"
                                                   x)
                                                .
                                                showString ", " .
                                                  showString
                                                    "minCommandsTokenBufferOffsetAlignment = "
                                                    .
                                                    showsPrec d
                                                      (getField
                                                         @"minCommandsTokenBufferOffsetAlignment"
                                                         x)
                                                      . showChar '}'

-- | > typedef struct VkDeviceGroupBindSparseInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         resourceDeviceIndex;
--   >     uint32_t                         memoryDeviceIndex;
--   > } VkDeviceGroupBindSparseInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupBindSparseInfo VkDeviceGroupBindSparseInfo registry at www.khronos.org>
data VkDeviceGroupBindSparseInfo = VkDeviceGroupBindSparseInfo## Addr##
                                                                ByteArray##

instance Eq VkDeviceGroupBindSparseInfo where
        (VkDeviceGroupBindSparseInfo## a _) ==
          x@(VkDeviceGroupBindSparseInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupBindSparseInfo where
        (VkDeviceGroupBindSparseInfo## a _) `compare`
          x@(VkDeviceGroupBindSparseInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupBindSparseInfo where
        sizeOf ~_ = #{size VkDeviceGroupBindSparseInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupBindSparseInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupBindSparseInfo where
        unsafeAddr (VkDeviceGroupBindSparseInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupBindSparseInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupBindSparseInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupBindSparseInfo where
        type StructFields VkDeviceGroupBindSparseInfo =
             '["sType", "pNext", "resourceDeviceIndex", "memoryDeviceIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupBindSparseInfo =
             '[VkBindSparseInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupBindSparseInfo where
        type FieldType "sType" VkDeviceGroupBindSparseInfo =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupBindSparseInfo =
             #{offset VkDeviceGroupBindSparseInfo, sType}
        type FieldIsArray "sType" VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupBindSparseInfo where
        type FieldType "pNext" VkDeviceGroupBindSparseInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupBindSparseInfo =
             #{offset VkDeviceGroupBindSparseInfo, pNext}
        type FieldIsArray "pNext" VkDeviceGroupBindSparseInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "resourceDeviceIndex" VkDeviceGroupBindSparseInfo where
        type FieldType "resourceDeviceIndex" VkDeviceGroupBindSparseInfo =
             Word32
        type FieldOptional "resourceDeviceIndex"
               VkDeviceGroupBindSparseInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "resourceDeviceIndex" VkDeviceGroupBindSparseInfo
             =
             #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex}
        type FieldIsArray "resourceDeviceIndex" VkDeviceGroupBindSparseInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         CanReadField "resourceDeviceIndex" VkDeviceGroupBindSparseInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "resourceDeviceIndex" VkDeviceGroupBindSparseInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfo, resourceDeviceIndex}

instance {-# OVERLAPPING #-}
         HasField "memoryDeviceIndex" VkDeviceGroupBindSparseInfo where
        type FieldType "memoryDeviceIndex" VkDeviceGroupBindSparseInfo =
             Word32
        type FieldOptional "memoryDeviceIndex" VkDeviceGroupBindSparseInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "memoryDeviceIndex" VkDeviceGroupBindSparseInfo =
             #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex}
        type FieldIsArray "memoryDeviceIndex" VkDeviceGroupBindSparseInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex}

instance {-# OVERLAPPING #-}
         CanReadField "memoryDeviceIndex" VkDeviceGroupBindSparseInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "memoryDeviceIndex" VkDeviceGroupBindSparseInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupBindSparseInfo, memoryDeviceIndex}

instance Show VkDeviceGroupBindSparseInfo where
        showsPrec d x
          = showString "VkDeviceGroupBindSparseInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "resourceDeviceIndex = " .
                            showsPrec d (getField @"resourceDeviceIndex" x) .
                              showString ", " .
                                showString "memoryDeviceIndex = " .
                                  showsPrec d (getField @"memoryDeviceIndex" x) . showChar '}'

-- | Alias for `VkDeviceGroupBindSparseInfo`
type VkDeviceGroupBindSparseInfoKHR = VkDeviceGroupBindSparseInfo

-- | > typedef struct VkDeviceGroupCommandBufferBeginInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   > } VkDeviceGroupCommandBufferBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupCommandBufferBeginInfo VkDeviceGroupCommandBufferBeginInfo registry at www.khronos.org>
data VkDeviceGroupCommandBufferBeginInfo = VkDeviceGroupCommandBufferBeginInfo## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupCommandBufferBeginInfo where
        (VkDeviceGroupCommandBufferBeginInfo## a _) ==
          x@(VkDeviceGroupCommandBufferBeginInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupCommandBufferBeginInfo where
        (VkDeviceGroupCommandBufferBeginInfo## a _) `compare`
          x@(VkDeviceGroupCommandBufferBeginInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupCommandBufferBeginInfo where
        sizeOf ~_ = #{size VkDeviceGroupCommandBufferBeginInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupCommandBufferBeginInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupCommandBufferBeginInfo
         where
        unsafeAddr (VkDeviceGroupCommandBufferBeginInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupCommandBufferBeginInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupCommandBufferBeginInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupCommandBufferBeginInfo where
        type StructFields VkDeviceGroupCommandBufferBeginInfo =
             '["sType", "pNext", "deviceMask"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupCommandBufferBeginInfo =
             '[VkCommandBufferBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupCommandBufferBeginInfo where
        type FieldType "sType" VkDeviceGroupCommandBufferBeginInfo =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupCommandBufferBeginInfo =
             #{offset VkDeviceGroupCommandBufferBeginInfo, sType}
        type FieldIsArray "sType" VkDeviceGroupCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupCommandBufferBeginInfo where
        type FieldType "pNext" VkDeviceGroupCommandBufferBeginInfo =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupCommandBufferBeginInfo =
             #{offset VkDeviceGroupCommandBufferBeginInfo, pNext}
        type FieldIsArray "pNext" VkDeviceGroupCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkDeviceGroupCommandBufferBeginInfo where
        type FieldType "deviceMask" VkDeviceGroupCommandBufferBeginInfo =
             Word32
        type FieldOptional "deviceMask" VkDeviceGroupCommandBufferBeginInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkDeviceGroupCommandBufferBeginInfo =
             #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask}
        type FieldIsArray "deviceMask" VkDeviceGroupCommandBufferBeginInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkDeviceGroupCommandBufferBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkDeviceGroupCommandBufferBeginInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupCommandBufferBeginInfo, deviceMask}

instance Show VkDeviceGroupCommandBufferBeginInfo where
        showsPrec d x
          = showString "VkDeviceGroupCommandBufferBeginInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceMask = " .
                            showsPrec d (getField @"deviceMask" x) . showChar '}'

-- | Alias for `VkDeviceGroupCommandBufferBeginInfo`
type VkDeviceGroupCommandBufferBeginInfoKHR =
     VkDeviceGroupCommandBufferBeginInfo

-- | > typedef struct VkDeviceGroupDeviceCreateInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         physicalDeviceCount;
--   >     const VkPhysicalDevice*  pPhysicalDevices;
--   > } VkDeviceGroupDeviceCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupDeviceCreateInfo VkDeviceGroupDeviceCreateInfo registry at www.khronos.org>
data VkDeviceGroupDeviceCreateInfo = VkDeviceGroupDeviceCreateInfo## Addr##
                                                                    ByteArray##

instance Eq VkDeviceGroupDeviceCreateInfo where
        (VkDeviceGroupDeviceCreateInfo## a _) ==
          x@(VkDeviceGroupDeviceCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupDeviceCreateInfo where
        (VkDeviceGroupDeviceCreateInfo## a _) `compare`
          x@(VkDeviceGroupDeviceCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupDeviceCreateInfo where
        sizeOf ~_ = #{size VkDeviceGroupDeviceCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupDeviceCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupDeviceCreateInfo where
        unsafeAddr (VkDeviceGroupDeviceCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupDeviceCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupDeviceCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupDeviceCreateInfo where
        type StructFields VkDeviceGroupDeviceCreateInfo =
             '["sType", "pNext", "physicalDeviceCount", "pPhysicalDevices"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupDeviceCreateInfo =
             '[VkDeviceCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupDeviceCreateInfo where
        type FieldType "sType" VkDeviceGroupDeviceCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupDeviceCreateInfo =
             #{offset VkDeviceGroupDeviceCreateInfo, sType}
        type FieldIsArray "sType" VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupDeviceCreateInfo where
        type FieldType "pNext" VkDeviceGroupDeviceCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupDeviceCreateInfo =
             #{offset VkDeviceGroupDeviceCreateInfo, pNext}
        type FieldIsArray "pNext" VkDeviceGroupDeviceCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "physicalDeviceCount" VkDeviceGroupDeviceCreateInfo where
        type FieldType "physicalDeviceCount" VkDeviceGroupDeviceCreateInfo
             = Word32
        type FieldOptional "physicalDeviceCount"
               VkDeviceGroupDeviceCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "physicalDeviceCount"
               VkDeviceGroupDeviceCreateInfo
             =
             #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount}
        type FieldIsArray "physicalDeviceCount"
               VkDeviceGroupDeviceCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         CanReadField "physicalDeviceCount" VkDeviceGroupDeviceCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         CanWriteField "physicalDeviceCount" VkDeviceGroupDeviceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfo, physicalDeviceCount}

instance {-# OVERLAPPING #-}
         HasField "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo where
        type FieldType "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo =
             Ptr VkPhysicalDevice
        type FieldOptional "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo =
             #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices}
        type FieldIsArray "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices}

instance {-# OVERLAPPING #-}
         CanReadField "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices}

instance {-# OVERLAPPING #-}
         CanWriteField "pPhysicalDevices" VkDeviceGroupDeviceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupDeviceCreateInfo, pPhysicalDevices}

instance Show VkDeviceGroupDeviceCreateInfo where
        showsPrec d x
          = showString "VkDeviceGroupDeviceCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "physicalDeviceCount = " .
                            showsPrec d (getField @"physicalDeviceCount" x) .
                              showString ", " .
                                showString "pPhysicalDevices = " .
                                  showsPrec d (getField @"pPhysicalDevices" x) . showChar '}'

-- | Alias for `VkDeviceGroupDeviceCreateInfo`
type VkDeviceGroupDeviceCreateInfoKHR =
     VkDeviceGroupDeviceCreateInfo

-- | > typedef struct VkDeviceGroupPresentCapabilitiesKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         presentMask[VK_MAX_DEVICE_GROUP_SIZE];
--   >     VkDeviceGroupPresentModeFlagsKHR modes;
--   > } VkDeviceGroupPresentCapabilitiesKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupPresentCapabilitiesKHR VkDeviceGroupPresentCapabilitiesKHR registry at www.khronos.org>
data VkDeviceGroupPresentCapabilitiesKHR = VkDeviceGroupPresentCapabilitiesKHR## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupPresentCapabilitiesKHR where
        (VkDeviceGroupPresentCapabilitiesKHR## a _) ==
          x@(VkDeviceGroupPresentCapabilitiesKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentCapabilitiesKHR where
        (VkDeviceGroupPresentCapabilitiesKHR## a _) `compare`
          x@(VkDeviceGroupPresentCapabilitiesKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentCapabilitiesKHR where
        sizeOf ~_ = #{size VkDeviceGroupPresentCapabilitiesKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupPresentCapabilitiesKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupPresentCapabilitiesKHR
         where
        unsafeAddr (VkDeviceGroupPresentCapabilitiesKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupPresentCapabilitiesKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupPresentCapabilitiesKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupPresentCapabilitiesKHR where
        type StructFields VkDeviceGroupPresentCapabilitiesKHR =
             '["sType", "pNext", "presentMask", "modes"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupPresentCapabilitiesKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupPresentCapabilitiesKHR = 'True -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupPresentCapabilitiesKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupPresentCapabilitiesKHR where
        type FieldType "sType" VkDeviceGroupPresentCapabilitiesKHR =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupPresentCapabilitiesKHR =
             #{offset VkDeviceGroupPresentCapabilitiesKHR, sType}
        type FieldIsArray "sType" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupPresentCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupPresentCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupPresentCapabilitiesKHR where
        type FieldType "pNext" VkDeviceGroupPresentCapabilitiesKHR =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupPresentCapabilitiesKHR =
             #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext}
        type FieldIsArray "pNext" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupPresentCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupPresentCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "presentMask" VkDeviceGroupPresentCapabilitiesKHR where
        type FieldType "presentMask" VkDeviceGroupPresentCapabilitiesKHR =
             Word32
        type FieldOptional "presentMask"
               VkDeviceGroupPresentCapabilitiesKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "presentMask" VkDeviceGroupPresentCapabilitiesKHR
             =
             #{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}
        type FieldIsArray "presentMask" VkDeviceGroupPresentCapabilitiesKHR
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "presentMask" idx
            VkDeviceGroupPresentCapabilitiesKHR) =>
         CanReadFieldArray "presentMask" idx
           VkDeviceGroupPresentCapabilitiesKHR
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 0
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 1
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 2
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "presentMask" 3
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}
        type FieldArrayLength "presentMask"
               VkDeviceGroupPresentCapabilitiesKHR
             = VK_MAX_DEVICE_GROUP_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_DEVICE_GROUP_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}
                      +
                      sizeOf (undefined :: Word32) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "presentMask" idx
            VkDeviceGroupPresentCapabilitiesKHR) =>
         CanWriteFieldArray "presentMask" idx
           VkDeviceGroupPresentCapabilitiesKHR
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 0
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 1
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 2
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "presentMask" 3
                         VkDeviceGroupPresentCapabilitiesKHR
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkDeviceGroupPresentCapabilitiesKHR, presentMask}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "modes" VkDeviceGroupPresentCapabilitiesKHR where
        type FieldType "modes" VkDeviceGroupPresentCapabilitiesKHR =
             VkDeviceGroupPresentModeFlagsKHR
        type FieldOptional "modes" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "modes" VkDeviceGroupPresentCapabilitiesKHR =
             #{offset VkDeviceGroupPresentCapabilitiesKHR, modes}
        type FieldIsArray "modes" VkDeviceGroupPresentCapabilitiesKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentCapabilitiesKHR, modes}

instance {-# OVERLAPPING #-}
         CanReadField "modes" VkDeviceGroupPresentCapabilitiesKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentCapabilitiesKHR, modes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, modes}

instance {-# OVERLAPPING #-}
         CanWriteField "modes" VkDeviceGroupPresentCapabilitiesKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentCapabilitiesKHR, modes}

instance Show VkDeviceGroupPresentCapabilitiesKHR where
        showsPrec d x
          = showString "VkDeviceGroupPresentCapabilitiesKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          (showString "presentMask = [" .
                             showsPrec d
                               (let s = sizeOf
                                          (undefined ::
                                             FieldType "presentMask"
                                               VkDeviceGroupPresentCapabilitiesKHR)
                                    o = fieldOffset @"presentMask"
                                          @VkDeviceGroupPresentCapabilitiesKHR
                                    f i
                                      = peekByteOff (unsafePtr x) i ::
                                          IO
                                            (FieldType "presentMask"
                                               VkDeviceGroupPresentCapabilitiesKHR)
                                  in
                                  unsafeDupablePerformIO . mapM f $
                                    map (\ i -> o + i * s) [0 .. VK_MAX_DEVICE_GROUP_SIZE - 1])
                               . showChar ']')
                            .
                            showString ", " .
                              showString "modes = " .
                                showsPrec d (getField @"modes" x) . showChar '}'

-- | > typedef struct VkDeviceGroupPresentInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         swapchainCount;
--   >     const uint32_t* pDeviceMasks;
--   >     VkDeviceGroupPresentModeFlagBitsKHR mode;
--   > } VkDeviceGroupPresentInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupPresentInfoKHR VkDeviceGroupPresentInfoKHR registry at www.khronos.org>
data VkDeviceGroupPresentInfoKHR = VkDeviceGroupPresentInfoKHR## Addr##
                                                                ByteArray##

instance Eq VkDeviceGroupPresentInfoKHR where
        (VkDeviceGroupPresentInfoKHR## a _) ==
          x@(VkDeviceGroupPresentInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupPresentInfoKHR where
        (VkDeviceGroupPresentInfoKHR## a _) `compare`
          x@(VkDeviceGroupPresentInfoKHR## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupPresentInfoKHR where
        sizeOf ~_ = #{size VkDeviceGroupPresentInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupPresentInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupPresentInfoKHR where
        unsafeAddr (VkDeviceGroupPresentInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupPresentInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupPresentInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupPresentInfoKHR where
        type StructFields VkDeviceGroupPresentInfoKHR =
             '["sType", "pNext", "swapchainCount", "pDeviceMasks", "mode"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupPresentInfoKHR =
             '[VkPresentInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupPresentInfoKHR where
        type FieldType "sType" VkDeviceGroupPresentInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupPresentInfoKHR =
             #{offset VkDeviceGroupPresentInfoKHR, sType}
        type FieldIsArray "sType" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupPresentInfoKHR where
        type FieldType "pNext" VkDeviceGroupPresentInfoKHR = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupPresentInfoKHR =
             #{offset VkDeviceGroupPresentInfoKHR, pNext}
        type FieldIsArray "pNext" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "swapchainCount" VkDeviceGroupPresentInfoKHR where
        type FieldType "swapchainCount" VkDeviceGroupPresentInfoKHR =
             Word32
        type FieldOptional "swapchainCount" VkDeviceGroupPresentInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "swapchainCount" VkDeviceGroupPresentInfoKHR =
             #{offset VkDeviceGroupPresentInfoKHR, swapchainCount}
        type FieldIsArray "swapchainCount" VkDeviceGroupPresentInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         CanReadField "swapchainCount" VkDeviceGroupPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHR, swapchainCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         CanWriteField "swapchainCount" VkDeviceGroupPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHR, swapchainCount}

instance {-# OVERLAPPING #-}
         HasField "pDeviceMasks" VkDeviceGroupPresentInfoKHR where
        type FieldType "pDeviceMasks" VkDeviceGroupPresentInfoKHR =
             Ptr Word32
        type FieldOptional "pDeviceMasks" VkDeviceGroupPresentInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceMasks" VkDeviceGroupPresentInfoKHR =
             #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks}
        type FieldIsArray "pDeviceMasks" VkDeviceGroupPresentInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceMasks" VkDeviceGroupPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceMasks" VkDeviceGroupPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHR, pDeviceMasks}

instance {-# OVERLAPPING #-}
         HasField "mode" VkDeviceGroupPresentInfoKHR where
        type FieldType "mode" VkDeviceGroupPresentInfoKHR =
             VkDeviceGroupPresentModeFlagBitsKHR
        type FieldOptional "mode" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs
        type FieldOffset "mode" VkDeviceGroupPresentInfoKHR =
             #{offset VkDeviceGroupPresentInfoKHR, mode}
        type FieldIsArray "mode" VkDeviceGroupPresentInfoKHR = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupPresentInfoKHR, mode}

instance {-# OVERLAPPING #-}
         CanReadField "mode" VkDeviceGroupPresentInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupPresentInfoKHR, mode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupPresentInfoKHR, mode}

instance {-# OVERLAPPING #-}
         CanWriteField "mode" VkDeviceGroupPresentInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupPresentInfoKHR, mode}

instance Show VkDeviceGroupPresentInfoKHR where
        showsPrec d x
          = showString "VkDeviceGroupPresentInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "swapchainCount = " .
                            showsPrec d (getField @"swapchainCount" x) .
                              showString ", " .
                                showString "pDeviceMasks = " .
                                  showsPrec d (getField @"pDeviceMasks" x) .
                                    showString ", " .
                                      showString "mode = " .
                                        showsPrec d (getField @"mode" x) . showChar '}'

-- | > typedef struct VkDeviceGroupRenderPassBeginInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t                         deviceMask;
--   >     uint32_t         deviceRenderAreaCount;
--   >     const VkRect2D*  pDeviceRenderAreas;
--   > } VkDeviceGroupRenderPassBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupRenderPassBeginInfo VkDeviceGroupRenderPassBeginInfo registry at www.khronos.org>
data VkDeviceGroupRenderPassBeginInfo = VkDeviceGroupRenderPassBeginInfo## Addr##
                                                                          ByteArray##

instance Eq VkDeviceGroupRenderPassBeginInfo where
        (VkDeviceGroupRenderPassBeginInfo## a _) ==
          x@(VkDeviceGroupRenderPassBeginInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupRenderPassBeginInfo where
        (VkDeviceGroupRenderPassBeginInfo## a _) `compare`
          x@(VkDeviceGroupRenderPassBeginInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupRenderPassBeginInfo where
        sizeOf ~_ = #{size VkDeviceGroupRenderPassBeginInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupRenderPassBeginInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupRenderPassBeginInfo where
        unsafeAddr (VkDeviceGroupRenderPassBeginInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupRenderPassBeginInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupRenderPassBeginInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupRenderPassBeginInfo where
        type StructFields VkDeviceGroupRenderPassBeginInfo =
             '["sType", "pNext", "deviceMask", "deviceRenderAreaCount", -- ' closing tick for hsc2hs
               "pDeviceRenderAreas"]
        type CUnionType VkDeviceGroupRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupRenderPassBeginInfo =
             '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupRenderPassBeginInfo where
        type FieldType "sType" VkDeviceGroupRenderPassBeginInfo =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupRenderPassBeginInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupRenderPassBeginInfo =
             #{offset VkDeviceGroupRenderPassBeginInfo, sType}
        type FieldIsArray "sType" VkDeviceGroupRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupRenderPassBeginInfo where
        type FieldType "pNext" VkDeviceGroupRenderPassBeginInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupRenderPassBeginInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupRenderPassBeginInfo =
             #{offset VkDeviceGroupRenderPassBeginInfo, pNext}
        type FieldIsArray "pNext" VkDeviceGroupRenderPassBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "deviceMask" VkDeviceGroupRenderPassBeginInfo where
        type FieldType "deviceMask" VkDeviceGroupRenderPassBeginInfo =
             Word32
        type FieldOptional "deviceMask" VkDeviceGroupRenderPassBeginInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "deviceMask" VkDeviceGroupRenderPassBeginInfo =
             #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask}
        type FieldIsArray "deviceMask" VkDeviceGroupRenderPassBeginInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanReadField "deviceMask" VkDeviceGroupRenderPassBeginInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceMask" VkDeviceGroupRenderPassBeginInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, deviceMask}

instance {-# OVERLAPPING #-}
         HasField "deviceRenderAreaCount" VkDeviceGroupRenderPassBeginInfo
         where
        type FieldType "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfo
             = Word32
        type FieldOptional "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfo
             =
             #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount}
        type FieldIsArray "deviceRenderAreaCount"
               VkDeviceGroupRenderPassBeginInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount}

instance {-# OVERLAPPING #-}
         CanReadField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount}

instance {-# OVERLAPPING #-}
         CanWriteField "deviceRenderAreaCount"
           VkDeviceGroupRenderPassBeginInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, deviceRenderAreaCount}

instance {-# OVERLAPPING #-}
         HasField "pDeviceRenderAreas" VkDeviceGroupRenderPassBeginInfo
         where
        type FieldType "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfo
             = Ptr VkRect2D
        type FieldOptional "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfo
             =
             #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas}
        type FieldIsArray "pDeviceRenderAreas"
               VkDeviceGroupRenderPassBeginInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas}

instance {-# OVERLAPPING #-}
         CanReadField "pDeviceRenderAreas" VkDeviceGroupRenderPassBeginInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas}

instance {-# OVERLAPPING #-}
         CanWriteField "pDeviceRenderAreas" VkDeviceGroupRenderPassBeginInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupRenderPassBeginInfo, pDeviceRenderAreas}

instance Show VkDeviceGroupRenderPassBeginInfo where
        showsPrec d x
          = showString "VkDeviceGroupRenderPassBeginInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "deviceMask = " .
                            showsPrec d (getField @"deviceMask" x) .
                              showString ", " .
                                showString "deviceRenderAreaCount = " .
                                  showsPrec d (getField @"deviceRenderAreaCount" x) .
                                    showString ", " .
                                      showString "pDeviceRenderAreas = " .
                                        showsPrec d (getField @"pDeviceRenderAreas" x) .
                                          showChar '}'

-- | Alias for `VkDeviceGroupRenderPassBeginInfo`
type VkDeviceGroupRenderPassBeginInfoKHR =
     VkDeviceGroupRenderPassBeginInfo

-- | > typedef struct VkDeviceGroupSubmitInfo {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         waitSemaphoreCount;
--   >     const uint32_t*    pWaitSemaphoreDeviceIndices;
--   >     uint32_t         commandBufferCount;
--   >     const uint32_t*    pCommandBufferDeviceMasks;
--   >     uint32_t         signalSemaphoreCount;
--   >     const uint32_t*  pSignalSemaphoreDeviceIndices;
--   > } VkDeviceGroupSubmitInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupSubmitInfo VkDeviceGroupSubmitInfo registry at www.khronos.org>
data VkDeviceGroupSubmitInfo = VkDeviceGroupSubmitInfo## Addr##
                                                        ByteArray##

instance Eq VkDeviceGroupSubmitInfo where
        (VkDeviceGroupSubmitInfo## a _) == x@(VkDeviceGroupSubmitInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupSubmitInfo where
        (VkDeviceGroupSubmitInfo## a _) `compare`
          x@(VkDeviceGroupSubmitInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupSubmitInfo where
        sizeOf ~_ = #{size VkDeviceGroupSubmitInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceGroupSubmitInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupSubmitInfo where
        unsafeAddr (VkDeviceGroupSubmitInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupSubmitInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupSubmitInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupSubmitInfo where
        type StructFields VkDeviceGroupSubmitInfo =
             '["sType", "pNext", "waitSemaphoreCount", -- ' closing tick for hsc2hs
               "pWaitSemaphoreDeviceIndices", "commandBufferCount",
               "pCommandBufferDeviceMasks", "signalSemaphoreCount",
               "pSignalSemaphoreDeviceIndices"]
        type CUnionType VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupSubmitInfo = '[VkSubmitInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupSubmitInfo where
        type FieldType "sType" VkDeviceGroupSubmitInfo = VkStructureType
        type FieldOptional "sType" VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupSubmitInfo =
             #{offset VkDeviceGroupSubmitInfo, sType}
        type FieldIsArray "sType" VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceGroupSubmitInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupSubmitInfo where
        type FieldType "pNext" VkDeviceGroupSubmitInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupSubmitInfo =
             #{offset VkDeviceGroupSubmitInfo, pNext}
        type FieldIsArray "pNext" VkDeviceGroupSubmitInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceGroupSubmitInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "waitSemaphoreCount" VkDeviceGroupSubmitInfo where
        type FieldType "waitSemaphoreCount" VkDeviceGroupSubmitInfo =
             Word32
        type FieldOptional "waitSemaphoreCount" VkDeviceGroupSubmitInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "waitSemaphoreCount" VkDeviceGroupSubmitInfo =
             #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount}
        type FieldIsArray "waitSemaphoreCount" VkDeviceGroupSubmitInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanReadField "waitSemaphoreCount" VkDeviceGroupSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "waitSemaphoreCount" VkDeviceGroupSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, waitSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "pWaitSemaphoreDeviceIndices" VkDeviceGroupSubmitInfo
         where
        type FieldType "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = Ptr Word32
        type FieldOptional "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             =
             #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices}
        type FieldIsArray "pWaitSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pWaitSemaphoreDeviceIndices" VkDeviceGroupSubmitInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pWaitSemaphoreDeviceIndices" VkDeviceGroupSubmitInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, pWaitSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         HasField "commandBufferCount" VkDeviceGroupSubmitInfo where
        type FieldType "commandBufferCount" VkDeviceGroupSubmitInfo =
             Word32
        type FieldOptional "commandBufferCount" VkDeviceGroupSubmitInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "commandBufferCount" VkDeviceGroupSubmitInfo =
             #{offset VkDeviceGroupSubmitInfo, commandBufferCount}
        type FieldIsArray "commandBufferCount" VkDeviceGroupSubmitInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         CanReadField "commandBufferCount" VkDeviceGroupSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, commandBufferCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         CanWriteField "commandBufferCount" VkDeviceGroupSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, commandBufferCount}

instance {-# OVERLAPPING #-}
         HasField "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfo where
        type FieldType "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfo
             = Ptr Word32
        type FieldOptional "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfo
             =
             #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks}
        type FieldIsArray "pCommandBufferDeviceMasks"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks}

instance {-# OVERLAPPING #-}
         CanReadField "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks}

instance {-# OVERLAPPING #-}
         CanWriteField "pCommandBufferDeviceMasks" VkDeviceGroupSubmitInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, pCommandBufferDeviceMasks}

instance {-# OVERLAPPING #-}
         HasField "signalSemaphoreCount" VkDeviceGroupSubmitInfo where
        type FieldType "signalSemaphoreCount" VkDeviceGroupSubmitInfo =
             Word32
        type FieldOptional "signalSemaphoreCount" VkDeviceGroupSubmitInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "signalSemaphoreCount" VkDeviceGroupSubmitInfo =
             #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount}
        type FieldIsArray "signalSemaphoreCount" VkDeviceGroupSubmitInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanReadField "signalSemaphoreCount" VkDeviceGroupSubmitInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         CanWriteField "signalSemaphoreCount" VkDeviceGroupSubmitInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, signalSemaphoreCount}

instance {-# OVERLAPPING #-}
         HasField "pSignalSemaphoreDeviceIndices" VkDeviceGroupSubmitInfo
         where
        type FieldType "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = Ptr Word32
        type FieldOptional "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             =
             #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices}
        type FieldIsArray "pSignalSemaphoreDeviceIndices"
               VkDeviceGroupSubmitInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         CanReadField "pSignalSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices}

instance {-# OVERLAPPING #-}
         CanWriteField "pSignalSemaphoreDeviceIndices"
           VkDeviceGroupSubmitInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSubmitInfo, pSignalSemaphoreDeviceIndices}

instance Show VkDeviceGroupSubmitInfo where
        showsPrec d x
          = showString "VkDeviceGroupSubmitInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "waitSemaphoreCount = " .
                            showsPrec d (getField @"waitSemaphoreCount" x) .
                              showString ", " .
                                showString "pWaitSemaphoreDeviceIndices = " .
                                  showsPrec d (getField @"pWaitSemaphoreDeviceIndices" x) .
                                    showString ", " .
                                      showString "commandBufferCount = " .
                                        showsPrec d (getField @"commandBufferCount" x) .
                                          showString ", " .
                                            showString "pCommandBufferDeviceMasks = " .
                                              showsPrec d (getField @"pCommandBufferDeviceMasks" x)
                                                .
                                                showString ", " .
                                                  showString "signalSemaphoreCount = " .
                                                    showsPrec d (getField @"signalSemaphoreCount" x)
                                                      .
                                                      showString ", " .
                                                        showString
                                                          "pSignalSemaphoreDeviceIndices = "
                                                          .
                                                          showsPrec d
                                                            (getField
                                                               @"pSignalSemaphoreDeviceIndices"
                                                               x)
                                                            . showChar '}'

-- | Alias for `VkDeviceGroupSubmitInfo`
type VkDeviceGroupSubmitInfoKHR = VkDeviceGroupSubmitInfo

-- | > typedef struct VkDeviceGroupSwapchainCreateInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkDeviceGroupPresentModeFlagsKHR                         modes;
--   > } VkDeviceGroupSwapchainCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceGroupSwapchainCreateInfoKHR VkDeviceGroupSwapchainCreateInfoKHR registry at www.khronos.org>
data VkDeviceGroupSwapchainCreateInfoKHR = VkDeviceGroupSwapchainCreateInfoKHR## Addr##
                                                                                ByteArray##

instance Eq VkDeviceGroupSwapchainCreateInfoKHR where
        (VkDeviceGroupSwapchainCreateInfoKHR## a _) ==
          x@(VkDeviceGroupSwapchainCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceGroupSwapchainCreateInfoKHR where
        (VkDeviceGroupSwapchainCreateInfoKHR## a _) `compare`
          x@(VkDeviceGroupSwapchainCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceGroupSwapchainCreateInfoKHR where
        sizeOf ~_ = #{size VkDeviceGroupSwapchainCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceGroupSwapchainCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceGroupSwapchainCreateInfoKHR
         where
        unsafeAddr (VkDeviceGroupSwapchainCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceGroupSwapchainCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceGroupSwapchainCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceGroupSwapchainCreateInfoKHR where
        type StructFields VkDeviceGroupSwapchainCreateInfoKHR =
             '["sType", "pNext", "modes"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceGroupSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceGroupSwapchainCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceGroupSwapchainCreateInfoKHR =
             '[VkSwapchainCreateInfoKHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceGroupSwapchainCreateInfoKHR where
        type FieldType "sType" VkDeviceGroupSwapchainCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceGroupSwapchainCreateInfoKHR =
             #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType}
        type FieldIsArray "sType" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceGroupSwapchainCreateInfoKHR where
        type FieldType "pNext" VkDeviceGroupSwapchainCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceGroupSwapchainCreateInfoKHR =
             #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "modes" VkDeviceGroupSwapchainCreateInfoKHR where
        type FieldType "modes" VkDeviceGroupSwapchainCreateInfoKHR =
             VkDeviceGroupPresentModeFlagsKHR
        type FieldOptional "modes" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "modes" VkDeviceGroupSwapchainCreateInfoKHR =
             #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes}
        type FieldIsArray "modes" VkDeviceGroupSwapchainCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes}

instance {-# OVERLAPPING #-}
         CanReadField "modes" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes}

instance {-# OVERLAPPING #-}
         CanWriteField "modes" VkDeviceGroupSwapchainCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceGroupSwapchainCreateInfoKHR, modes}

instance Show VkDeviceGroupSwapchainCreateInfoKHR where
        showsPrec d x
          = showString "VkDeviceGroupSwapchainCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "modes = " .
                            showsPrec d (getField @"modes" x) . showChar '}'

-- | > typedef struct VkDeviceQueueCreateInfo {
--   >     VkStructureType sType;
--   >     const void*     pNext;
--   >     VkDeviceQueueCreateFlags    flags;
--   >     uint32_t        queueFamilyIndex;
--   >     uint32_t        queueCount;
--   >     const float*    pQueuePriorities;
--   > } VkDeviceQueueCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceQueueCreateInfo VkDeviceQueueCreateInfo registry at www.khronos.org>
data VkDeviceQueueCreateInfo = VkDeviceQueueCreateInfo## Addr##
                                                        ByteArray##

instance Eq VkDeviceQueueCreateInfo where
        (VkDeviceQueueCreateInfo## a _) == x@(VkDeviceQueueCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceQueueCreateInfo where
        (VkDeviceQueueCreateInfo## a _) `compare`
          x@(VkDeviceQueueCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceQueueCreateInfo where
        sizeOf ~_ = #{size VkDeviceQueueCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceQueueCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceQueueCreateInfo where
        unsafeAddr (VkDeviceQueueCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceQueueCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceQueueCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceQueueCreateInfo where
        type StructFields VkDeviceQueueCreateInfo =
             '["sType", "pNext", "flags", "queueFamilyIndex", "queueCount", -- ' closing tick for hsc2hs
               "pQueuePriorities"]
        type CUnionType VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceQueueCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceQueueCreateInfo where
        type FieldType "sType" VkDeviceQueueCreateInfo = VkStructureType
        type FieldOptional "sType" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, sType}
        type FieldIsArray "sType" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceQueueCreateInfo where
        type FieldType "pNext" VkDeviceQueueCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, pNext}
        type FieldIsArray "pNext" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDeviceQueueCreateInfo where
        type FieldType "flags" VkDeviceQueueCreateInfo =
             VkDeviceQueueCreateFlags
        type FieldOptional "flags" VkDeviceQueueCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, flags}
        type FieldIsArray "flags" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndex" VkDeviceQueueCreateInfo where
        type FieldType "queueFamilyIndex" VkDeviceQueueCreateInfo = Word32
        type FieldOptional "queueFamilyIndex" VkDeviceQueueCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndex" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}
        type FieldIsArray "queueFamilyIndex" VkDeviceQueueCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyIndex" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, queueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyIndex" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "queueCount" VkDeviceQueueCreateInfo where
        type FieldType "queueCount" VkDeviceQueueCreateInfo = Word32
        type FieldOptional "queueCount" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueCount" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, queueCount}
        type FieldIsArray "queueCount" VkDeviceQueueCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueCreateInfo, queueCount}

instance {-# OVERLAPPING #-}
         CanReadField "queueCount" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, queueCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, queueCount}

instance {-# OVERLAPPING #-}
         CanWriteField "queueCount" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, queueCount}

instance {-# OVERLAPPING #-}
         HasField "pQueuePriorities" VkDeviceQueueCreateInfo where
        type FieldType "pQueuePriorities" VkDeviceQueueCreateInfo =
             Ptr #{type float}
        type FieldOptional "pQueuePriorities" VkDeviceQueueCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pQueuePriorities" VkDeviceQueueCreateInfo =
             #{offset VkDeviceQueueCreateInfo, pQueuePriorities}
        type FieldIsArray "pQueuePriorities" VkDeviceQueueCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueCreateInfo, pQueuePriorities}

instance {-# OVERLAPPING #-}
         CanReadField "pQueuePriorities" VkDeviceQueueCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, pQueuePriorities})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, pQueuePriorities}

instance {-# OVERLAPPING #-}
         CanWriteField "pQueuePriorities" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, pQueuePriorities}

instance Show VkDeviceQueueCreateInfo where
        showsPrec d x
          = showString "VkDeviceQueueCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "queueFamilyIndex = " .
                                  showsPrec d (getField @"queueFamilyIndex" x) .
                                    showString ", " .
                                      showString "queueCount = " .
                                        showsPrec d (getField @"queueCount" x) .
                                          showString ", " .
                                            showString "pQueuePriorities = " .
                                              showsPrec d (getField @"pQueuePriorities" x) .
                                                showChar '}'

-- | > typedef struct VkDeviceQueueGlobalPriorityCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                    pNext;
--   >     VkQueueGlobalPriorityEXT       globalPriority;
--   > } VkDeviceQueueGlobalPriorityCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceQueueGlobalPriorityCreateInfoEXT VkDeviceQueueGlobalPriorityCreateInfoEXT registry at www.khronos.org>
data VkDeviceQueueGlobalPriorityCreateInfoEXT = VkDeviceQueueGlobalPriorityCreateInfoEXT## Addr##
                                                                                          ByteArray##

instance Eq VkDeviceQueueGlobalPriorityCreateInfoEXT where
        (VkDeviceQueueGlobalPriorityCreateInfoEXT## a _) ==
          x@(VkDeviceQueueGlobalPriorityCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceQueueGlobalPriorityCreateInfoEXT where
        (VkDeviceQueueGlobalPriorityCreateInfoEXT## a _) `compare`
          x@(VkDeviceQueueGlobalPriorityCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceQueueGlobalPriorityCreateInfoEXT where
        sizeOf ~_
          = #{size VkDeviceQueueGlobalPriorityCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDeviceQueueGlobalPriorityCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        unsafeAddr (VkDeviceQueueGlobalPriorityCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceQueueGlobalPriorityCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceQueueGlobalPriorityCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        type StructFields VkDeviceQueueGlobalPriorityCreateInfoEXT =
             '["sType", "pNext", "globalPriority"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceQueueGlobalPriorityCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceQueueGlobalPriorityCreateInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceQueueGlobalPriorityCreateInfoEXT =
             '[VkDeviceQueueCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT where
        type FieldType "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}
        type FieldIsArray "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT where
        type FieldType "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT =
             #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}
        type FieldIsArray "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "globalPriority" VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        type FieldType "globalPriority"
               VkDeviceQueueGlobalPriorityCreateInfoEXT
             = VkQueueGlobalPriorityEXT
        type FieldOptional "globalPriority"
               VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "globalPriority"
               VkDeviceQueueGlobalPriorityCreateInfoEXT
             =
             #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}
        type FieldIsArray "globalPriority"
               VkDeviceQueueGlobalPriorityCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

instance {-# OVERLAPPING #-}
         CanReadField "globalPriority"
           VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

instance {-# OVERLAPPING #-}
         CanWriteField "globalPriority"
           VkDeviceQueueGlobalPriorityCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueGlobalPriorityCreateInfoEXT, globalPriority}

instance Show VkDeviceQueueGlobalPriorityCreateInfoEXT where
        showsPrec d x
          = showString "VkDeviceQueueGlobalPriorityCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "globalPriority = " .
                            showsPrec d (getField @"globalPriority" x) . showChar '}'

-- | > typedef struct VkDeviceQueueInfo2 {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     VkDeviceQueueCreateFlags            flags;
--   >     uint32_t                            queueFamilyIndex;
--   >     uint32_t                            queueIndex;
--   > } VkDeviceQueueInfo2;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDeviceQueueInfo2 VkDeviceQueueInfo2 registry at www.khronos.org>
data VkDeviceQueueInfo2 = VkDeviceQueueInfo2## Addr## ByteArray##

instance Eq VkDeviceQueueInfo2 where
        (VkDeviceQueueInfo2## a _) == x@(VkDeviceQueueInfo2## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDeviceQueueInfo2 where
        (VkDeviceQueueInfo2## a _) `compare` x@(VkDeviceQueueInfo2## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDeviceQueueInfo2 where
        sizeOf ~_ = #{size VkDeviceQueueInfo2}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDeviceQueueInfo2}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDeviceQueueInfo2 where
        unsafeAddr (VkDeviceQueueInfo2## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDeviceQueueInfo2## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDeviceQueueInfo2## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDeviceQueueInfo2 where
        type StructFields VkDeviceQueueInfo2 =
             '["sType", "pNext", "flags", "queueFamilyIndex", "queueIndex"] -- ' closing tick for hsc2hs
        type CUnionType VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDeviceQueueInfo2 = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "sType" VkDeviceQueueInfo2
         where
        type FieldType "sType" VkDeviceQueueInfo2 = VkStructureType
        type FieldOptional "sType" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDeviceQueueInfo2 =
             #{offset VkDeviceQueueInfo2, sType}
        type FieldIsArray "sType" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueInfo2, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDeviceQueueInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueInfo2, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueInfo2, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDeviceQueueInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueInfo2, sType}

instance {-# OVERLAPPING #-} HasField "pNext" VkDeviceQueueInfo2
         where
        type FieldType "pNext" VkDeviceQueueInfo2 = Ptr Void
        type FieldOptional "pNext" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDeviceQueueInfo2 =
             #{offset VkDeviceQueueInfo2, pNext}
        type FieldIsArray "pNext" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDeviceQueueInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueInfo2, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueInfo2, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDeviceQueueInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueInfo2, pNext}

instance {-# OVERLAPPING #-} HasField "flags" VkDeviceQueueInfo2
         where
        type FieldType "flags" VkDeviceQueueInfo2 =
             VkDeviceQueueCreateFlags
        type FieldOptional "flags" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDeviceQueueInfo2 =
             #{offset VkDeviceQueueInfo2, flags}
        type FieldIsArray "flags" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueInfo2, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDeviceQueueInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueInfo2, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueInfo2, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDeviceQueueInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueInfo2, flags}

instance {-# OVERLAPPING #-}
         HasField "queueFamilyIndex" VkDeviceQueueInfo2 where
        type FieldType "queueFamilyIndex" VkDeviceQueueInfo2 = Word32
        type FieldOptional "queueFamilyIndex" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueFamilyIndex" VkDeviceQueueInfo2 =
             #{offset VkDeviceQueueInfo2, queueFamilyIndex}
        type FieldIsArray "queueFamilyIndex" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDeviceQueueInfo2, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanReadField "queueFamilyIndex" VkDeviceQueueInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueInfo2, queueFamilyIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueInfo2, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "queueFamilyIndex" VkDeviceQueueInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueInfo2, queueFamilyIndex}

instance {-# OVERLAPPING #-}
         HasField "queueIndex" VkDeviceQueueInfo2 where
        type FieldType "queueIndex" VkDeviceQueueInfo2 = Word32
        type FieldOptional "queueIndex" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs
        type FieldOffset "queueIndex" VkDeviceQueueInfo2 =
             #{offset VkDeviceQueueInfo2, queueIndex}
        type FieldIsArray "queueIndex" VkDeviceQueueInfo2 = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDeviceQueueInfo2, queueIndex}

instance {-# OVERLAPPING #-}
         CanReadField "queueIndex" VkDeviceQueueInfo2 where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueInfo2, queueIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDeviceQueueInfo2, queueIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "queueIndex" VkDeviceQueueInfo2 where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDeviceQueueInfo2, queueIndex}

instance Show VkDeviceQueueInfo2 where
        showsPrec d x
          = showString "VkDeviceQueueInfo2 {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "queueFamilyIndex = " .
                                  showsPrec d (getField @"queueFamilyIndex" x) .
                                    showString ", " .
                                      showString "queueIndex = " .
                                        showsPrec d (getField @"queueIndex" x) . showChar '}'
