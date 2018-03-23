#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceCreateInfo
       (VkDeviceCreateInfo(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                        (VkDeviceCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType            (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo  (VkDeviceQueueCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceFeatures (VkPhysicalDeviceFeatures)
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDeviceCreateInfo.html VkDeviceCreateInfo registry at www.khronos.org>
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
