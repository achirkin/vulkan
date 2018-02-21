#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceCreateInfo.html VkDeviceCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkDeviceCreateInfo where
        type VkSTypeMType VkDeviceCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceCreateInfo, sType}

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

instance CanReadField "sType" VkDeviceCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDeviceCreateInfo where
        type VkPNextMType VkDeviceCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceCreateInfo, pNext}

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

instance CanReadField "pNext" VkDeviceCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkDeviceCreateInfo where
        type VkFlagsMType VkDeviceCreateInfo = VkDeviceCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDeviceCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDeviceCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDeviceCreateInfo, flags}

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

instance CanReadField "flags" VkDeviceCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkQueueCreateInfoCount VkDeviceCreateInfo where
        type VkQueueCreateInfoCountMType VkDeviceCreateInfo = Word32

        {-# NOINLINE vkQueueCreateInfoCount #-}
        vkQueueCreateInfoCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, queueCreateInfoCount})

        {-# INLINE vkQueueCreateInfoCountByteOffset #-}
        vkQueueCreateInfoCountByteOffset ~_
          = #{offset VkDeviceCreateInfo, queueCreateInfoCount}

        {-# INLINE readVkQueueCreateInfoCount #-}
        readVkQueueCreateInfoCount p
          = peekByteOff p #{offset VkDeviceCreateInfo, queueCreateInfoCount}

        {-# INLINE writeVkQueueCreateInfoCount #-}
        writeVkQueueCreateInfoCount p
          = pokeByteOff p #{offset VkDeviceCreateInfo, queueCreateInfoCount}

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

instance CanReadField "queueCreateInfoCount" VkDeviceCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkQueueCreateInfoCount

        {-# INLINE readField #-}
        readField = readVkQueueCreateInfoCount

instance CanWriteField "queueCreateInfoCount" VkDeviceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkQueueCreateInfoCount

instance {-# OVERLAPPING #-}
         HasVkPQueueCreateInfos VkDeviceCreateInfo where
        type VkPQueueCreateInfosMType VkDeviceCreateInfo =
             Ptr VkDeviceQueueCreateInfo

        {-# NOINLINE vkPQueueCreateInfos #-}
        vkPQueueCreateInfos x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, pQueueCreateInfos})

        {-# INLINE vkPQueueCreateInfosByteOffset #-}
        vkPQueueCreateInfosByteOffset ~_
          = #{offset VkDeviceCreateInfo, pQueueCreateInfos}

        {-# INLINE readVkPQueueCreateInfos #-}
        readVkPQueueCreateInfos p
          = peekByteOff p #{offset VkDeviceCreateInfo, pQueueCreateInfos}

        {-# INLINE writeVkPQueueCreateInfos #-}
        writeVkPQueueCreateInfos p
          = pokeByteOff p #{offset VkDeviceCreateInfo, pQueueCreateInfos}

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

instance CanReadField "pQueueCreateInfos" VkDeviceCreateInfo where
        {-# INLINE getField #-}
        getField = vkPQueueCreateInfos

        {-# INLINE readField #-}
        readField = readVkPQueueCreateInfos

instance CanWriteField "pQueueCreateInfos" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPQueueCreateInfos

instance {-# OVERLAPPING #-}
         HasVkEnabledLayerCount VkDeviceCreateInfo where
        type VkEnabledLayerCountMType VkDeviceCreateInfo = Word32

        {-# NOINLINE vkEnabledLayerCount #-}
        vkEnabledLayerCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, enabledLayerCount})

        {-# INLINE vkEnabledLayerCountByteOffset #-}
        vkEnabledLayerCountByteOffset ~_
          = #{offset VkDeviceCreateInfo, enabledLayerCount}

        {-# INLINE readVkEnabledLayerCount #-}
        readVkEnabledLayerCount p
          = peekByteOff p #{offset VkDeviceCreateInfo, enabledLayerCount}

        {-# INLINE writeVkEnabledLayerCount #-}
        writeVkEnabledLayerCount p
          = pokeByteOff p #{offset VkDeviceCreateInfo, enabledLayerCount}

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

instance CanReadField "enabledLayerCount" VkDeviceCreateInfo where
        {-# INLINE getField #-}
        getField = vkEnabledLayerCount

        {-# INLINE readField #-}
        readField = readVkEnabledLayerCount

instance CanWriteField "enabledLayerCount" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkEnabledLayerCount

instance {-# OVERLAPPING #-}
         HasVkPpEnabledLayerNames VkDeviceCreateInfo where
        type VkPpEnabledLayerNamesMType VkDeviceCreateInfo = Ptr CString

        {-# NOINLINE vkPpEnabledLayerNames #-}
        vkPpEnabledLayerNames x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, ppEnabledLayerNames})

        {-# INLINE vkPpEnabledLayerNamesByteOffset #-}
        vkPpEnabledLayerNamesByteOffset ~_
          = #{offset VkDeviceCreateInfo, ppEnabledLayerNames}

        {-# INLINE readVkPpEnabledLayerNames #-}
        readVkPpEnabledLayerNames p
          = peekByteOff p #{offset VkDeviceCreateInfo, ppEnabledLayerNames}

        {-# INLINE writeVkPpEnabledLayerNames #-}
        writeVkPpEnabledLayerNames p
          = pokeByteOff p #{offset VkDeviceCreateInfo, ppEnabledLayerNames}

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

instance CanReadField "ppEnabledLayerNames" VkDeviceCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPpEnabledLayerNames

        {-# INLINE readField #-}
        readField = readVkPpEnabledLayerNames

instance CanWriteField "ppEnabledLayerNames" VkDeviceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPpEnabledLayerNames

instance {-# OVERLAPPING #-}
         HasVkEnabledExtensionCount VkDeviceCreateInfo where
        type VkEnabledExtensionCountMType VkDeviceCreateInfo = Word32

        {-# NOINLINE vkEnabledExtensionCount #-}
        vkEnabledExtensionCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, enabledExtensionCount})

        {-# INLINE vkEnabledExtensionCountByteOffset #-}
        vkEnabledExtensionCountByteOffset ~_
          = #{offset VkDeviceCreateInfo, enabledExtensionCount}

        {-# INLINE readVkEnabledExtensionCount #-}
        readVkEnabledExtensionCount p
          = peekByteOff p #{offset VkDeviceCreateInfo, enabledExtensionCount}

        {-# INLINE writeVkEnabledExtensionCount #-}
        writeVkEnabledExtensionCount p
          = pokeByteOff p #{offset VkDeviceCreateInfo, enabledExtensionCount}

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

instance CanReadField "enabledExtensionCount" VkDeviceCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkEnabledExtensionCount

        {-# INLINE readField #-}
        readField = readVkEnabledExtensionCount

instance CanWriteField "enabledExtensionCount" VkDeviceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkEnabledExtensionCount

instance {-# OVERLAPPING #-}
         HasVkPpEnabledExtensionNames VkDeviceCreateInfo where
        type VkPpEnabledExtensionNamesMType VkDeviceCreateInfo =
             Ptr CString

        {-# NOINLINE vkPpEnabledExtensionNames #-}
        vkPpEnabledExtensionNames x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, ppEnabledExtensionNames})

        {-# INLINE vkPpEnabledExtensionNamesByteOffset #-}
        vkPpEnabledExtensionNamesByteOffset ~_
          = #{offset VkDeviceCreateInfo, ppEnabledExtensionNames}

        {-# INLINE readVkPpEnabledExtensionNames #-}
        readVkPpEnabledExtensionNames p
          = peekByteOff p #{offset VkDeviceCreateInfo, ppEnabledExtensionNames}

        {-# INLINE writeVkPpEnabledExtensionNames #-}
        writeVkPpEnabledExtensionNames p
          = pokeByteOff p #{offset VkDeviceCreateInfo, ppEnabledExtensionNames}

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

instance CanReadField "ppEnabledExtensionNames" VkDeviceCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPpEnabledExtensionNames

        {-# INLINE readField #-}
        readField = readVkPpEnabledExtensionNames

instance CanWriteField "ppEnabledExtensionNames" VkDeviceCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPpEnabledExtensionNames

instance {-# OVERLAPPING #-}
         HasVkPEnabledFeatures VkDeviceCreateInfo where
        type VkPEnabledFeaturesMType VkDeviceCreateInfo =
             Ptr VkPhysicalDeviceFeatures

        {-# NOINLINE vkPEnabledFeatures #-}
        vkPEnabledFeatures x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceCreateInfo, pEnabledFeatures})

        {-# INLINE vkPEnabledFeaturesByteOffset #-}
        vkPEnabledFeaturesByteOffset ~_
          = #{offset VkDeviceCreateInfo, pEnabledFeatures}

        {-# INLINE readVkPEnabledFeatures #-}
        readVkPEnabledFeatures p
          = peekByteOff p #{offset VkDeviceCreateInfo, pEnabledFeatures}

        {-# INLINE writeVkPEnabledFeatures #-}
        writeVkPEnabledFeatures p
          = pokeByteOff p #{offset VkDeviceCreateInfo, pEnabledFeatures}

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

instance CanReadField "pEnabledFeatures" VkDeviceCreateInfo where
        {-# INLINE getField #-}
        getField = vkPEnabledFeatures

        {-# INLINE readField #-}
        readField = readVkPEnabledFeatures

instance CanWriteField "pEnabledFeatures" VkDeviceCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPEnabledFeatures

instance Show VkDeviceCreateInfo where
        showsPrec d x
          = showString "VkDeviceCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkQueueCreateInfoCount = " .
                                  showsPrec d (vkQueueCreateInfoCount x) .
                                    showString ", " .
                                      showString "vkPQueueCreateInfos = " .
                                        showsPrec d (vkPQueueCreateInfos x) .
                                          showString ", " .
                                            showString "vkEnabledLayerCount = " .
                                              showsPrec d (vkEnabledLayerCount x) .
                                                showString ", " .
                                                  showString "vkPpEnabledLayerNames = " .
                                                    showsPrec d (vkPpEnabledLayerNames x) .
                                                      showString ", " .
                                                        showString "vkEnabledExtensionCount = " .
                                                          showsPrec d (vkEnabledExtensionCount x) .
                                                            showString ", " .
                                                              showString
                                                                "vkPpEnabledExtensionNames = "
                                                                .
                                                                showsPrec d
                                                                  (vkPpEnabledExtensionNames x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "vkPEnabledFeatures = "
                                                                      .
                                                                      showsPrec d
                                                                        (vkPEnabledFeatures x)
                                                                        . showChar '}'
