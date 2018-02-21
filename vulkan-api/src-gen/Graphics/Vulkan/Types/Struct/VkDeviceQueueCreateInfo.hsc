#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDeviceQueueCreateInfo
       (VkDeviceQueueCreateInfo(..)) where
import           Foreign.Storable                           (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks             (VkDeviceQueueCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                           (unsafeDupablePerformIO)

-- | > typedef struct VkDeviceQueueCreateInfo {
--   >     VkStructureType sType;
--   >     const void*     pNext;
--   >     VkDeviceQueueCreateFlags    flags;
--   >     uint32_t        queueFamilyIndex;
--   >     uint32_t        queueCount;
--   >     const float*    pQueuePriorities;
--   > } VkDeviceQueueCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDeviceQueueCreateInfo.html VkDeviceQueueCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkSType VkDeviceQueueCreateInfo
         where
        type VkSTypeMType VkDeviceQueueCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDeviceQueueCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, sType}

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

instance CanReadField "sType" VkDeviceQueueCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDeviceQueueCreateInfo
         where
        type VkPNextMType VkDeviceQueueCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDeviceQueueCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, pNext}

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

instance CanReadField "pNext" VkDeviceQueueCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkDeviceQueueCreateInfo
         where
        type VkFlagsMType VkDeviceQueueCreateInfo =
             VkDeviceQueueCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDeviceQueueCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, flags}

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

instance CanReadField "flags" VkDeviceQueueCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkQueueFamilyIndex VkDeviceQueueCreateInfo where
        type VkQueueFamilyIndexMType VkDeviceQueueCreateInfo = Word32

        {-# NOINLINE vkQueueFamilyIndex #-}
        vkQueueFamilyIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, queueFamilyIndex})

        {-# INLINE vkQueueFamilyIndexByteOffset #-}
        vkQueueFamilyIndexByteOffset ~_
          = #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}

        {-# INLINE readVkQueueFamilyIndex #-}
        readVkQueueFamilyIndex p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}

        {-# INLINE writeVkQueueFamilyIndex #-}
        writeVkQueueFamilyIndex p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, queueFamilyIndex}

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

instance CanReadField "queueFamilyIndex" VkDeviceQueueCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkQueueFamilyIndex

        {-# INLINE readField #-}
        readField = readVkQueueFamilyIndex

instance CanWriteField "queueFamilyIndex" VkDeviceQueueCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkQueueFamilyIndex

instance {-# OVERLAPPING #-}
         HasVkQueueCount VkDeviceQueueCreateInfo where
        type VkQueueCountMType VkDeviceQueueCreateInfo = Word32

        {-# NOINLINE vkQueueCount #-}
        vkQueueCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, queueCount})

        {-# INLINE vkQueueCountByteOffset #-}
        vkQueueCountByteOffset ~_
          = #{offset VkDeviceQueueCreateInfo, queueCount}

        {-# INLINE readVkQueueCount #-}
        readVkQueueCount p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, queueCount}

        {-# INLINE writeVkQueueCount #-}
        writeVkQueueCount p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, queueCount}

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

instance CanReadField "queueCount" VkDeviceQueueCreateInfo where
        {-# INLINE getField #-}
        getField = vkQueueCount

        {-# INLINE readField #-}
        readField = readVkQueueCount

instance CanWriteField "queueCount" VkDeviceQueueCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkQueueCount

instance {-# OVERLAPPING #-}
         HasVkPQueuePriorities VkDeviceQueueCreateInfo where
        type VkPQueuePrioritiesMType VkDeviceQueueCreateInfo =
             Ptr #{type float}

        {-# NOINLINE vkPQueuePriorities #-}
        vkPQueuePriorities x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDeviceQueueCreateInfo, pQueuePriorities})

        {-# INLINE vkPQueuePrioritiesByteOffset #-}
        vkPQueuePrioritiesByteOffset ~_
          = #{offset VkDeviceQueueCreateInfo, pQueuePriorities}

        {-# INLINE readVkPQueuePriorities #-}
        readVkPQueuePriorities p
          = peekByteOff p #{offset VkDeviceQueueCreateInfo, pQueuePriorities}

        {-# INLINE writeVkPQueuePriorities #-}
        writeVkPQueuePriorities p
          = pokeByteOff p #{offset VkDeviceQueueCreateInfo, pQueuePriorities}

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

instance CanReadField "pQueuePriorities" VkDeviceQueueCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPQueuePriorities

        {-# INLINE readField #-}
        readField = readVkPQueuePriorities

instance CanWriteField "pQueuePriorities" VkDeviceQueueCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPQueuePriorities

instance Show VkDeviceQueueCreateInfo where
        showsPrec d x
          = showString "VkDeviceQueueCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkQueueFamilyIndex = " .
                                  showsPrec d (vkQueueFamilyIndex x) .
                                    showString ", " .
                                      showString "vkQueueCount = " .
                                        showsPrec d (vkQueueCount x) .
                                          showString ", " .
                                            showString "vkPQueuePriorities = " .
                                              showsPrec d (vkPQueuePriorities x) . showChar '}'
