#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceImageFormatInfo2KHR
       (VkPhysicalDeviceImageFormatInfo2KHR(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkFormat           (VkFormat)
import           Graphics.Vulkan.Types.Enum.VkImageCreateFlags (VkImageCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkImageTiling      (VkImageTiling)
import           Graphics.Vulkan.Types.Enum.VkImageType        (VkImageType)
import           Graphics.Vulkan.Types.Enum.VkImageUsageFlags  (VkImageUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceImageFormatInfo2KHR {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkFormat                         format;
--   >     VkImageType                      type;
--   >     VkImageTiling                    tiling;
--   >     VkImageUsageFlags                usage;
--   >     VkImageCreateFlags flags;
--   > } VkPhysicalDeviceImageFormatInfo2KHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceImageFormatInfo2KHR.html VkPhysicalDeviceImageFormatInfo2KHR registry at www.khronos.org>
data VkPhysicalDeviceImageFormatInfo2KHR = VkPhysicalDeviceImageFormatInfo2KHR## Addr##
                                                                                ByteArray##

instance Eq VkPhysicalDeviceImageFormatInfo2KHR where
        (VkPhysicalDeviceImageFormatInfo2KHR## a _) ==
          x@(VkPhysicalDeviceImageFormatInfo2KHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceImageFormatInfo2KHR where
        (VkPhysicalDeviceImageFormatInfo2KHR## a _) `compare`
          x@(VkPhysicalDeviceImageFormatInfo2KHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceImageFormatInfo2KHR where
        sizeOf ~_ = #{size VkPhysicalDeviceImageFormatInfo2KHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceImageFormatInfo2KHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceImageFormatInfo2KHR
         where
        unsafeAddr (VkPhysicalDeviceImageFormatInfo2KHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceImageFormatInfo2KHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceImageFormatInfo2KHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceImageFormatInfo2KHR where
        type StructFields VkPhysicalDeviceImageFormatInfo2KHR =
             '["sType", "pNext", "format", "type", "tiling", "usage", "flags"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceImageFormatInfo2KHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceImageFormatInfo2KHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceImageFormatInfo2KHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceImageFormatInfo2KHR where
        type VkSTypeMType VkPhysicalDeviceImageFormatInfo2KHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "sType" VkPhysicalDeviceImageFormatInfo2KHR =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}
        type FieldIsArray "sType" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, sType}

instance CanReadField "sType" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceImageFormatInfo2KHR where
        type VkPNextMType VkPhysicalDeviceImageFormatInfo2KHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "pNext" VkPhysicalDeviceImageFormatInfo2KHR =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, pNext}

instance CanReadField "pNext" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFormat VkPhysicalDeviceImageFormatInfo2KHR where
        type VkFormatMType VkPhysicalDeviceImageFormatInfo2KHR = VkFormat

        {-# NOINLINE vkFormat #-}
        vkFormat x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, format})

        {-# INLINE vkFormatByteOffset #-}
        vkFormatByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}

        {-# INLINE readVkFormat #-}
        readVkFormat p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}

        {-# INLINE writeVkFormat #-}
        writeVkFormat p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}

instance {-# OVERLAPPING #-}
         HasField "format" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "format" VkPhysicalDeviceImageFormatInfo2KHR =
             VkFormat
        type FieldOptional "format" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "format" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}
        type FieldIsArray "format" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, format}

instance CanReadField "format" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkFormat

        {-# INLINE readField #-}
        readField = readVkFormat

instance CanWriteField "format" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFormat

instance {-# OVERLAPPING #-}
         HasVkType VkPhysicalDeviceImageFormatInfo2KHR where
        type VkTypeMType VkPhysicalDeviceImageFormatInfo2KHR = VkImageType

        {-# NOINLINE vkType #-}
        vkType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, type})

        {-# INLINE vkTypeByteOffset #-}
        vkTypeByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}

        {-# INLINE readVkType #-}
        readVkType p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}

        {-# INLINE writeVkType #-}
        writeVkType p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}

instance {-# OVERLAPPING #-}
         HasField "type" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "type" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageType
        type FieldOptional "type" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "type" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}
        type FieldIsArray "type" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, type}

instance CanReadField "type" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkType

        {-# INLINE readField #-}
        readField = readVkType

instance CanWriteField "type" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkType

instance {-# OVERLAPPING #-}
         HasVkTiling VkPhysicalDeviceImageFormatInfo2KHR where
        type VkTilingMType VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageTiling

        {-# NOINLINE vkTiling #-}
        vkTiling x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling})

        {-# INLINE vkTilingByteOffset #-}
        vkTilingByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}

        {-# INLINE readVkTiling #-}
        readVkTiling p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}

        {-# INLINE writeVkTiling #-}
        writeVkTiling p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}

instance {-# OVERLAPPING #-}
         HasField "tiling" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "tiling" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageTiling
        type FieldOptional "tiling" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "tiling" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}
        type FieldIsArray "tiling" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, tiling}

instance CanReadField "tiling" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkTiling

        {-# INLINE readField #-}
        readField = readVkTiling

instance CanWriteField "tiling" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkTiling

instance {-# OVERLAPPING #-}
         HasVkUsage VkPhysicalDeviceImageFormatInfo2KHR where
        type VkUsageMType VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageUsageFlags

        {-# NOINLINE vkUsage #-}
        vkUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage})

        {-# INLINE vkUsageByteOffset #-}
        vkUsageByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}

        {-# INLINE readVkUsage #-}
        readVkUsage p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}

        {-# INLINE writeVkUsage #-}
        writeVkUsage p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}

instance {-# OVERLAPPING #-}
         HasField "usage" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "usage" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageUsageFlags
        type FieldOptional "usage" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "usage" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}
        type FieldIsArray "usage" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, usage}

instance CanReadField "usage" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkUsage

        {-# INLINE readField #-}
        readField = readVkUsage

instance CanWriteField "usage" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkUsage

instance {-# OVERLAPPING #-}
         HasVkFlags VkPhysicalDeviceImageFormatInfo2KHR where
        type VkFlagsMType VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPhysicalDeviceImageFormatInfo2KHR where
        type FieldType "flags" VkPhysicalDeviceImageFormatInfo2KHR =
             VkImageCreateFlags
        type FieldOptional "flags" VkPhysicalDeviceImageFormatInfo2KHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPhysicalDeviceImageFormatInfo2KHR =
             #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}
        type FieldIsArray "flags" VkPhysicalDeviceImageFormatInfo2KHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceImageFormatInfo2KHR, flags}

instance CanReadField "flags" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkPhysicalDeviceImageFormatInfo2KHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance Show VkPhysicalDeviceImageFormatInfo2KHR where
        showsPrec d x
          = showString "VkPhysicalDeviceImageFormatInfo2KHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFormat = " .
                            showsPrec d (vkFormat x) .
                              showString ", " .
                                showString "vkType = " .
                                  showsPrec d (vkType x) .
                                    showString ", " .
                                      showString "vkTiling = " .
                                        showsPrec d (vkTiling x) .
                                          showString ", " .
                                            showString "vkUsage = " .
                                              showsPrec d (vkUsage x) .
                                                showString ", " .
                                                  showString "vkFlags = " .
                                                    showsPrec d (vkFlags x) . showChar '}'
