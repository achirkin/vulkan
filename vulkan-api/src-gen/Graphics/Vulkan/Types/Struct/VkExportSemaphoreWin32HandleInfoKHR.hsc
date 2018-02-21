#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkExportSemaphoreWin32HandleInfoKHR
       (VkExportSemaphoreWin32HandleInfoKHR(..)) where
import           Foreign.Storable                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType         (VkStructureType)
import           Graphics.Vulkan.Types.Include                      (DWORD,
                                                                     LPCWSTR,
                                                                     SECURITY_ATTRIBUTES)
import           Graphics.Vulkan.Types.Struct.VkSemaphoreCreateInfo (VkSemaphoreCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                   (unsafeDupablePerformIO)

-- | > typedef struct VkExportSemaphoreWin32HandleInfoKHR {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     const SECURITY_ATTRIBUTES*       pAttributes;
--   >     DWORD                            dwAccess;
--   >     LPCWSTR                          name;
--   > } VkExportSemaphoreWin32HandleInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExportSemaphoreWin32HandleInfoKHR.html VkExportSemaphoreWin32HandleInfoKHR registry at www.khronos.org>
data VkExportSemaphoreWin32HandleInfoKHR = VkExportSemaphoreWin32HandleInfoKHR## Addr##
                                                                                ByteArray##

instance Eq VkExportSemaphoreWin32HandleInfoKHR where
        (VkExportSemaphoreWin32HandleInfoKHR## a _) ==
          x@(VkExportSemaphoreWin32HandleInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExportSemaphoreWin32HandleInfoKHR where
        (VkExportSemaphoreWin32HandleInfoKHR## a _) `compare`
          x@(VkExportSemaphoreWin32HandleInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExportSemaphoreWin32HandleInfoKHR where
        sizeOf ~_ = #{size VkExportSemaphoreWin32HandleInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkExportSemaphoreWin32HandleInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExportSemaphoreWin32HandleInfoKHR
         where
        unsafeAddr (VkExportSemaphoreWin32HandleInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExportSemaphoreWin32HandleInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExportSemaphoreWin32HandleInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExportSemaphoreWin32HandleInfoKHR where
        type StructFields VkExportSemaphoreWin32HandleInfoKHR =
             '["sType", "pNext", "pAttributes", "dwAccess", "name"] -- ' closing tick for hsc2hs
        type CUnionType VkExportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExportSemaphoreWin32HandleInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkExportSemaphoreWin32HandleInfoKHR =
             '[VkSemaphoreCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkExportSemaphoreWin32HandleInfoKHR where
        type VkSTypeMType VkExportSemaphoreWin32HandleInfoKHR =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "sType" VkExportSemaphoreWin32HandleInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}
        type FieldIsArray "sType" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, sType}

instance CanReadField "sType" VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkExportSemaphoreWin32HandleInfoKHR where
        type VkPNextMType VkExportSemaphoreWin32HandleInfoKHR = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}
        type FieldIsArray "pNext" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, pNext}

instance CanReadField "pNext" VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkPAttributes VkExportSemaphoreWin32HandleInfoKHR where
        type VkPAttributesMType VkExportSemaphoreWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES

        {-# NOINLINE vkPAttributes #-}
        vkPAttributes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes})

        {-# INLINE vkPAttributesByteOffset #-}
        vkPAttributesByteOffset ~_
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

        {-# INLINE readVkPAttributes #-}
        readVkPAttributes p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

        {-# INLINE writeVkPAttributes #-}
        writeVkPAttributes p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

instance {-# OVERLAPPING #-}
         HasField "pAttributes" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "pAttributes" VkExportSemaphoreWin32HandleInfoKHR =
             Ptr SECURITY_ATTRIBUTES
        type FieldOptional "pAttributes"
               VkExportSemaphoreWin32HandleInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pAttributes" VkExportSemaphoreWin32HandleInfoKHR
             =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}
        type FieldIsArray "pAttributes" VkExportSemaphoreWin32HandleInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, pAttributes}

instance CanReadField "pAttributes"
           VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkPAttributes

        {-# INLINE readField #-}
        readField = readVkPAttributes

instance CanWriteField "pAttributes"
           VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkPAttributes

instance {-# OVERLAPPING #-}
         HasVkDwAccess VkExportSemaphoreWin32HandleInfoKHR where
        type VkDwAccessMType VkExportSemaphoreWin32HandleInfoKHR = DWORD

        {-# NOINLINE vkDwAccess #-}
        vkDwAccess x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess})

        {-# INLINE vkDwAccessByteOffset #-}
        vkDwAccessByteOffset ~_
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

        {-# INLINE readVkDwAccess #-}
        readVkDwAccess p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

        {-# INLINE writeVkDwAccess #-}
        writeVkDwAccess p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

instance {-# OVERLAPPING #-}
         HasField "dwAccess" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             DWORD
        type FieldOptional "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}
        type FieldIsArray "dwAccess" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, dwAccess}

instance CanReadField "dwAccess"
           VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkDwAccess

        {-# INLINE readField #-}
        readField = readVkDwAccess

instance CanWriteField "dwAccess"
           VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkDwAccess

instance {-# OVERLAPPING #-}
         HasVkName VkExportSemaphoreWin32HandleInfoKHR where
        type VkNameMType VkExportSemaphoreWin32HandleInfoKHR = LPCWSTR

        {-# NOINLINE vkName #-}
        vkName x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExportSemaphoreWin32HandleInfoKHR, name})

        {-# INLINE vkNameByteOffset #-}
        vkNameByteOffset ~_
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

        {-# INLINE readVkName #-}
        readVkName p
          = peekByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

        {-# INLINE writeVkName #-}
        writeVkName p
          = pokeByteOff p #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

instance {-# OVERLAPPING #-}
         HasField "name" VkExportSemaphoreWin32HandleInfoKHR where
        type FieldType "name" VkExportSemaphoreWin32HandleInfoKHR = LPCWSTR
        type FieldOptional "name" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "name" VkExportSemaphoreWin32HandleInfoKHR =
             #{offset VkExportSemaphoreWin32HandleInfoKHR, name}
        type FieldIsArray "name" VkExportSemaphoreWin32HandleInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExportSemaphoreWin32HandleInfoKHR, name}

instance CanReadField "name" VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE getField #-}
        getField = vkName

        {-# INLINE readField #-}
        readField = readVkName

instance CanWriteField "name" VkExportSemaphoreWin32HandleInfoKHR
         where
        {-# INLINE writeField #-}
        writeField = writeVkName

instance Show VkExportSemaphoreWin32HandleInfoKHR where
        showsPrec d x
          = showString "VkExportSemaphoreWin32HandleInfoKHR {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPAttributes = " .
                            showsPrec d (vkPAttributes x) .
                              showString ", " .
                                showString "vkDwAccess = " .
                                  showsPrec d (vkDwAccess x) .
                                    showString ", " .
                                      showString "vkName = " . showsPrec d (vkName x) . showChar '}'
