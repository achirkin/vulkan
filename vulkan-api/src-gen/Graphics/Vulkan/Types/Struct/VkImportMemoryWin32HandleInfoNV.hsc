#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportMemoryWin32HandleInfoNV
       (VkImportMemoryWin32HandleInfoNV(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsNV (VkExternalMemoryHandleTypeFlagsNV)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Include                                (HANDLE)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo            (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkImportMemoryWin32HandleInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkExternalMemoryHandleTypeFlagsNV handleType;
--   >     HANDLE                           handle;
--   > } VkImportMemoryWin32HandleInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImportMemoryWin32HandleInfoNV.html VkImportMemoryWin32HandleInfoNV registry at www.khronos.org>
data VkImportMemoryWin32HandleInfoNV = VkImportMemoryWin32HandleInfoNV## Addr##
                                                                        ByteArray##

instance Eq VkImportMemoryWin32HandleInfoNV where
        (VkImportMemoryWin32HandleInfoNV## a _) ==
          x@(VkImportMemoryWin32HandleInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryWin32HandleInfoNV where
        (VkImportMemoryWin32HandleInfoNV## a _) `compare`
          x@(VkImportMemoryWin32HandleInfoNV## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryWin32HandleInfoNV where
        sizeOf ~_ = #{size VkImportMemoryWin32HandleInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryWin32HandleInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportMemoryWin32HandleInfoNV where
        unsafeAddr (VkImportMemoryWin32HandleInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportMemoryWin32HandleInfoNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportMemoryWin32HandleInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportMemoryWin32HandleInfoNV where
        type StructFields VkImportMemoryWin32HandleInfoNV =
             '["sType", "pNext", "handleType", "handle"] -- ' closing tick for hsc2hs
        type CUnionType VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportMemoryWin32HandleInfoNV =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkImportMemoryWin32HandleInfoNV where
        type VkSTypeMType VkImportMemoryWin32HandleInfoNV = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryWin32HandleInfoNV where
        type FieldType "sType" VkImportMemoryWin32HandleInfoNV =
             VkStructureType
        type FieldOptional "sType" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, sType}
        type FieldIsArray "sType" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, sType}

instance CanReadField "sType" VkImportMemoryWin32HandleInfoNV where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImportMemoryWin32HandleInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkImportMemoryWin32HandleInfoNV where
        type VkPNextMType VkImportMemoryWin32HandleInfoNV = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryWin32HandleInfoNV where
        type FieldType "pNext" VkImportMemoryWin32HandleInfoNV = Ptr Void
        type FieldOptional "pNext" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, pNext}
        type FieldIsArray "pNext" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, pNext}

instance CanReadField "pNext" VkImportMemoryWin32HandleInfoNV where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImportMemoryWin32HandleInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleType VkImportMemoryWin32HandleInfoNV where
        type VkHandleTypeMType VkImportMemoryWin32HandleInfoNV =
             VkExternalMemoryHandleTypeFlagsNV

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoNV, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryWin32HandleInfoNV where
        type FieldType "handleType" VkImportMemoryWin32HandleInfoNV =
             VkExternalMemoryHandleTypeFlagsNV
        type FieldOptional "handleType" VkImportMemoryWin32HandleInfoNV =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, handleType}
        type FieldIsArray "handleType" VkImportMemoryWin32HandleInfoNV =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, handleType}

instance CanReadField "handleType" VkImportMemoryWin32HandleInfoNV
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType" VkImportMemoryWin32HandleInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance {-# OVERLAPPING #-}
         HasVkHandle VkImportMemoryWin32HandleInfoNV where
        type VkHandleMType VkImportMemoryWin32HandleInfoNV = HANDLE

        {-# NOINLINE vkHandle #-}
        vkHandle x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryWin32HandleInfoNV, handle})

        {-# INLINE vkHandleByteOffset #-}
        vkHandleByteOffset ~_
          = #{offset VkImportMemoryWin32HandleInfoNV, handle}

        {-# INLINE readVkHandle #-}
        readVkHandle p
          = peekByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handle}

        {-# INLINE writeVkHandle #-}
        writeVkHandle p
          = pokeByteOff p #{offset VkImportMemoryWin32HandleInfoNV, handle}

instance {-# OVERLAPPING #-}
         HasField "handle" VkImportMemoryWin32HandleInfoNV where
        type FieldType "handle" VkImportMemoryWin32HandleInfoNV = HANDLE
        type FieldOptional "handle" VkImportMemoryWin32HandleInfoNV = 'True -- ' closing tick for hsc2hs
        type FieldOffset "handle" VkImportMemoryWin32HandleInfoNV =
             #{offset VkImportMemoryWin32HandleInfoNV, handle}
        type FieldIsArray "handle" VkImportMemoryWin32HandleInfoNV = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryWin32HandleInfoNV, handle}

instance CanReadField "handle" VkImportMemoryWin32HandleInfoNV
         where
        {-# INLINE getField #-}
        getField = vkHandle

        {-# INLINE readField #-}
        readField = readVkHandle

instance CanWriteField "handle" VkImportMemoryWin32HandleInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandle

instance Show VkImportMemoryWin32HandleInfoNV where
        showsPrec d x
          = showString "VkImportMemoryWin32HandleInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleType = " .
                            showsPrec d (vkHandleType x) .
                              showString ", " .
                                showString "vkHandle = " . showsPrec d (vkHandle x) . showChar '}'
