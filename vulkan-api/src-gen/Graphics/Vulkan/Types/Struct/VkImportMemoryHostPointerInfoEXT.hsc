#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkImportMemoryHostPointerInfoEXT
       (VkImportMemoryHostPointerInfoEXT(..)) where
import           Foreign.Storable                                              (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkExternalMemoryHandleTypeFlagsKHR (VkExternalMemoryHandleTypeFlagBitsKHR)
import           Graphics.Vulkan.Types.Enum.VkStructureType                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkMemoryAllocateInfo             (VkMemoryAllocateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                              (unsafeDupablePerformIO)

-- | > typedef struct VkImportMemoryHostPointerInfoEXT {
--   >     VkStructureType sType;
--   >     const void* pNext;
--   >     VkExternalMemoryHandleTypeFlagBitsKHR handleType;
--   >     void* pHostPointer;
--   > } VkImportMemoryHostPointerInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkImportMemoryHostPointerInfoEXT.html VkImportMemoryHostPointerInfoEXT registry at www.khronos.org>
data VkImportMemoryHostPointerInfoEXT = VkImportMemoryHostPointerInfoEXT## Addr##
                                                                          ByteArray##

instance Eq VkImportMemoryHostPointerInfoEXT where
        (VkImportMemoryHostPointerInfoEXT## a _) ==
          x@(VkImportMemoryHostPointerInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkImportMemoryHostPointerInfoEXT where
        (VkImportMemoryHostPointerInfoEXT## a _) `compare`
          x@(VkImportMemoryHostPointerInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkImportMemoryHostPointerInfoEXT where
        sizeOf ~_ = #{size VkImportMemoryHostPointerInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkImportMemoryHostPointerInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkImportMemoryHostPointerInfoEXT where
        unsafeAddr (VkImportMemoryHostPointerInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkImportMemoryHostPointerInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkImportMemoryHostPointerInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkImportMemoryHostPointerInfoEXT where
        type StructFields VkImportMemoryHostPointerInfoEXT =
             '["sType", "pNext", "handleType", "pHostPointer"] -- ' closing tick for hsc2hs
        type CUnionType VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkImportMemoryHostPointerInfoEXT =
             '[VkMemoryAllocateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkImportMemoryHostPointerInfoEXT where
        type VkSTypeMType VkImportMemoryHostPointerInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkImportMemoryHostPointerInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkImportMemoryHostPointerInfoEXT where
        type FieldType "sType" VkImportMemoryHostPointerInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, sType}
        type FieldIsArray "sType" VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, sType}

instance CanReadField "sType" VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkImportMemoryHostPointerInfoEXT where
        type VkPNextMType VkImportMemoryHostPointerInfoEXT = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkImportMemoryHostPointerInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkImportMemoryHostPointerInfoEXT where
        type FieldType "pNext" VkImportMemoryHostPointerInfoEXT = Ptr Void
        type FieldOptional "pNext" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, pNext}
        type FieldIsArray "pNext" VkImportMemoryHostPointerInfoEXT = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, pNext}

instance CanReadField "pNext" VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkHandleType VkImportMemoryHostPointerInfoEXT where
        type VkHandleTypeMType VkImportMemoryHostPointerInfoEXT =
             VkExternalMemoryHandleTypeFlagBitsKHR

        {-# NOINLINE vkHandleType #-}
        vkHandleType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, handleType})

        {-# INLINE vkHandleTypeByteOffset #-}
        vkHandleTypeByteOffset ~_
          = #{offset VkImportMemoryHostPointerInfoEXT, handleType}

        {-# INLINE readVkHandleType #-}
        readVkHandleType p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, handleType}

        {-# INLINE writeVkHandleType #-}
        writeVkHandleType p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, handleType}

instance {-# OVERLAPPING #-}
         HasField "handleType" VkImportMemoryHostPointerInfoEXT where
        type FieldType "handleType" VkImportMemoryHostPointerInfoEXT =
             VkExternalMemoryHandleTypeFlagBitsKHR
        type FieldOptional "handleType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "handleType" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, handleType}
        type FieldIsArray "handleType" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, handleType}

instance CanReadField "handleType" VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkHandleType

        {-# INLINE readField #-}
        readField = readVkHandleType

instance CanWriteField "handleType"
           VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkHandleType

instance {-# OVERLAPPING #-}
         HasVkPHostPointer VkImportMemoryHostPointerInfoEXT where
        type VkPHostPointerMType VkImportMemoryHostPointerInfoEXT =
             Ptr Void

        {-# NOINLINE vkPHostPointer #-}
        vkPHostPointer x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer})

        {-# INLINE vkPHostPointerByteOffset #-}
        vkPHostPointerByteOffset ~_
          = #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

        {-# INLINE readVkPHostPointer #-}
        readVkPHostPointer p
          = peekByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

        {-# INLINE writeVkPHostPointer #-}
        writeVkPHostPointer p
          = pokeByteOff p #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

instance {-# OVERLAPPING #-}
         HasField "pHostPointer" VkImportMemoryHostPointerInfoEXT where
        type FieldType "pHostPointer" VkImportMemoryHostPointerInfoEXT =
             Ptr Void
        type FieldOptional "pHostPointer" VkImportMemoryHostPointerInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pHostPointer" VkImportMemoryHostPointerInfoEXT =
             #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}
        type FieldIsArray "pHostPointer" VkImportMemoryHostPointerInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkImportMemoryHostPointerInfoEXT, pHostPointer}

instance CanReadField "pHostPointer"
           VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPHostPointer

        {-# INLINE readField #-}
        readField = readVkPHostPointer

instance CanWriteField "pHostPointer"
           VkImportMemoryHostPointerInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPHostPointer

instance Show VkImportMemoryHostPointerInfoEXT where
        showsPrec d x
          = showString "VkImportMemoryHostPointerInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkHandleType = " .
                            showsPrec d (vkHandleType x) .
                              showString ", " .
                                showString "vkPHostPointer = " .
                                  showsPrec d (vkPHostPointer x) . showChar '}'
