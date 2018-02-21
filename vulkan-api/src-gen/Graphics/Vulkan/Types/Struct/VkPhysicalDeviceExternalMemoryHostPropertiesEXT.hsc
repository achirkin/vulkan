#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceExternalMemoryHostPropertiesEXT
       (VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                             (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2KHR (VkPhysicalDeviceProperties2KHR)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceExternalMemoryHostPropertiesEXT {
--   >     VkStructureType sType;
--   >     void* pNext;
--   >     VkDeviceSize minImportedHostPointerAlignment;
--   > } VkPhysicalDeviceExternalMemoryHostPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPhysicalDeviceExternalMemoryHostPropertiesEXT.html VkPhysicalDeviceExternalMemoryHostPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceExternalMemoryHostPropertiesEXT = VkPhysicalDeviceExternalMemoryHostPropertiesEXT## Addr##
                                                                                                        ByteArray##

instance Eq VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## a _) ==
          x@(VkPhysicalDeviceExternalMemoryHostPropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## a _) `compare`
          x@(VkPhysicalDeviceExternalMemoryHostPropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceExternalMemoryHostPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceExternalMemoryHostPropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        unsafeAddr (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceExternalMemoryHostPropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceExternalMemoryHostPropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type StructFields VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
             '["sType", "pNext", "minImportedHostPointerAlignment"] -- ' closing tick for hsc2hs
        type CUnionType VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = '[VkPhysicalDeviceProperties2KHR] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        type VkSTypeMType VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             =
             #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, sType}

instance CanReadField "sType"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        type VkPNextMType VkPhysicalDeviceExternalMemoryHostPropertiesEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             =
             #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, pNext}

instance CanReadField "pNext"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkMinImportedHostPointerAlignment
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type VkMinImportedHostPointerAlignmentMType
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = VkDeviceSize

        {-# NOINLINE vkMinImportedHostPointerAlignment #-}
        vkMinImportedHostPointerAlignment x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment})

        {-# INLINE vkMinImportedHostPointerAlignmentByteOffset #-}
        vkMinImportedHostPointerAlignmentByteOffset ~_
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}

        {-# INLINE readVkMinImportedHostPointerAlignment #-}
        readVkMinImportedHostPointerAlignment p
          = peekByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}

        {-# INLINE writeVkMinImportedHostPointerAlignment #-}
        writeVkMinImportedHostPointerAlignment p
          = pokeByteOff p #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}

instance {-# OVERLAPPING #-}
         HasField "minImportedHostPointerAlignment"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        type FieldType "minImportedHostPointerAlignment"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = VkDeviceSize
        type FieldOptional "minImportedHostPointerAlignment"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minImportedHostPointerAlignment"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             =
             #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}
        type FieldIsArray "minImportedHostPointerAlignment"
               VkPhysicalDeviceExternalMemoryHostPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceExternalMemoryHostPropertiesEXT, minImportedHostPointerAlignment}

instance CanReadField "minImportedHostPointerAlignment"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE getField #-}
        getField = vkMinImportedHostPointerAlignment

        {-# INLINE readField #-}
        readField = readVkMinImportedHostPointerAlignment

instance CanWriteField "minImportedHostPointerAlignment"
           VkPhysicalDeviceExternalMemoryHostPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkMinImportedHostPointerAlignment

instance Show VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
        showsPrec d x
          = showString "VkPhysicalDeviceExternalMemoryHostPropertiesEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkMinImportedHostPointerAlignment = " .
                            showsPrec d (vkMinImportedHostPointerAlignment x) . showChar '}'
