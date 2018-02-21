#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutCreateInfo
       (VkDescriptorSetLayoutCreateInfo(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorSetLayoutCreateFlags (VkDescriptorSetLayoutCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDescriptorSetLayoutBinding   (VkDescriptorSetLayoutBinding)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorSetLayoutCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorSetLayoutCreateFlags    flags;
--   >     uint32_t               bindingCount;
--   >     const VkDescriptorSetLayoutBinding* pBindings;
--   > } VkDescriptorSetLayoutCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorSetLayoutCreateInfo.html VkDescriptorSetLayoutCreateInfo registry at www.khronos.org>
data VkDescriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo## Addr##
                                                                        ByteArray##

instance Eq VkDescriptorSetLayoutCreateInfo where
        (VkDescriptorSetLayoutCreateInfo## a _) ==
          x@(VkDescriptorSetLayoutCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorSetLayoutCreateInfo where
        (VkDescriptorSetLayoutCreateInfo## a _) `compare`
          x@(VkDescriptorSetLayoutCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorSetLayoutCreateInfo where
        sizeOf ~_ = #{size VkDescriptorSetLayoutCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorSetLayoutCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorSetLayoutCreateInfo where
        unsafeAddr (VkDescriptorSetLayoutCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorSetLayoutCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorSetLayoutCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorSetLayoutCreateInfo where
        type StructFields VkDescriptorSetLayoutCreateInfo =
             '["sType", "pNext", "flags", "bindingCount", "pBindings"] -- ' closing tick for hsc2hs
        type CUnionType VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorSetLayoutCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkDescriptorSetLayoutCreateInfo where
        type VkSTypeMType VkDescriptorSetLayoutCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDescriptorSetLayoutCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorSetLayoutCreateInfo where
        type FieldType "sType" VkDescriptorSetLayoutCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, sType}
        type FieldIsArray "sType" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, sType}

instance CanReadField "sType" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDescriptorSetLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkDescriptorSetLayoutCreateInfo where
        type VkPNextMType VkDescriptorSetLayoutCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDescriptorSetLayoutCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorSetLayoutCreateInfo where
        type FieldType "pNext" VkDescriptorSetLayoutCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, pNext}
        type FieldIsArray "pNext" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, pNext}

instance CanReadField "pNext" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDescriptorSetLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkDescriptorSetLayoutCreateInfo where
        type VkFlagsMType VkDescriptorSetLayoutCreateInfo =
             VkDescriptorSetLayoutCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDescriptorSetLayoutCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorSetLayoutCreateInfo where
        type FieldType "flags" VkDescriptorSetLayoutCreateInfo =
             VkDescriptorSetLayoutCreateFlags
        type FieldOptional "flags" VkDescriptorSetLayoutCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, flags}
        type FieldIsArray "flags" VkDescriptorSetLayoutCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, flags}

instance CanReadField "flags" VkDescriptorSetLayoutCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkDescriptorSetLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkBindingCount VkDescriptorSetLayoutCreateInfo where
        type VkBindingCountMType VkDescriptorSetLayoutCreateInfo = Word32

        {-# NOINLINE vkBindingCount #-}
        vkBindingCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, bindingCount})

        {-# INLINE vkBindingCountByteOffset #-}
        vkBindingCountByteOffset ~_
          = #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}

        {-# INLINE readVkBindingCount #-}
        readVkBindingCount p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}

        {-# INLINE writeVkBindingCount #-}
        writeVkBindingCount p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}

instance {-# OVERLAPPING #-}
         HasField "bindingCount" VkDescriptorSetLayoutCreateInfo where
        type FieldType "bindingCount" VkDescriptorSetLayoutCreateInfo =
             Word32
        type FieldOptional "bindingCount" VkDescriptorSetLayoutCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "bindingCount" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}
        type FieldIsArray "bindingCount" VkDescriptorSetLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, bindingCount}

instance CanReadField "bindingCount"
           VkDescriptorSetLayoutCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkBindingCount

        {-# INLINE readField #-}
        readField = readVkBindingCount

instance CanWriteField "bindingCount"
           VkDescriptorSetLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkBindingCount

instance {-# OVERLAPPING #-}
         HasVkPBindings VkDescriptorSetLayoutCreateInfo where
        type VkPBindingsMType VkDescriptorSetLayoutCreateInfo =
             Ptr VkDescriptorSetLayoutBinding

        {-# NOINLINE vkPBindings #-}
        vkPBindings x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorSetLayoutCreateInfo, pBindings})

        {-# INLINE vkPBindingsByteOffset #-}
        vkPBindingsByteOffset ~_
          = #{offset VkDescriptorSetLayoutCreateInfo, pBindings}

        {-# INLINE readVkPBindings #-}
        readVkPBindings p
          = peekByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pBindings}

        {-# INLINE writeVkPBindings #-}
        writeVkPBindings p
          = pokeByteOff p #{offset VkDescriptorSetLayoutCreateInfo, pBindings}

instance {-# OVERLAPPING #-}
         HasField "pBindings" VkDescriptorSetLayoutCreateInfo where
        type FieldType "pBindings" VkDescriptorSetLayoutCreateInfo =
             Ptr VkDescriptorSetLayoutBinding
        type FieldOptional "pBindings" VkDescriptorSetLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pBindings" VkDescriptorSetLayoutCreateInfo =
             #{offset VkDescriptorSetLayoutCreateInfo, pBindings}
        type FieldIsArray "pBindings" VkDescriptorSetLayoutCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorSetLayoutCreateInfo, pBindings}

instance CanReadField "pBindings" VkDescriptorSetLayoutCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPBindings

        {-# INLINE readField #-}
        readField = readVkPBindings

instance CanWriteField "pBindings" VkDescriptorSetLayoutCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPBindings

instance Show VkDescriptorSetLayoutCreateInfo where
        showsPrec d x
          = showString "VkDescriptorSetLayoutCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkBindingCount = " .
                                  showsPrec d (vkBindingCount x) .
                                    showString ", " .
                                      showString "vkPBindings = " .
                                        showsPrec d (vkPBindings x) . showChar '}'
