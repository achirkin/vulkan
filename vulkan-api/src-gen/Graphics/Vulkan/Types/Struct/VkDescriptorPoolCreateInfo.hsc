#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorPoolCreateInfo
       (VkDescriptorPoolCreateInfo(..)) where
import           Foreign.Storable                                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkDescriptorPoolCreateFlags (VkDescriptorPoolCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType             (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkDescriptorPoolSize      (VkDescriptorPoolSize)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                       (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorPoolCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkDescriptorPoolCreateFlags  flags;
--   >     uint32_t               maxSets;
--   >     uint32_t               poolSizeCount;
--   >     const VkDescriptorPoolSize* pPoolSizes;
--   > } VkDescriptorPoolCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorPoolCreateInfo.html VkDescriptorPoolCreateInfo registry at www.khronos.org>
data VkDescriptorPoolCreateInfo = VkDescriptorPoolCreateInfo## Addr##
                                                              ByteArray##

instance Eq VkDescriptorPoolCreateInfo where
        (VkDescriptorPoolCreateInfo## a _) ==
          x@(VkDescriptorPoolCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorPoolCreateInfo where
        (VkDescriptorPoolCreateInfo## a _) `compare`
          x@(VkDescriptorPoolCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorPoolCreateInfo where
        sizeOf ~_ = #{size VkDescriptorPoolCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDescriptorPoolCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorPoolCreateInfo where
        unsafeAddr (VkDescriptorPoolCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorPoolCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorPoolCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorPoolCreateInfo where
        type StructFields VkDescriptorPoolCreateInfo =
             '["sType", "pNext", "flags", "maxSets", "poolSizeCount", -- ' closing tick for hsc2hs
               "pPoolSizes"]
        type CUnionType VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorPoolCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkDescriptorPoolCreateInfo
         where
        type VkSTypeMType VkDescriptorPoolCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkDescriptorPoolCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorPoolCreateInfo where
        type FieldType "sType" VkDescriptorPoolCreateInfo = VkStructureType
        type FieldOptional "sType" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, sType}
        type FieldIsArray "sType" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, sType}

instance CanReadField "sType" VkDescriptorPoolCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkDescriptorPoolCreateInfo
         where
        type VkPNextMType VkDescriptorPoolCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkDescriptorPoolCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorPoolCreateInfo where
        type FieldType "pNext" VkDescriptorPoolCreateInfo = Ptr Void
        type FieldOptional "pNext" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, pNext}
        type FieldIsArray "pNext" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, pNext}

instance CanReadField "pNext" VkDescriptorPoolCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkDescriptorPoolCreateInfo
         where
        type VkFlagsMType VkDescriptorPoolCreateInfo =
             VkDescriptorPoolCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkDescriptorPoolCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorPoolCreateInfo where
        type FieldType "flags" VkDescriptorPoolCreateInfo =
             VkDescriptorPoolCreateFlags
        type FieldOptional "flags" VkDescriptorPoolCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, flags}
        type FieldIsArray "flags" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, flags}

instance CanReadField "flags" VkDescriptorPoolCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkMaxSets VkDescriptorPoolCreateInfo where
        type VkMaxSetsMType VkDescriptorPoolCreateInfo = Word32

        {-# NOINLINE vkMaxSets #-}
        vkMaxSets x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, maxSets})

        {-# INLINE vkMaxSetsByteOffset #-}
        vkMaxSetsByteOffset ~_
          = #{offset VkDescriptorPoolCreateInfo, maxSets}

        {-# INLINE readVkMaxSets #-}
        readVkMaxSets p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, maxSets}

        {-# INLINE writeVkMaxSets #-}
        writeVkMaxSets p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, maxSets}

instance {-# OVERLAPPING #-}
         HasField "maxSets" VkDescriptorPoolCreateInfo where
        type FieldType "maxSets" VkDescriptorPoolCreateInfo = Word32
        type FieldOptional "maxSets" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSets" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, maxSets}
        type FieldIsArray "maxSets" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, maxSets}

instance CanReadField "maxSets" VkDescriptorPoolCreateInfo where
        {-# INLINE getField #-}
        getField = vkMaxSets

        {-# INLINE readField #-}
        readField = readVkMaxSets

instance CanWriteField "maxSets" VkDescriptorPoolCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkMaxSets

instance {-# OVERLAPPING #-}
         HasVkPoolSizeCount VkDescriptorPoolCreateInfo where
        type VkPoolSizeCountMType VkDescriptorPoolCreateInfo = Word32

        {-# NOINLINE vkPoolSizeCount #-}
        vkPoolSizeCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, poolSizeCount})

        {-# INLINE vkPoolSizeCountByteOffset #-}
        vkPoolSizeCountByteOffset ~_
          = #{offset VkDescriptorPoolCreateInfo, poolSizeCount}

        {-# INLINE readVkPoolSizeCount #-}
        readVkPoolSizeCount p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, poolSizeCount}

        {-# INLINE writeVkPoolSizeCount #-}
        writeVkPoolSizeCount p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, poolSizeCount}

instance {-# OVERLAPPING #-}
         HasField "poolSizeCount" VkDescriptorPoolCreateInfo where
        type FieldType "poolSizeCount" VkDescriptorPoolCreateInfo = Word32
        type FieldOptional "poolSizeCount" VkDescriptorPoolCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "poolSizeCount" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, poolSizeCount}
        type FieldIsArray "poolSizeCount" VkDescriptorPoolCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, poolSizeCount}

instance CanReadField "poolSizeCount" VkDescriptorPoolCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkPoolSizeCount

        {-# INLINE readField #-}
        readField = readVkPoolSizeCount

instance CanWriteField "poolSizeCount" VkDescriptorPoolCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPoolSizeCount

instance {-# OVERLAPPING #-}
         HasVkPPoolSizes VkDescriptorPoolCreateInfo where
        type VkPPoolSizesMType VkDescriptorPoolCreateInfo =
             Ptr VkDescriptorPoolSize

        {-# NOINLINE vkPPoolSizes #-}
        vkPPoolSizes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorPoolCreateInfo, pPoolSizes})

        {-# INLINE vkPPoolSizesByteOffset #-}
        vkPPoolSizesByteOffset ~_
          = #{offset VkDescriptorPoolCreateInfo, pPoolSizes}

        {-# INLINE readVkPPoolSizes #-}
        readVkPPoolSizes p
          = peekByteOff p #{offset VkDescriptorPoolCreateInfo, pPoolSizes}

        {-# INLINE writeVkPPoolSizes #-}
        writeVkPPoolSizes p
          = pokeByteOff p #{offset VkDescriptorPoolCreateInfo, pPoolSizes}

instance {-# OVERLAPPING #-}
         HasField "pPoolSizes" VkDescriptorPoolCreateInfo where
        type FieldType "pPoolSizes" VkDescriptorPoolCreateInfo =
             Ptr VkDescriptorPoolSize
        type FieldOptional "pPoolSizes" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPoolSizes" VkDescriptorPoolCreateInfo =
             #{offset VkDescriptorPoolCreateInfo, pPoolSizes}
        type FieldIsArray "pPoolSizes" VkDescriptorPoolCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorPoolCreateInfo, pPoolSizes}

instance CanReadField "pPoolSizes" VkDescriptorPoolCreateInfo where
        {-# INLINE getField #-}
        getField = vkPPoolSizes

        {-# INLINE readField #-}
        readField = readVkPPoolSizes

instance CanWriteField "pPoolSizes" VkDescriptorPoolCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPPoolSizes

instance Show VkDescriptorPoolCreateInfo where
        showsPrec d x
          = showString "VkDescriptorPoolCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkMaxSets = " .
                                  showsPrec d (vkMaxSets x) .
                                    showString ", " .
                                      showString "vkPoolSizeCount = " .
                                        showsPrec d (vkPoolSizeCount x) .
                                          showString ", " .
                                            showString "vkPPoolSizes = " .
                                              showsPrec d (vkPPoolSizes x) . showChar '}'
