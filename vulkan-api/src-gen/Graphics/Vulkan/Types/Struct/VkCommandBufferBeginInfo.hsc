#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkCommandBufferBeginInfo
       (VkCommandBufferBeginInfo(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkCommandBufferUsageFlags        (VkCommandBufferUsageFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkCommandBufferInheritanceInfo (VkCommandBufferInheritanceInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkCommandBufferBeginInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkCommandBufferUsageFlags  flags;
--   >     const VkCommandBufferInheritanceInfo*       pInheritanceInfo;
--   > } VkCommandBufferBeginInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkCommandBufferBeginInfo.html VkCommandBufferBeginInfo registry at www.khronos.org>
data VkCommandBufferBeginInfo = VkCommandBufferBeginInfo## Addr##
                                                          ByteArray##

instance Eq VkCommandBufferBeginInfo where
        (VkCommandBufferBeginInfo## a _) ==
          x@(VkCommandBufferBeginInfo## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkCommandBufferBeginInfo where
        (VkCommandBufferBeginInfo## a _) `compare`
          x@(VkCommandBufferBeginInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkCommandBufferBeginInfo where
        sizeOf ~_ = #{size VkCommandBufferBeginInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkCommandBufferBeginInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkCommandBufferBeginInfo where
        unsafeAddr (VkCommandBufferBeginInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkCommandBufferBeginInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkCommandBufferBeginInfo## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkCommandBufferBeginInfo where
        type StructFields VkCommandBufferBeginInfo =
             '["sType", "pNext", "flags", "pInheritanceInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkCommandBufferBeginInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkCommandBufferBeginInfo
         where
        type VkSTypeMType VkCommandBufferBeginInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkCommandBufferBeginInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkCommandBufferBeginInfo where
        type FieldType "sType" VkCommandBufferBeginInfo = VkStructureType
        type FieldOptional "sType" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, sType}
        type FieldIsArray "sType" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandBufferBeginInfo, sType}

instance CanReadField "sType" VkCommandBufferBeginInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkCommandBufferBeginInfo
         where
        type VkPNextMType VkCommandBufferBeginInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkCommandBufferBeginInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkCommandBufferBeginInfo where
        type FieldType "pNext" VkCommandBufferBeginInfo = Ptr Void
        type FieldOptional "pNext" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, pNext}
        type FieldIsArray "pNext" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandBufferBeginInfo, pNext}

instance CanReadField "pNext" VkCommandBufferBeginInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkCommandBufferBeginInfo
         where
        type VkFlagsMType VkCommandBufferBeginInfo =
             VkCommandBufferUsageFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkCommandBufferBeginInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkCommandBufferBeginInfo where
        type FieldType "flags" VkCommandBufferBeginInfo =
             VkCommandBufferUsageFlags
        type FieldOptional "flags" VkCommandBufferBeginInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, flags}
        type FieldIsArray "flags" VkCommandBufferBeginInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkCommandBufferBeginInfo, flags}

instance CanReadField "flags" VkCommandBufferBeginInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkCommandBufferBeginInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkPInheritanceInfo VkCommandBufferBeginInfo where
        type VkPInheritanceInfoMType VkCommandBufferBeginInfo =
             Ptr VkCommandBufferInheritanceInfo

        {-# NOINLINE vkPInheritanceInfo #-}
        vkPInheritanceInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkCommandBufferBeginInfo, pInheritanceInfo})

        {-# INLINE vkPInheritanceInfoByteOffset #-}
        vkPInheritanceInfoByteOffset ~_
          = #{offset VkCommandBufferBeginInfo, pInheritanceInfo}

        {-# INLINE readVkPInheritanceInfo #-}
        readVkPInheritanceInfo p
          = peekByteOff p #{offset VkCommandBufferBeginInfo, pInheritanceInfo}

        {-# INLINE writeVkPInheritanceInfo #-}
        writeVkPInheritanceInfo p
          = pokeByteOff p #{offset VkCommandBufferBeginInfo, pInheritanceInfo}

instance {-# OVERLAPPING #-}
         HasField "pInheritanceInfo" VkCommandBufferBeginInfo where
        type FieldType "pInheritanceInfo" VkCommandBufferBeginInfo =
             Ptr VkCommandBufferInheritanceInfo
        type FieldOptional "pInheritanceInfo" VkCommandBufferBeginInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "pInheritanceInfo" VkCommandBufferBeginInfo =
             #{offset VkCommandBufferBeginInfo, pInheritanceInfo}
        type FieldIsArray "pInheritanceInfo" VkCommandBufferBeginInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkCommandBufferBeginInfo, pInheritanceInfo}

instance CanReadField "pInheritanceInfo" VkCommandBufferBeginInfo
         where
        {-# INLINE getField #-}
        getField = vkPInheritanceInfo

        {-# INLINE readField #-}
        readField = readVkPInheritanceInfo

instance CanWriteField "pInheritanceInfo" VkCommandBufferBeginInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkPInheritanceInfo

instance Show VkCommandBufferBeginInfo where
        showsPrec d x
          = showString "VkCommandBufferBeginInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkPInheritanceInfo = " .
                                  showsPrec d (vkPInheritanceInfo x) . showChar '}'
