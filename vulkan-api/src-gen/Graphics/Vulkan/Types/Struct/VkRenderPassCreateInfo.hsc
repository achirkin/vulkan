#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRenderPassCreateInfo
       (VkRenderPassCreateInfo(..)) where
import           Foreign.Storable                                     (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                       (VkRenderPassCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType           (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkAttachmentDescription (VkAttachmentDescription)
import           Graphics.Vulkan.Types.Struct.VkSubpassDependency     (VkSubpassDependency)
import           Graphics.Vulkan.Types.Struct.VkSubpassDescription    (VkSubpassDescription)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                     (unsafeDupablePerformIO)

-- | > typedef struct VkRenderPassCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkRenderPassCreateFlags    flags;
--   >     uint32_t   attachmentCount;
--   >     const VkAttachmentDescription* pAttachments;
--   >     uint32_t               subpassCount;
--   >     const VkSubpassDescription* pSubpasses;
--   >     uint32_t       dependencyCount;
--   >     const VkSubpassDependency* pDependencies;
--   > } VkRenderPassCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkRenderPassCreateInfo.html VkRenderPassCreateInfo registry at www.khronos.org>
data VkRenderPassCreateInfo = VkRenderPassCreateInfo## Addr##
                                                      ByteArray##

instance Eq VkRenderPassCreateInfo where
        (VkRenderPassCreateInfo## a _) == x@(VkRenderPassCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassCreateInfo where
        (VkRenderPassCreateInfo## a _) `compare`
          x@(VkRenderPassCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassCreateInfo where
        sizeOf ~_ = #{size VkRenderPassCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkRenderPassCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassCreateInfo where
        unsafeAddr (VkRenderPassCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassCreateInfo## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassCreateInfo where
        type StructFields VkRenderPassCreateInfo =
             '["sType", "pNext", "flags", "attachmentCount", "pAttachments", -- ' closing tick for hsc2hs
               "subpassCount", "pSubpasses", "dependencyCount", "pDependencies"]
        type CUnionType VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSType VkRenderPassCreateInfo
         where
        type VkSTypeMType VkRenderPassCreateInfo = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkRenderPassCreateInfo, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkRenderPassCreateInfo, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassCreateInfo where
        type FieldType "sType" VkRenderPassCreateInfo = VkStructureType
        type FieldOptional "sType" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, sType}
        type FieldIsArray "sType" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRenderPassCreateInfo, sType}

instance CanReadField "sType" VkRenderPassCreateInfo where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-} HasVkPNext VkRenderPassCreateInfo
         where
        type VkPNextMType VkRenderPassCreateInfo = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkRenderPassCreateInfo, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassCreateInfo where
        type FieldType "pNext" VkRenderPassCreateInfo = Ptr Void
        type FieldOptional "pNext" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, pNext}
        type FieldIsArray "pNext" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRenderPassCreateInfo, pNext}

instance CanReadField "pNext" VkRenderPassCreateInfo where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-} HasVkFlags VkRenderPassCreateInfo
         where
        type VkFlagsMType VkRenderPassCreateInfo = VkRenderPassCreateFlags

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkRenderPassCreateInfo, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkRenderPassCreateInfo, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkRenderPassCreateInfo where
        type FieldType "flags" VkRenderPassCreateInfo =
             VkRenderPassCreateFlags
        type FieldOptional "flags" VkRenderPassCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, flags}
        type FieldIsArray "flags" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkRenderPassCreateInfo, flags}

instance CanReadField "flags" VkRenderPassCreateInfo where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkAttachmentCount VkRenderPassCreateInfo where
        type VkAttachmentCountMType VkRenderPassCreateInfo = Word32

        {-# NOINLINE vkAttachmentCount #-}
        vkAttachmentCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, attachmentCount})

        {-# INLINE vkAttachmentCountByteOffset #-}
        vkAttachmentCountByteOffset ~_
          = #{offset VkRenderPassCreateInfo, attachmentCount}

        {-# INLINE readVkAttachmentCount #-}
        readVkAttachmentCount p
          = peekByteOff p #{offset VkRenderPassCreateInfo, attachmentCount}

        {-# INLINE writeVkAttachmentCount #-}
        writeVkAttachmentCount p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         HasField "attachmentCount" VkRenderPassCreateInfo where
        type FieldType "attachmentCount" VkRenderPassCreateInfo = Word32
        type FieldOptional "attachmentCount" VkRenderPassCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "attachmentCount" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, attachmentCount}
        type FieldIsArray "attachmentCount" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, attachmentCount}

instance CanReadField "attachmentCount" VkRenderPassCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkAttachmentCount

        {-# INLINE readField #-}
        readField = readVkAttachmentCount

instance CanWriteField "attachmentCount" VkRenderPassCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkAttachmentCount

instance {-# OVERLAPPING #-}
         HasVkPAttachments VkRenderPassCreateInfo where
        type VkPAttachmentsMType VkRenderPassCreateInfo =
             Ptr VkAttachmentDescription

        {-# NOINLINE vkPAttachments #-}
        vkPAttachments x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pAttachments})

        {-# INLINE vkPAttachmentsByteOffset #-}
        vkPAttachmentsByteOffset ~_
          = #{offset VkRenderPassCreateInfo, pAttachments}

        {-# INLINE readVkPAttachments #-}
        readVkPAttachments p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pAttachments}

        {-# INLINE writeVkPAttachments #-}
        writeVkPAttachments p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         HasField "pAttachments" VkRenderPassCreateInfo where
        type FieldType "pAttachments" VkRenderPassCreateInfo =
             Ptr VkAttachmentDescription
        type FieldOptional "pAttachments" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAttachments" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, pAttachments}
        type FieldIsArray "pAttachments" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, pAttachments}

instance CanReadField "pAttachments" VkRenderPassCreateInfo where
        {-# INLINE getField #-}
        getField = vkPAttachments

        {-# INLINE readField #-}
        readField = readVkPAttachments

instance CanWriteField "pAttachments" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPAttachments

instance {-# OVERLAPPING #-}
         HasVkSubpassCount VkRenderPassCreateInfo where
        type VkSubpassCountMType VkRenderPassCreateInfo = Word32

        {-# NOINLINE vkSubpassCount #-}
        vkSubpassCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, subpassCount})

        {-# INLINE vkSubpassCountByteOffset #-}
        vkSubpassCountByteOffset ~_
          = #{offset VkRenderPassCreateInfo, subpassCount}

        {-# INLINE readVkSubpassCount #-}
        readVkSubpassCount p
          = peekByteOff p #{offset VkRenderPassCreateInfo, subpassCount}

        {-# INLINE writeVkSubpassCount #-}
        writeVkSubpassCount p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         HasField "subpassCount" VkRenderPassCreateInfo where
        type FieldType "subpassCount" VkRenderPassCreateInfo = Word32
        type FieldOptional "subpassCount" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subpassCount" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, subpassCount}
        type FieldIsArray "subpassCount" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, subpassCount}

instance CanReadField "subpassCount" VkRenderPassCreateInfo where
        {-# INLINE getField #-}
        getField = vkSubpassCount

        {-# INLINE readField #-}
        readField = readVkSubpassCount

instance CanWriteField "subpassCount" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkSubpassCount

instance {-# OVERLAPPING #-} HasVkPSubpasses VkRenderPassCreateInfo
         where
        type VkPSubpassesMType VkRenderPassCreateInfo =
             Ptr VkSubpassDescription

        {-# NOINLINE vkPSubpasses #-}
        vkPSubpasses x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pSubpasses})

        {-# INLINE vkPSubpassesByteOffset #-}
        vkPSubpassesByteOffset ~_
          = #{offset VkRenderPassCreateInfo, pSubpasses}

        {-# INLINE readVkPSubpasses #-}
        readVkPSubpasses p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pSubpasses}

        {-# INLINE writeVkPSubpasses #-}
        writeVkPSubpasses p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pSubpasses}

instance {-# OVERLAPPING #-}
         HasField "pSubpasses" VkRenderPassCreateInfo where
        type FieldType "pSubpasses" VkRenderPassCreateInfo =
             Ptr VkSubpassDescription
        type FieldOptional "pSubpasses" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pSubpasses" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, pSubpasses}
        type FieldIsArray "pSubpasses" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, pSubpasses}

instance CanReadField "pSubpasses" VkRenderPassCreateInfo where
        {-# INLINE getField #-}
        getField = vkPSubpasses

        {-# INLINE readField #-}
        readField = readVkPSubpasses

instance CanWriteField "pSubpasses" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPSubpasses

instance {-# OVERLAPPING #-}
         HasVkDependencyCount VkRenderPassCreateInfo where
        type VkDependencyCountMType VkRenderPassCreateInfo = Word32

        {-# NOINLINE vkDependencyCount #-}
        vkDependencyCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, dependencyCount})

        {-# INLINE vkDependencyCountByteOffset #-}
        vkDependencyCountByteOffset ~_
          = #{offset VkRenderPassCreateInfo, dependencyCount}

        {-# INLINE readVkDependencyCount #-}
        readVkDependencyCount p
          = peekByteOff p #{offset VkRenderPassCreateInfo, dependencyCount}

        {-# INLINE writeVkDependencyCount #-}
        writeVkDependencyCount p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         HasField "dependencyCount" VkRenderPassCreateInfo where
        type FieldType "dependencyCount" VkRenderPassCreateInfo = Word32
        type FieldOptional "dependencyCount" VkRenderPassCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "dependencyCount" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, dependencyCount}
        type FieldIsArray "dependencyCount" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, dependencyCount}

instance CanReadField "dependencyCount" VkRenderPassCreateInfo
         where
        {-# INLINE getField #-}
        getField = vkDependencyCount

        {-# INLINE readField #-}
        readField = readVkDependencyCount

instance CanWriteField "dependencyCount" VkRenderPassCreateInfo
         where
        {-# INLINE writeField #-}
        writeField = writeVkDependencyCount

instance {-# OVERLAPPING #-}
         HasVkPDependencies VkRenderPassCreateInfo where
        type VkPDependenciesMType VkRenderPassCreateInfo =
             Ptr VkSubpassDependency

        {-# NOINLINE vkPDependencies #-}
        vkPDependencies x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pDependencies})

        {-# INLINE vkPDependenciesByteOffset #-}
        vkPDependenciesByteOffset ~_
          = #{offset VkRenderPassCreateInfo, pDependencies}

        {-# INLINE readVkPDependencies #-}
        readVkPDependencies p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pDependencies}

        {-# INLINE writeVkPDependencies #-}
        writeVkPDependencies p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pDependencies}

instance {-# OVERLAPPING #-}
         HasField "pDependencies" VkRenderPassCreateInfo where
        type FieldType "pDependencies" VkRenderPassCreateInfo =
             Ptr VkSubpassDependency
        type FieldOptional "pDependencies" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDependencies" VkRenderPassCreateInfo =
             #{offset VkRenderPassCreateInfo, pDependencies}
        type FieldIsArray "pDependencies" VkRenderPassCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassCreateInfo, pDependencies}

instance CanReadField "pDependencies" VkRenderPassCreateInfo where
        {-# INLINE getField #-}
        getField = vkPDependencies

        {-# INLINE readField #-}
        readField = readVkPDependencies

instance CanWriteField "pDependencies" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField = writeVkPDependencies

instance Show VkRenderPassCreateInfo where
        showsPrec d x
          = showString "VkRenderPassCreateInfo {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkAttachmentCount = " .
                                  showsPrec d (vkAttachmentCount x) .
                                    showString ", " .
                                      showString "vkPAttachments = " .
                                        showsPrec d (vkPAttachments x) .
                                          showString ", " .
                                            showString "vkSubpassCount = " .
                                              showsPrec d (vkSubpassCount x) .
                                                showString ", " .
                                                  showString "vkPSubpasses = " .
                                                    showsPrec d (vkPSubpasses x) .
                                                      showString ", " .
                                                        showString "vkDependencyCount = " .
                                                          showsPrec d (vkDependencyCount x) .
                                                            showString ", " .
                                                              showString "vkPDependencies = " .
                                                                showsPrec d (vkPDependencies x) .
                                                                  showChar '}'
