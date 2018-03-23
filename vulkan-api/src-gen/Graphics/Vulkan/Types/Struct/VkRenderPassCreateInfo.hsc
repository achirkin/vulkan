#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkRenderPassCreateInfo.html VkRenderPassCreateInfo registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "attachmentCount" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, attachmentCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, attachmentCount}

instance {-# OVERLAPPING #-}
         CanWriteField "attachmentCount" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, attachmentCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pAttachments" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "pAttachments" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pAttachments}

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

instance {-# OVERLAPPING #-}
         CanReadField "subpassCount" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, subpassCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, subpassCount}

instance {-# OVERLAPPING #-}
         CanWriteField "subpassCount" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, subpassCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pSubpasses" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pSubpasses})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pSubpasses}

instance {-# OVERLAPPING #-}
         CanWriteField "pSubpasses" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pSubpasses}

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

instance {-# OVERLAPPING #-}
         CanReadField "dependencyCount" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, dependencyCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, dependencyCount}

instance {-# OVERLAPPING #-}
         CanWriteField "dependencyCount" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, dependencyCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pDependencies" VkRenderPassCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassCreateInfo, pDependencies})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkRenderPassCreateInfo, pDependencies}

instance {-# OVERLAPPING #-}
         CanWriteField "pDependencies" VkRenderPassCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkRenderPassCreateInfo, pDependencies}

instance Show VkRenderPassCreateInfo where
        showsPrec d x
          = showString "VkRenderPassCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "attachmentCount = " .
                                  showsPrec d (getField @"attachmentCount" x) .
                                    showString ", " .
                                      showString "pAttachments = " .
                                        showsPrec d (getField @"pAttachments" x) .
                                          showString ", " .
                                            showString "subpassCount = " .
                                              showsPrec d (getField @"subpassCount" x) .
                                                showString ", " .
                                                  showString "pSubpasses = " .
                                                    showsPrec d (getField @"pSubpasses" x) .
                                                      showString ", " .
                                                        showString "dependencyCount = " .
                                                          showsPrec d
                                                            (getField @"dependencyCount" x)
                                                            .
                                                            showString ", " .
                                                              showString "pDependencies = " .
                                                                showsPrec d
                                                                  (getField @"pDependencies" x)
                                                                  . showChar '}'
