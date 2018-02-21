#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkRenderPassSampleLocationsBeginInfoEXT
       (VkRenderPassSampleLocationsBeginInfoEXT(..)) where
import           Foreign.Storable                                            (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType                  (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkAttachmentSampleLocationsEXT (VkAttachmentSampleLocationsEXT)
import           Graphics.Vulkan.Types.Struct.VkRenderPassBeginInfo          (VkRenderPassBeginInfo)
import           Graphics.Vulkan.Types.Struct.VkSubpassSampleLocationsEXT    (VkSubpassSampleLocationsEXT)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                            (unsafeDupablePerformIO)

-- | > typedef struct VkRenderPassSampleLocationsBeginInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     uint32_t         attachmentInitialSampleLocationsCount;
--   >     const VkAttachmentSampleLocationsEXT* pAttachmentInitialSampleLocations;
--   >     uint32_t         postSubpassSampleLocationsCount;
--   >     const VkSubpassSampleLocationsEXT* pPostSubpassSampleLocations;
--   > } VkRenderPassSampleLocationsBeginInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkRenderPassSampleLocationsBeginInfoEXT.html VkRenderPassSampleLocationsBeginInfoEXT registry at www.khronos.org>
data VkRenderPassSampleLocationsBeginInfoEXT = VkRenderPassSampleLocationsBeginInfoEXT## Addr##
                                                                                        ByteArray##

instance Eq VkRenderPassSampleLocationsBeginInfoEXT where
        (VkRenderPassSampleLocationsBeginInfoEXT## a _) ==
          x@(VkRenderPassSampleLocationsBeginInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkRenderPassSampleLocationsBeginInfoEXT where
        (VkRenderPassSampleLocationsBeginInfoEXT## a _) `compare`
          x@(VkRenderPassSampleLocationsBeginInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkRenderPassSampleLocationsBeginInfoEXT where
        sizeOf ~_
          = #{size VkRenderPassSampleLocationsBeginInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkRenderPassSampleLocationsBeginInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkRenderPassSampleLocationsBeginInfoEXT
         where
        unsafeAddr (VkRenderPassSampleLocationsBeginInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkRenderPassSampleLocationsBeginInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkRenderPassSampleLocationsBeginInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkRenderPassSampleLocationsBeginInfoEXT
         where
        type StructFields VkRenderPassSampleLocationsBeginInfoEXT =
             '["sType", "pNext", "attachmentInitialSampleLocationsCount", -- ' closing tick for hsc2hs
               "pAttachmentInitialSampleLocations",
               "postSubpassSampleLocationsCount", "pPostSubpassSampleLocations"]
        type CUnionType VkRenderPassSampleLocationsBeginInfoEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkRenderPassSampleLocationsBeginInfoEXT = 'False -- ' closing tick for hsc2hs
        type StructExtends VkRenderPassSampleLocationsBeginInfoEXT =
             '[VkRenderPassBeginInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkRenderPassSampleLocationsBeginInfoEXT where
        type VkSTypeMType VkRenderPassSampleLocationsBeginInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkRenderPassSampleLocationsBeginInfoEXT where
        type FieldType "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             VkStructureType
        type FieldOptional "sType" VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}
        type FieldIsArray "sType" VkRenderPassSampleLocationsBeginInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, sType}

instance CanReadField "sType"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkRenderPassSampleLocationsBeginInfoEXT where
        type VkPNextMType VkRenderPassSampleLocationsBeginInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkRenderPassSampleLocationsBeginInfoEXT where
        type FieldType "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             Ptr Void
        type FieldOptional "pNext" VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}
        type FieldIsArray "pNext" VkRenderPassSampleLocationsBeginInfoEXT =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pNext}

instance CanReadField "pNext"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkAttachmentInitialSampleLocationsCount
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkAttachmentInitialSampleLocationsCountMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32

        {-# NOINLINE vkAttachmentInitialSampleLocationsCount #-}
        vkAttachmentInitialSampleLocationsCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount})

        {-# INLINE vkAttachmentInitialSampleLocationsCountByteOffset #-}
        vkAttachmentInitialSampleLocationsCountByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

        {-# INLINE readVkAttachmentInitialSampleLocationsCount #-}
        readVkAttachmentInitialSampleLocationsCount p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

        {-# INLINE writeVkAttachmentInitialSampleLocationsCount #-}
        writeVkAttachmentInitialSampleLocationsCount p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32
        type FieldOptional "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}
        type FieldIsArray "attachmentInitialSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, attachmentInitialSampleLocationsCount}

instance CanReadField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkAttachmentInitialSampleLocationsCount

        {-# INLINE readField #-}
        readField = readVkAttachmentInitialSampleLocationsCount

instance CanWriteField "attachmentInitialSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkAttachmentInitialSampleLocationsCount

instance {-# OVERLAPPING #-}
         HasVkPAttachmentInitialSampleLocations
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkPAttachmentInitialSampleLocationsMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkAttachmentSampleLocationsEXT

        {-# NOINLINE vkPAttachmentInitialSampleLocations #-}
        vkPAttachmentInitialSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations})

        {-# INLINE vkPAttachmentInitialSampleLocationsByteOffset #-}
        vkPAttachmentInitialSampleLocationsByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

        {-# INLINE readVkPAttachmentInitialSampleLocations #-}
        readVkPAttachmentInitialSampleLocations p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

        {-# INLINE writeVkPAttachmentInitialSampleLocations #-}
        writeVkPAttachmentInitialSampleLocations p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance {-# OVERLAPPING #-}
         HasField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkAttachmentSampleLocationsEXT
        type FieldOptional "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}
        type FieldIsArray "pAttachmentInitialSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pAttachmentInitialSampleLocations}

instance CanReadField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPAttachmentInitialSampleLocations

        {-# INLINE readField #-}
        readField = readVkPAttachmentInitialSampleLocations

instance CanWriteField "pAttachmentInitialSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPAttachmentInitialSampleLocations

instance {-# OVERLAPPING #-}
         HasVkPostSubpassSampleLocationsCount
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkPostSubpassSampleLocationsCountMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32

        {-# NOINLINE vkPostSubpassSampleLocationsCount #-}
        vkPostSubpassSampleLocationsCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount})

        {-# INLINE vkPostSubpassSampleLocationsCountByteOffset #-}
        vkPostSubpassSampleLocationsCountByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

        {-# INLINE readVkPostSubpassSampleLocationsCount #-}
        readVkPostSubpassSampleLocationsCount p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

        {-# INLINE writeVkPostSubpassSampleLocationsCount #-}
        writeVkPostSubpassSampleLocationsCount p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance {-# OVERLAPPING #-}
         HasField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Word32
        type FieldOptional "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}
        type FieldIsArray "postSubpassSampleLocationsCount"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, postSubpassSampleLocationsCount}

instance CanReadField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPostSubpassSampleLocationsCount

        {-# INLINE readField #-}
        readField = readVkPostSubpassSampleLocationsCount

instance CanWriteField "postSubpassSampleLocationsCount"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPostSubpassSampleLocationsCount

instance {-# OVERLAPPING #-}
         HasVkPPostSubpassSampleLocations
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type VkPPostSubpassSampleLocationsMType
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkSubpassSampleLocationsEXT

        {-# NOINLINE vkPPostSubpassSampleLocations #-}
        vkPPostSubpassSampleLocations x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations})

        {-# INLINE vkPPostSubpassSampleLocationsByteOffset #-}
        vkPPostSubpassSampleLocationsByteOffset ~_
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

        {-# INLINE readVkPPostSubpassSampleLocations #-}
        readVkPPostSubpassSampleLocations p
          = peekByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

        {-# INLINE writeVkPPostSubpassSampleLocations #-}
        writeVkPPostSubpassSampleLocations p
          = pokeByteOff p #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance {-# OVERLAPPING #-}
         HasField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        type FieldType "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = Ptr VkSubpassSampleLocationsEXT
        type FieldOptional "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             =
             #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}
        type FieldIsArray "pPostSubpassSampleLocations"
               VkRenderPassSampleLocationsBeginInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkRenderPassSampleLocationsBeginInfoEXT, pPostSubpassSampleLocations}

instance CanReadField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPPostSubpassSampleLocations

        {-# INLINE readField #-}
        readField = readVkPPostSubpassSampleLocations

instance CanWriteField "pPostSubpassSampleLocations"
           VkRenderPassSampleLocationsBeginInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPPostSubpassSampleLocations

instance Show VkRenderPassSampleLocationsBeginInfoEXT where
        showsPrec d x
          = showString "VkRenderPassSampleLocationsBeginInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkAttachmentInitialSampleLocationsCount = " .
                            showsPrec d (vkAttachmentInitialSampleLocationsCount x) .
                              showString ", " .
                                showString "vkPAttachmentInitialSampleLocations = " .
                                  showsPrec d (vkPAttachmentInitialSampleLocations x) .
                                    showString ", " .
                                      showString "vkPostSubpassSampleLocationsCount = " .
                                        showsPrec d (vkPostSubpassSampleLocationsCount x) .
                                          showString ", " .
                                            showString "vkPPostSubpassSampleLocations = " .
                                              showsPrec d (vkPPostSubpassSampleLocations x) .
                                                showChar '}'
