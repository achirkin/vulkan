#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineViewportWScalingStateCreateInfoNV
       (VkPipelineViewportWScalingStateCreateInfoNV(..)) where
import           Foreign.Storable                                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                                (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo (VkPipelineViewportStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkViewportWScalingNV              (VkViewportWScalingNV)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                               (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineViewportWScalingStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBool32               viewportWScalingEnable;
--   >     uint32_t               viewportCount;
--   >     const VkViewportWScalingNV*      pViewportWScalings;
--   > } VkPipelineViewportWScalingStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineViewportWScalingStateCreateInfoNV.html VkPipelineViewportWScalingStateCreateInfoNV registry at www.khronos.org>
data VkPipelineViewportWScalingStateCreateInfoNV = VkPipelineViewportWScalingStateCreateInfoNV## Addr##
                                                                                                ByteArray##

instance Eq VkPipelineViewportWScalingStateCreateInfoNV where
        (VkPipelineViewportWScalingStateCreateInfoNV## a _) ==
          x@(VkPipelineViewportWScalingStateCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineViewportWScalingStateCreateInfoNV where
        (VkPipelineViewportWScalingStateCreateInfoNV## a _) `compare`
          x@(VkPipelineViewportWScalingStateCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineViewportWScalingStateCreateInfoNV where
        sizeOf ~_
          = #{size VkPipelineViewportWScalingStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineViewportWScalingStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        unsafeAddr (VkPipelineViewportWScalingStateCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineViewportWScalingStateCreateInfoNV## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineViewportWScalingStateCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineViewportWScalingStateCreateInfoNV
         where
        type StructFields VkPipelineViewportWScalingStateCreateInfoNV =
             '["sType", "pNext", "viewportWScalingEnable", "viewportCount", -- ' closing tick for hsc2hs
               "pViewportWScalings"]
        type CUnionType VkPipelineViewportWScalingStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineViewportWScalingStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineViewportWScalingStateCreateInfoNV =
             '[VkPipelineViewportStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineViewportWScalingStateCreateInfoNV where
        type VkSTypeMType VkPipelineViewportWScalingStateCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineViewportWScalingStateCreateInfoNV where
        type FieldType "sType" VkPipelineViewportWScalingStateCreateInfoNV
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}
        type FieldIsArray "sType"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

instance CanReadField "sType"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineViewportWScalingStateCreateInfoNV where
        type VkPNextMType VkPipelineViewportWScalingStateCreateInfoNV =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineViewportWScalingStateCreateInfoNV where
        type FieldType "pNext" VkPipelineViewportWScalingStateCreateInfoNV
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}
        type FieldIsArray "pNext"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

instance CanReadField "pNext"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkViewportWScalingEnable
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type VkViewportWScalingEnableMType
               VkPipelineViewportWScalingStateCreateInfoNV
             = VkBool32

        {-# NOINLINE vkViewportWScalingEnable #-}
        vkViewportWScalingEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable})

        {-# INLINE vkViewportWScalingEnableByteOffset #-}
        vkViewportWScalingEnableByteOffset ~_
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

        {-# INLINE readVkViewportWScalingEnable #-}
        readVkViewportWScalingEnable p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

        {-# INLINE writeVkViewportWScalingEnable #-}
        writeVkViewportWScalingEnable p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

instance {-# OVERLAPPING #-}
         HasField "viewportWScalingEnable"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type FieldType "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             = VkBool32
        type FieldOptional "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}
        type FieldIsArray "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

instance CanReadField "viewportWScalingEnable"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkViewportWScalingEnable

        {-# INLINE readField #-}
        readField = readVkViewportWScalingEnable

instance CanWriteField "viewportWScalingEnable"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkViewportWScalingEnable

instance {-# OVERLAPPING #-}
         HasVkViewportCount VkPipelineViewportWScalingStateCreateInfoNV
         where
        type VkViewportCountMType
               VkPipelineViewportWScalingStateCreateInfoNV
             = Word32

        {-# NOINLINE vkViewportCount #-}
        vkViewportCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount})

        {-# INLINE vkViewportCountByteOffset #-}
        vkViewportCountByteOffset ~_
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

        {-# INLINE readVkViewportCount #-}
        readVkViewportCount p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

        {-# INLINE writeVkViewportCount #-}
        writeVkViewportCount p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         HasField "viewportCount"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type FieldType "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             = Word32
        type FieldOptional "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}
        type FieldIsArray "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

instance CanReadField "viewportCount"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkViewportCount

        {-# INLINE readField #-}
        readField = readVkViewportCount

instance CanWriteField "viewportCount"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkViewportCount

instance {-# OVERLAPPING #-}
         HasVkPViewportWScalings VkPipelineViewportWScalingStateCreateInfoNV
         where
        type VkPViewportWScalingsMType
               VkPipelineViewportWScalingStateCreateInfoNV
             = Ptr VkViewportWScalingNV

        {-# NOINLINE vkPViewportWScalings #-}
        vkPViewportWScalings x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings})

        {-# INLINE vkPViewportWScalingsByteOffset #-}
        vkPViewportWScalingsByteOffset ~_
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

        {-# INLINE readVkPViewportWScalings #-}
        readVkPViewportWScalings p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

        {-# INLINE writeVkPViewportWScalings #-}
        writeVkPViewportWScalings p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

instance {-# OVERLAPPING #-}
         HasField "pViewportWScalings"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type FieldType "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             = Ptr VkViewportWScalingNV
        type FieldOptional "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}
        type FieldIsArray "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

instance CanReadField "pViewportWScalings"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPViewportWScalings

        {-# INLINE readField #-}
        readField = readVkPViewportWScalings

instance CanWriteField "pViewportWScalings"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPViewportWScalings

instance Show VkPipelineViewportWScalingStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineViewportWScalingStateCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkViewportWScalingEnable = " .
                            showsPrec d (vkViewportWScalingEnable x) .
                              showString ", " .
                                showString "vkViewportCount = " .
                                  showsPrec d (vkViewportCount x) .
                                    showString ", " .
                                      showString "vkPViewportWScalings = " .
                                        showsPrec d (vkPViewportWScalings x) . showChar '}'
