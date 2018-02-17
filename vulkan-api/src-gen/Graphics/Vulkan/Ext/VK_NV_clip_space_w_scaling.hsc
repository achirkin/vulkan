#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_NV_clip_space_w_scaling
       (-- * Vulkan extension: @VK_NV_clip_space_w_scaling@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Eric Werness @ewerness@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @88@
        VkViewportWScalingNV(..),
        VkPipelineViewportWScalingStateCreateInfoNV(..),
        vkCmdSetViewportWScalingNV,
        VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION,
        pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION,
        VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME,
        pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV,
        pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkPipelineViewportStateCreateInfo)
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkViewportWScalingNV {
--   >     float          xcoeff;
--   >     float          ycoeff;
--   > } VkViewportWScalingNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkViewportWScalingNV.html VkViewportWScalingNV registry at www.khronos.org>
data VkViewportWScalingNV = VkViewportWScalingNV## Addr## ByteArray##

instance Eq VkViewportWScalingNV where
        (VkViewportWScalingNV## a _) == x@(VkViewportWScalingNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkViewportWScalingNV where
        (VkViewportWScalingNV## a _) `compare` x@(VkViewportWScalingNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkViewportWScalingNV where
        sizeOf ~_ = #{size VkViewportWScalingNV}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViewportWScalingNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkViewportWScalingNV where
        unsafeAddr (VkViewportWScalingNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkViewportWScalingNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkViewportWScalingNV## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkViewportWScalingNV where
        type StructFields VkViewportWScalingNV = '["xcoeff", "ycoeff"] -- ' closing tick for hsc2hs
        type CUnionType VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkViewportWScalingNV = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkXcoeff VkViewportWScalingNV where
        type VkXcoeffMType VkViewportWScalingNV = #{type float}

        {-# NOINLINE vkXcoeff #-}
        vkXcoeff x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportWScalingNV, xcoeff})

        {-# INLINE vkXcoeffByteOffset #-}
        vkXcoeffByteOffset ~_
          = #{offset VkViewportWScalingNV, xcoeff}

        {-# INLINE readVkXcoeff #-}
        readVkXcoeff p
          = peekByteOff p #{offset VkViewportWScalingNV, xcoeff}

        {-# INLINE writeVkXcoeff #-}
        writeVkXcoeff p
          = pokeByteOff p #{offset VkViewportWScalingNV, xcoeff}

instance {-# OVERLAPPING #-} HasField "xcoeff" VkViewportWScalingNV
         where
        type FieldType "xcoeff" VkViewportWScalingNV =
             #{type float}
        type FieldOptional "xcoeff" VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "xcoeff" VkViewportWScalingNV =
             #{offset VkViewportWScalingNV, xcoeff}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportWScalingNV, xcoeff}

instance CanReadField "xcoeff" VkViewportWScalingNV where
        {-# INLINE getField #-}
        getField = vkXcoeff

        {-# INLINE readField #-}
        readField = readVkXcoeff

instance CanWriteField "xcoeff" VkViewportWScalingNV where
        {-# INLINE writeField #-}
        writeField = writeVkXcoeff

instance {-# OVERLAPPING #-} HasVkYcoeff VkViewportWScalingNV where
        type VkYcoeffMType VkViewportWScalingNV = #{type float}

        {-# NOINLINE vkYcoeff #-}
        vkYcoeff x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportWScalingNV, ycoeff})

        {-# INLINE vkYcoeffByteOffset #-}
        vkYcoeffByteOffset ~_
          = #{offset VkViewportWScalingNV, ycoeff}

        {-# INLINE readVkYcoeff #-}
        readVkYcoeff p
          = peekByteOff p #{offset VkViewportWScalingNV, ycoeff}

        {-# INLINE writeVkYcoeff #-}
        writeVkYcoeff p
          = pokeByteOff p #{offset VkViewportWScalingNV, ycoeff}

instance {-# OVERLAPPING #-} HasField "ycoeff" VkViewportWScalingNV
         where
        type FieldType "ycoeff" VkViewportWScalingNV =
             #{type float}
        type FieldOptional "ycoeff" VkViewportWScalingNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ycoeff" VkViewportWScalingNV =
             #{offset VkViewportWScalingNV, ycoeff}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportWScalingNV, ycoeff}

instance CanReadField "ycoeff" VkViewportWScalingNV where
        {-# INLINE getField #-}
        getField = vkYcoeff

        {-# INLINE readField #-}
        readField = readVkYcoeff

instance CanWriteField "ycoeff" VkViewportWScalingNV where
        {-# INLINE writeField #-}
        writeField = writeVkYcoeff

instance Show VkViewportWScalingNV where
        showsPrec d x
          = showString "VkViewportWScalingNV {" .
              showString "vkXcoeff = " .
                showsPrec d (vkXcoeff x) .
                  showString ", " .
                    showString "vkYcoeff = " . showsPrec d (vkYcoeff x) . showChar '}'

-- | > typedef struct VkPipelineViewportWScalingStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBool32               viewportWScalingEnable;
--   >     uint32_t               viewportCount;
--   >     const VkViewportWScalingNV*      pViewportWScalings;
--   > } VkPipelineViewportWScalingStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineViewportWScalingStateCreateInfoNV.html VkPipelineViewportWScalingStateCreateInfoNV registry at www.khronos.org>
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

-- | queues: @graphics@
--
--   renderpass: @both@
--
--   > void vkCmdSetViewportWScalingNV
--   >     ( VkCommandBuffer commandBuffer
--   >     , uint32_t firstViewport
--   >     , uint32_t viewportCount
--   >     , const VkViewportWScalingNV* pViewportWScalings
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/vkCmdSetViewportWScalingNV.html vkCmdSetViewportWScalingNV registry at www.khronos.org>
foreign import ccall unsafe "vkCmdSetViewportWScalingNV"
               vkCmdSetViewportWScalingNV ::
               VkCommandBuffer -- ^ commandBuffer
                               ->
                 Word32 -- ^ firstViewport
                        -> Word32 -- ^ viewportCount
                                  -> Ptr VkViewportWScalingNV -- ^ pViewportWScalings
                                                              -> IO ()

pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1

type VK_NV_CLIP_SPACE_W_SCALING_SPEC_VERSION = 1

pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString

pattern VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME <-
        (is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME -> True)
  where VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
          = _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME

{-# INLINE _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME #-}

_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString
_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  = Ptr "VK_NV_clip_space_w_scaling\NUL"##

{-# INLINE is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME #-}

is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME :: CString -> Bool
is_VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME
  = eqCStrings _VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME

type VK_NV_CLIP_SPACE_W_SCALING_EXTENSION_NAME =
     "VK_NV_clip_space_w_scaling"

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
        = VkStructureType 1000087000

pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: VkDynamicState

pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV =
        VkDynamicState 1000087000
