#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_AMD_rasterization_order
       (-- * Vulkan extension: @VK_AMD_rasterization_order@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Daniel Rakos @aqnuep@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @19@
        VkPipelineRasterizationStateRasterizationOrderAMD(..),
        VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION,
        pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION,
        VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME,
        pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.ForeignPtr                   (ForeignPtr (..),
                                                   ForeignPtrContents (..),
                                                   newForeignPtr_)
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.Types                        (IO (..), Int (..))
import           Graphics.Vulkan.Common           (VkRasterizationOrderAMD,
                                                   VkStructureType (..))
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineRasterizationStateRasterizationOrderAMD {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkRasterizationOrderAMD          rasterizationOrder;
--   > } VkPipelineRasterizationStateRasterizationOrderAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineRasterizationStateRasterizationOrderAMD.html VkPipelineRasterizationStateRasterizationOrderAMD registry at www.khronos.org>
data VkPipelineRasterizationStateRasterizationOrderAMD = VkPipelineRasterizationStateRasterizationOrderAMD## ByteArray##

instance Eq VkPipelineRasterizationStateRasterizationOrderAMD where
        (VkPipelineRasterizationStateRasterizationOrderAMD## a) ==
          (VkPipelineRasterizationStateRasterizationOrderAMD## b)
          = EQ == cmpImmutableContent a b

        {-# INLINE (==) #-}

instance Ord VkPipelineRasterizationStateRasterizationOrderAMD
         where
        (VkPipelineRasterizationStateRasterizationOrderAMD## a) `compare`
          (VkPipelineRasterizationStateRasterizationOrderAMD## b)
          = cmpImmutableContent a b

        {-# INLINE compare #-}

instance Storable VkPipelineRasterizationStateRasterizationOrderAMD
         where
        sizeOf ~_
          = #{size VkPipelineRasterizationStateRasterizationOrderAMD}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineRasterizationStateRasterizationOrderAMD}

        {-# INLINE alignment #-}
        peek (Ptr addr)
          | I## n <- sizeOf
                      (undefined :: VkPipelineRasterizationStateRasterizationOrderAMD),
            I## a <- alignment
                      (undefined :: VkPipelineRasterizationStateRasterizationOrderAMD)
            =
            IO
              (\ s ->
                 case newAlignedPinnedByteArray## n a s of
                     (## s1, mba ##) -> case copyAddrToByteArray## addr mba 0## n s1 of
                                          s2 -> case unsafeFreezeByteArray## mba s2 of
                                                    (## s3, ba ##) -> (## s3,
                                                                       VkPipelineRasterizationStateRasterizationOrderAMD##
                                                                         ba ##))

        {-# INLINE peek #-}
        poke (Ptr addr)
          (VkPipelineRasterizationStateRasterizationOrderAMD## ba)
          | I## n <- sizeOf
                      (undefined :: VkPipelineRasterizationStateRasterizationOrderAMD)
            = IO (\ s -> (## copyByteArrayToAddr## ba 0## addr n s, () ##))

        {-# INLINE poke #-}

instance VulkanMarshal
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type StructFields VkPipelineRasterizationStateRasterizationOrderAMD
             = '["sType", "pNext", "rasterizationOrder"] -- ' closing tick for hsc2hs

        {-# INLINE newVkData #-}
        newVkData f
          | I## n <- sizeOf
                      (undefined :: VkPipelineRasterizationStateRasterizationOrderAMD),
            I## a <- alignment
                      (undefined :: VkPipelineRasterizationStateRasterizationOrderAMD)
            =
            IO
              (\ s0 ->
                 case newAlignedPinnedByteArray## n a s0 of
                     (## s1, mba ##) -> case unsafeFreezeByteArray## mba s1 of
                                          (## s2, ba ##) -> case f (Ptr (byteArrayContents## ba)) of
                                                              IO k -> case k s2 of
                                                                          (## s3, () ##) -> (## s3,
                                                                                             VkPipelineRasterizationStateRasterizationOrderAMD##
                                                                                               ba ##))

        {-# INLINE unsafePtr #-}
        unsafePtr (VkPipelineRasterizationStateRasterizationOrderAMD## ba)
          = Ptr (byteArrayContents## ba)

        {-# INLINE fromForeignPtr #-}
        fromForeignPtr
          = fromForeignPtr##
              VkPipelineRasterizationStateRasterizationOrderAMD##

        {-# INLINE toForeignPtr #-}
        toForeignPtr
          (VkPipelineRasterizationStateRasterizationOrderAMD## ba)
          = do ForeignPtr addr (PlainForeignPtr r) <- newForeignPtr_
                                                        (Ptr (byteArrayContents## ba))
               IO
                 (\ s -> (## s, ForeignPtr addr (MallocPtr (unsafeCoerce## ba) r) ##))

        {-# INLINE toPlainForeignPtr #-}
        toPlainForeignPtr
          (VkPipelineRasterizationStateRasterizationOrderAMD## ba)
          = IO
              (\ s ->
                 (## s,
                    ForeignPtr (byteArrayContents## ba)
                      (PlainPtr (unsafeCoerce## ba)) ##))

        {-# INLINE touchVkData #-}
        touchVkData
          x@(VkPipelineRasterizationStateRasterizationOrderAMD## ba)
          = IO (\ s -> (## touch## x (touch## ba s), () ##))

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineRasterizationStateRasterizationOrderAMD where
        type VkSTypeMType VkPipelineRasterizationStateRasterizationOrderAMD
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type FieldType "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineRasterizationStateRasterizationOrderAMD
             =
             #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, sType}

instance CanReadField "sType"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineRasterizationStateRasterizationOrderAMD where
        type VkPNextMType VkPipelineRasterizationStateRasterizationOrderAMD
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type FieldType "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineRasterizationStateRasterizationOrderAMD
             =
             #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, pNext}

instance CanReadField "pNext"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkRasterizationOrder
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type VkRasterizationOrderMType
               VkPipelineRasterizationStateRasterizationOrderAMD
             = VkRasterizationOrderAMD

        {-# NOINLINE vkRasterizationOrder #-}
        vkRasterizationOrder x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder})

        {-# INLINE vkRasterizationOrderByteOffset #-}
        vkRasterizationOrderByteOffset ~_
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

        {-# INLINE readVkRasterizationOrder #-}
        readVkRasterizationOrder p
          = peekByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

        {-# INLINE writeVkRasterizationOrder #-}
        writeVkRasterizationOrder p
          = pokeByteOff p #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

instance {-# OVERLAPPING #-}
         HasField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        type FieldType "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = VkRasterizationOrderAMD
        type FieldOptional "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "rasterizationOrder"
               VkPipelineRasterizationStateRasterizationOrderAMD
             =
             #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationStateRasterizationOrderAMD, rasterizationOrder}

instance CanReadField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE getField #-}
        getField = vkRasterizationOrder

        {-# INLINE readField #-}
        readField = readVkRasterizationOrder

instance CanWriteField "rasterizationOrder"
           VkPipelineRasterizationStateRasterizationOrderAMD
         where
        {-# INLINE writeField #-}
        writeField = writeVkRasterizationOrder

instance Show VkPipelineRasterizationStateRasterizationOrderAMD
         where
        showsPrec d x
          = showString "VkPipelineRasterizationStateRasterizationOrderAMD {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkRasterizationOrder = " .
                            showsPrec d (vkRasterizationOrder x) . showChar '}'

pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION :: (Num a, Eq a) =>
        a

pattern VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1

type VK_AMD_RASTERIZATION_ORDER_SPEC_VERSION = 1

pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: CString

pattern VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME <-
        (is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME -> True)
  where VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
          = _VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME

{-# INLINE _VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME #-}

_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: CString
_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  = Ptr "VK_AMD_rasterization_order\NUL"##

{-# INLINE is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME #-}

is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME
  = eqCStrings _VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME

type VK_AMD_RASTERIZATION_ORDER_EXTENSION_NAME =
     "VK_AMD_rasterization_order"

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
        = VkStructureType 1000018000
