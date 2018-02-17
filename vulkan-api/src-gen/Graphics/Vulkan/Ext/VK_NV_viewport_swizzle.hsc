#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Graphics.Vulkan.Ext.VK_NV_viewport_swizzle
       (-- * Vulkan extension: @VK_NV_viewport_swizzle@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Piers Daniell @pdaniell@
        --
        -- author: @NV@
        --
        -- type: @device@
        --
        -- Extension number: @99@
        VkViewportSwizzleNV(..),
        VkPipelineViewportSwizzleStateCreateInfoNV(..),
        VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION,
        pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION,
        VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME,
        pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME,
        pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV)
       where
import           Foreign.C.String                 (CString)
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           Graphics.Vulkan.Base             (VkPipelineViewportStateCreateInfo)
import           Graphics.Vulkan.Common           (VkPipelineViewportSwizzleStateCreateFlagsNV,
                                                   VkStructureType,
                                                   VkStructureType (..),
                                                   VkViewportCoordinateSwizzleNV,
                                                   Word32)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkViewportSwizzleNV {
--   >     VkViewportCoordinateSwizzleNV          x;
--   >     VkViewportCoordinateSwizzleNV          y;
--   >     VkViewportCoordinateSwizzleNV          z;
--   >     VkViewportCoordinateSwizzleNV          w;
--   > } VkViewportSwizzleNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkViewportSwizzleNV.html VkViewportSwizzleNV registry at www.khronos.org>
data VkViewportSwizzleNV = VkViewportSwizzleNV## Addr## ByteArray##

instance Eq VkViewportSwizzleNV where
        (VkViewportSwizzleNV## a _) == x@(VkViewportSwizzleNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkViewportSwizzleNV where
        (VkViewportSwizzleNV## a _) `compare` x@(VkViewportSwizzleNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkViewportSwizzleNV where
        sizeOf ~_ = #{size VkViewportSwizzleNV}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkViewportSwizzleNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkViewportSwizzleNV where
        unsafeAddr (VkViewportSwizzleNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkViewportSwizzleNV## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkViewportSwizzleNV## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkViewportSwizzleNV where
        type StructFields VkViewportSwizzleNV = '["x", "y", "z", "w"] -- ' closing tick for hsc2hs
        type CUnionType VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type StructExtends VkViewportSwizzleNV = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkX VkViewportSwizzleNV where
        type VkXMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_ = #{offset VkViewportSwizzleNV, x}

        {-# INLINE readVkX #-}
        readVkX p
          = peekByteOff p #{offset VkViewportSwizzleNV, x}

        {-# INLINE writeVkX #-}
        writeVkX p
          = pokeByteOff p #{offset VkViewportSwizzleNV, x}

instance {-# OVERLAPPING #-} HasField "x" VkViewportSwizzleNV where
        type FieldType "x" VkViewportSwizzleNV =
             VkViewportCoordinateSwizzleNV
        type FieldOptional "x" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "x" VkViewportSwizzleNV =
             #{offset VkViewportSwizzleNV, x}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportSwizzleNV, x}

instance CanReadField "x" VkViewportSwizzleNV where
        {-# INLINE getField #-}
        getField = vkX

        {-# INLINE readField #-}
        readField = readVkX

instance CanWriteField "x" VkViewportSwizzleNV where
        {-# INLINE writeField #-}
        writeField = writeVkX

instance {-# OVERLAPPING #-} HasVkY VkViewportSwizzleNV where
        type VkYMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_ = #{offset VkViewportSwizzleNV, y}

        {-# INLINE readVkY #-}
        readVkY p
          = peekByteOff p #{offset VkViewportSwizzleNV, y}

        {-# INLINE writeVkY #-}
        writeVkY p
          = pokeByteOff p #{offset VkViewportSwizzleNV, y}

instance {-# OVERLAPPING #-} HasField "y" VkViewportSwizzleNV where
        type FieldType "y" VkViewportSwizzleNV =
             VkViewportCoordinateSwizzleNV
        type FieldOptional "y" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "y" VkViewportSwizzleNV =
             #{offset VkViewportSwizzleNV, y}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportSwizzleNV, y}

instance CanReadField "y" VkViewportSwizzleNV where
        {-# INLINE getField #-}
        getField = vkY

        {-# INLINE readField #-}
        readField = readVkY

instance CanWriteField "y" VkViewportSwizzleNV where
        {-# INLINE writeField #-}
        writeField = writeVkY

instance {-# OVERLAPPING #-} HasVkZ VkViewportSwizzleNV where
        type VkZMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkZ #-}
        vkZ x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, z})

        {-# INLINE vkZByteOffset #-}
        vkZByteOffset ~_ = #{offset VkViewportSwizzleNV, z}

        {-# INLINE readVkZ #-}
        readVkZ p
          = peekByteOff p #{offset VkViewportSwizzleNV, z}

        {-# INLINE writeVkZ #-}
        writeVkZ p
          = pokeByteOff p #{offset VkViewportSwizzleNV, z}

instance {-# OVERLAPPING #-} HasField "z" VkViewportSwizzleNV where
        type FieldType "z" VkViewportSwizzleNV =
             VkViewportCoordinateSwizzleNV
        type FieldOptional "z" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "z" VkViewportSwizzleNV =
             #{offset VkViewportSwizzleNV, z}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportSwizzleNV, z}

instance CanReadField "z" VkViewportSwizzleNV where
        {-# INLINE getField #-}
        getField = vkZ

        {-# INLINE readField #-}
        readField = readVkZ

instance CanWriteField "z" VkViewportSwizzleNV where
        {-# INLINE writeField #-}
        writeField = writeVkZ

instance {-# OVERLAPPING #-} HasVkW VkViewportSwizzleNV where
        type VkWMType VkViewportSwizzleNV = VkViewportCoordinateSwizzleNV

        {-# NOINLINE vkW #-}
        vkW x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkViewportSwizzleNV, w})

        {-# INLINE vkWByteOffset #-}
        vkWByteOffset ~_ = #{offset VkViewportSwizzleNV, w}

        {-# INLINE readVkW #-}
        readVkW p
          = peekByteOff p #{offset VkViewportSwizzleNV, w}

        {-# INLINE writeVkW #-}
        writeVkW p
          = pokeByteOff p #{offset VkViewportSwizzleNV, w}

instance {-# OVERLAPPING #-} HasField "w" VkViewportSwizzleNV where
        type FieldType "w" VkViewportSwizzleNV =
             VkViewportCoordinateSwizzleNV
        type FieldOptional "w" VkViewportSwizzleNV = 'False -- ' closing tick for hsc2hs
        type FieldOffset "w" VkViewportSwizzleNV =
             #{offset VkViewportSwizzleNV, w}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkViewportSwizzleNV, w}

instance CanReadField "w" VkViewportSwizzleNV where
        {-# INLINE getField #-}
        getField = vkW

        {-# INLINE readField #-}
        readField = readVkW

instance CanWriteField "w" VkViewportSwizzleNV where
        {-# INLINE writeField #-}
        writeField = writeVkW

instance Show VkViewportSwizzleNV where
        showsPrec d x
          = showString "VkViewportSwizzleNV {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " .
                      showsPrec d (vkY x) .
                        showString ", " .
                          showString "vkZ = " .
                            showsPrec d (vkZ x) .
                              showString ", " .
                                showString "vkW = " . showsPrec d (vkW x) . showChar '}'

-- | > typedef struct VkPipelineViewportSwizzleStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineViewportSwizzleStateCreateFlagsNV    flags;
--   >     uint32_t               viewportCount;
--   >     const VkViewportSwizzleNV*      pViewportSwizzles;
--   > } VkPipelineViewportSwizzleStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0/man/html/VkPipelineViewportSwizzleStateCreateInfoNV.html VkPipelineViewportSwizzleStateCreateInfoNV registry at www.khronos.org>
data VkPipelineViewportSwizzleStateCreateInfoNV = VkPipelineViewportSwizzleStateCreateInfoNV## Addr##
                                                                                              ByteArray##

instance Eq VkPipelineViewportSwizzleStateCreateInfoNV where
        (VkPipelineViewportSwizzleStateCreateInfoNV## a _) ==
          x@(VkPipelineViewportSwizzleStateCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineViewportSwizzleStateCreateInfoNV where
        (VkPipelineViewportSwizzleStateCreateInfoNV## a _) `compare`
          x@(VkPipelineViewportSwizzleStateCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineViewportSwizzleStateCreateInfoNV where
        sizeOf ~_
          = #{size VkPipelineViewportSwizzleStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineViewportSwizzleStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        unsafeAddr (VkPipelineViewportSwizzleStateCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineViewportSwizzleStateCreateInfoNV## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineViewportSwizzleStateCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineViewportSwizzleStateCreateInfoNV
         where
        type StructFields VkPipelineViewportSwizzleStateCreateInfoNV =
             '["sType", "pNext", "flags", "viewportCount", "pViewportSwizzles"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineViewportSwizzleStateCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineViewportSwizzleStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineViewportSwizzleStateCreateInfoNV =
             '[VkPipelineViewportStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineViewportSwizzleStateCreateInfoNV where
        type VkSTypeMType VkPipelineViewportSwizzleStateCreateInfoNV =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineViewportSwizzleStateCreateInfoNV where
        type FieldType "sType" VkPipelineViewportSwizzleStateCreateInfoNV =
             VkStructureType
        type FieldOptional "sType"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineViewportSwizzleStateCreateInfoNV
             =
             #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

instance CanReadField "sType"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineViewportSwizzleStateCreateInfoNV where
        type VkPNextMType VkPipelineViewportSwizzleStateCreateInfoNV =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineViewportSwizzleStateCreateInfoNV where
        type FieldType "pNext" VkPipelineViewportSwizzleStateCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineViewportSwizzleStateCreateInfoNV
             =
             #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

instance CanReadField "pNext"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineViewportSwizzleStateCreateInfoNV where
        type VkFlagsMType VkPipelineViewportSwizzleStateCreateInfoNV =
             VkPipelineViewportSwizzleStateCreateFlagsNV

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineViewportSwizzleStateCreateInfoNV where
        type FieldType "flags" VkPipelineViewportSwizzleStateCreateInfoNV =
             VkPipelineViewportSwizzleStateCreateFlagsNV
        type FieldOptional "flags"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineViewportSwizzleStateCreateInfoNV
             =
             #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

instance CanReadField "flags"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkViewportCount VkPipelineViewportSwizzleStateCreateInfoNV where
        type VkViewportCountMType
               VkPipelineViewportSwizzleStateCreateInfoNV
             = Word32

        {-# NOINLINE vkViewportCount #-}
        vkViewportCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount})

        {-# INLINE vkViewportCountByteOffset #-}
        vkViewportCountByteOffset ~_
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

        {-# INLINE readVkViewportCount #-}
        readVkViewportCount p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

        {-# INLINE writeVkViewportCount #-}
        writeVkViewportCount p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         HasField "viewportCount" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        type FieldType "viewportCount"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = Word32
        type FieldOptional "viewportCount"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportCount"
               VkPipelineViewportSwizzleStateCreateInfoNV
             =
             #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

instance CanReadField "viewportCount"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkViewportCount

        {-# INLINE readField #-}
        readField = readVkViewportCount

instance CanWriteField "viewportCount"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkViewportCount

instance {-# OVERLAPPING #-}
         HasVkPViewportSwizzles VkPipelineViewportSwizzleStateCreateInfoNV
         where
        type VkPViewportSwizzlesMType
               VkPipelineViewportSwizzleStateCreateInfoNV
             = Ptr VkViewportSwizzleNV

        {-# NOINLINE vkPViewportSwizzles #-}
        vkPViewportSwizzles x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles})

        {-# INLINE vkPViewportSwizzlesByteOffset #-}
        vkPViewportSwizzlesByteOffset ~_
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

        {-# INLINE readVkPViewportSwizzles #-}
        readVkPViewportSwizzles p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

        {-# INLINE writeVkPViewportSwizzles #-}
        writeVkPViewportSwizzles p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

instance {-# OVERLAPPING #-}
         HasField "pViewportSwizzles"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        type FieldType "pViewportSwizzles"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = Ptr VkViewportSwizzleNV
        type FieldOptional "pViewportSwizzles"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pViewportSwizzles"
               VkPipelineViewportSwizzleStateCreateInfoNV
             =
             #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

instance CanReadField "pViewportSwizzles"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE getField #-}
        getField = vkPViewportSwizzles

        {-# INLINE readField #-}
        readField = readVkPViewportSwizzles

instance CanWriteField "pViewportSwizzles"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField = writeVkPViewportSwizzles

instance Show VkPipelineViewportSwizzleStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineViewportSwizzleStateCreateInfoNV {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkViewportCount = " .
                                  showsPrec d (vkViewportCount x) .
                                    showString ", " .
                                      showString "vkPViewportSwizzles = " .
                                        showsPrec d (vkPViewportSwizzles x) . showChar '}'

pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1

type VK_NV_VIEWPORT_SWIZZLE_SPEC_VERSION = 1

pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: CString

pattern VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME <-
        (is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME -> True)
  where VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
          = _VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME

{-# INLINE _VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME #-}

_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: CString
_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  = Ptr "VK_NV_viewport_swizzle\NUL"##

{-# INLINE is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME #-}

is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME :: CString -> Bool
is_VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME
  = eqCStrings _VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME

type VK_NV_VIEWPORT_SWIZZLE_EXTENSION_NAME =
     "VK_NV_viewport_swizzle"

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
        :: VkStructureType

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
        = VkStructureType 1000098000
