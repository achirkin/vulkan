#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineViewportSwizzleStateCreateInfoNV
       (VkPipelineViewportSwizzleStateCreateInfoNV(..)) where
import           Foreign.Storable                                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                                 (VkPipelineViewportSwizzleStateCreateFlagsNV)
import           Graphics.Vulkan.Types.Enum.VkStructureType                     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo (VkPipelineViewportStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkViewportSwizzleNV               (VkViewportSwizzleNV)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                               (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineViewportSwizzleStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineViewportSwizzleStateCreateFlagsNV    flags;
--   >     uint32_t               viewportCount;
--   >     const VkViewportSwizzleNV*      pViewportSwizzles;
--   > } VkPipelineViewportSwizzleStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineViewportSwizzleStateCreateInfoNV.html VkPipelineViewportSwizzleStateCreateInfoNV registry at www.khronos.org>
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
        type FieldIsArray "sType"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pNext"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "flags"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "viewportCount"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

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
        type FieldIsArray "pViewportSwizzles"
               VkPipelineViewportSwizzleStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

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
