#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineRasterizationConservativeStateCreateInfoEXT
       (VkPipelineRasterizationConservativeStateCreateInfoEXT(..)) where
import           Foreign.Storable
                                                                                      (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks
                                                                                      (VkPipelineRasterizationConservativeStateCreateFlagsEXT)
import           Graphics.Vulkan.Types.Enum.VkConservativeRasterizationModeEXT
                                                                                      (VkConservativeRasterizationModeEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                      (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineRasterizationStateCreateInfo
                                                                                      (VkPipelineRasterizationStateCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                      (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineRasterizationConservativeStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineRasterizationConservativeStateCreateFlagsEXT           flags;
--   >     VkConservativeRasterizationModeEXT                                               conservativeRasterizationMode;
--   >     float                                                                            extraPrimitiveOverestimationSize;
--   > } VkPipelineRasterizationConservativeStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineRasterizationConservativeStateCreateInfoEXT.html VkPipelineRasterizationConservativeStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineRasterizationConservativeStateCreateInfoEXT = VkPipelineRasterizationConservativeStateCreateInfoEXT## Addr##
                                                                                                                    ByteArray##

instance Eq VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        (VkPipelineRasterizationConservativeStateCreateInfoEXT## a _) ==
          x@(VkPipelineRasterizationConservativeStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        (VkPipelineRasterizationConservativeStateCreateInfoEXT## a _)
          `compare`
          x@(VkPipelineRasterizationConservativeStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineRasterizationConservativeStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineRasterizationConservativeStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        unsafeAddr
          (VkPipelineRasterizationConservativeStateCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineRasterizationConservativeStateCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineRasterizationConservativeStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type StructFields
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             '["sType", "pNext", "flags", "conservativeRasterizationMode", -- ' closing tick for hsc2hs
               "extraPrimitiveOverestimationSize"]
        type CUnionType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type StructExtends
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = '[VkPipelineRasterizationStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type VkSTypeMType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "sType"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

instance CanReadField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type VkPNextMType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "pNext"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

instance CanReadField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkFlags VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type VkFlagsMType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkPipelineRasterizationConservativeStateCreateFlagsEXT

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         HasField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "flags"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkPipelineRasterizationConservativeStateCreateFlagsEXT
        type FieldOptional "flags"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}
        type FieldIsArray "flags"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

instance CanReadField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkConservativeRasterizationMode
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type VkConservativeRasterizationModeMType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkConservativeRasterizationModeEXT

        {-# NOINLINE vkConservativeRasterizationMode #-}
        vkConservativeRasterizationMode x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode})

        {-# INLINE vkConservativeRasterizationModeByteOffset #-}
        vkConservativeRasterizationModeByteOffset ~_
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

        {-# INLINE readVkConservativeRasterizationMode #-}
        readVkConservativeRasterizationMode p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

        {-# INLINE writeVkConservativeRasterizationMode #-}
        writeVkConservativeRasterizationMode p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

instance {-# OVERLAPPING #-}
         HasField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "conservativeRasterizationMode"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = VkConservativeRasterizationModeEXT
        type FieldOptional "conservativeRasterizationMode"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "conservativeRasterizationMode"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}
        type FieldIsArray "conservativeRasterizationMode"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

instance CanReadField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkConservativeRasterizationMode

        {-# INLINE readField #-}
        readField = readVkConservativeRasterizationMode

instance CanWriteField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkConservativeRasterizationMode

instance {-# OVERLAPPING #-}
         HasVkExtraPrimitiveOverestimationSize
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type VkExtraPrimitiveOverestimationSizeMType
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = #{type float}

        {-# NOINLINE vkExtraPrimitiveOverestimationSize #-}
        vkExtraPrimitiveOverestimationSize x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize})

        {-# INLINE vkExtraPrimitiveOverestimationSizeByteOffset #-}
        vkExtraPrimitiveOverestimationSizeByteOffset ~_
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

        {-# INLINE readVkExtraPrimitiveOverestimationSize #-}
        readVkExtraPrimitiveOverestimationSize p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

        {-# INLINE writeVkExtraPrimitiveOverestimationSize #-}
        writeVkExtraPrimitiveOverestimationSize p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

instance {-# OVERLAPPING #-}
         HasField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        type FieldType "extraPrimitiveOverestimationSize"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = #{type float}
        type FieldOptional "extraPrimitiveOverestimationSize"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extraPrimitiveOverestimationSize"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             =
             #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}
        type FieldIsArray "extraPrimitiveOverestimationSize"
               VkPipelineRasterizationConservativeStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

instance CanReadField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkExtraPrimitiveOverestimationSize

        {-# INLINE readField #-}
        readField = readVkExtraPrimitiveOverestimationSize

instance CanWriteField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkExtraPrimitiveOverestimationSize

instance Show VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        showsPrec d x
          = showString
              "VkPipelineRasterizationConservativeStateCreateInfoEXT {"
              .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkFlags = " .
                            showsPrec d (vkFlags x) .
                              showString ", " .
                                showString "vkConservativeRasterizationMode = " .
                                  showsPrec d (vkConservativeRasterizationMode x) .
                                    showString ", " .
                                      showString "vkExtraPrimitiveOverestimationSize = " .
                                        showsPrec d (vkExtraPrimitiveOverestimationSize x) .
                                          showChar '}'
