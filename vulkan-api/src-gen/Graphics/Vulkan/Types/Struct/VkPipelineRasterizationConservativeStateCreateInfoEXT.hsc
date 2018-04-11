#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineRasterizationConservativeStateCreateInfoEXT
       (VkPipelineRasterizationConservativeStateCreateInfoEXT(..)) where
import           Foreign.Storable
                                                                                      (Storable (..))
import           GHC.Base
                                                                                      (Addr##,
                                                                                      ByteArray##,
                                                                                      byteArrayContents##,
                                                                                      plusAddr##)
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineRasterizationConservativeStateCreateInfoEXT VkPipelineRasterizationConservativeStateCreateInfoEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

instance {-# OVERLAPPING #-}
         CanWriteField "conservativeRasterizationMode"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, conservativeRasterizationMode}

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

instance {-# OVERLAPPING #-}
         CanReadField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

instance {-# OVERLAPPING #-}
         CanWriteField "extraPrimitiveOverestimationSize"
           VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineRasterizationConservativeStateCreateInfoEXT, extraPrimitiveOverestimationSize}

instance Show VkPipelineRasterizationConservativeStateCreateInfoEXT
         where
        showsPrec d x
          = showString
              "VkPipelineRasterizationConservativeStateCreateInfoEXT {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "conservativeRasterizationMode = " .
                                  showsPrec d (getField @"conservativeRasterizationMode" x) .
                                    showString ", " .
                                      showString "extraPrimitiveOverestimationSize = " .
                                        showsPrec d (getField @"extraPrimitiveOverestimationSize" x)
                                          . showChar '}'
