#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineSampleLocationsStateCreateInfoEXT
       (VkPipelineSampleLocationsStateCreateInfoEXT(..)) where
import           Foreign.Storable
                                                                                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes
                                                                                    (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo
                                                                                    (VkPipelineMultisampleStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkSampleLocationsInfoEXT
                                                                                    (VkSampleLocationsInfoEXT)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                    (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineSampleLocationsStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32                         sampleLocationsEnable;
--   >     VkSampleLocationsInfoEXT         sampleLocationsInfo;
--   > } VkPipelineSampleLocationsStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineSampleLocationsStateCreateInfoEXT.html VkPipelineSampleLocationsStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineSampleLocationsStateCreateInfoEXT = VkPipelineSampleLocationsStateCreateInfoEXT## Addr##
                                                                                                ByteArray##

instance Eq VkPipelineSampleLocationsStateCreateInfoEXT where
        (VkPipelineSampleLocationsStateCreateInfoEXT## a _) ==
          x@(VkPipelineSampleLocationsStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineSampleLocationsStateCreateInfoEXT where
        (VkPipelineSampleLocationsStateCreateInfoEXT## a _) `compare`
          x@(VkPipelineSampleLocationsStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineSampleLocationsStateCreateInfoEXT where
        sizeOf ~_
          = #{size VkPipelineSampleLocationsStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineSampleLocationsStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        unsafeAddr (VkPipelineSampleLocationsStateCreateInfoEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineSampleLocationsStateCreateInfoEXT## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineSampleLocationsStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type StructFields VkPipelineSampleLocationsStateCreateInfoEXT =
             '["sType", "pNext", "sampleLocationsEnable", "sampleLocationsInfo"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineSampleLocationsStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineSampleLocationsStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineSampleLocationsStateCreateInfoEXT =
             '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineSampleLocationsStateCreateInfoEXT where
        type VkSTypeMType VkPipelineSampleLocationsStateCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineSampleLocationsStateCreateInfoEXT where
        type FieldType "sType" VkPipelineSampleLocationsStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

instance CanReadField "sType"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineSampleLocationsStateCreateInfoEXT where
        type VkPNextMType VkPipelineSampleLocationsStateCreateInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineSampleLocationsStateCreateInfoEXT where
        type FieldType "pNext" VkPipelineSampleLocationsStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

instance CanReadField "pNext"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsEnable
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type VkSampleLocationsEnableMType
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkBool32

        {-# NOINLINE vkSampleLocationsEnable #-}
        vkSampleLocationsEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable})

        {-# INLINE vkSampleLocationsEnableByteOffset #-}
        vkSampleLocationsEnableByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

        {-# INLINE readVkSampleLocationsEnable #-}
        readVkSampleLocationsEnable p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

        {-# INLINE writeVkSampleLocationsEnable #-}
        writeVkSampleLocationsEnable p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type FieldType "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkBool32
        type FieldOptional "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}
        type FieldIsArray "sampleLocationsEnable"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

instance CanReadField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationsEnable

        {-# INLINE readField #-}
        readField = readVkSampleLocationsEnable

instance CanWriteField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationsEnable

instance {-# OVERLAPPING #-}
         HasVkSampleLocationsInfo
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type VkSampleLocationsInfoMType
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkSampleLocationsInfoEXT

        {-# NOINLINE vkSampleLocationsInfo #-}
        vkSampleLocationsInfo x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo})

        {-# INLINE vkSampleLocationsInfoByteOffset #-}
        vkSampleLocationsInfoByteOffset ~_
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

        {-# INLINE readVkSampleLocationsInfo #-}
        readVkSampleLocationsInfo p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

        {-# INLINE writeVkSampleLocationsInfo #-}
        writeVkSampleLocationsInfo p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         HasField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        type FieldType "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = VkSampleLocationsInfoEXT
        type FieldOptional "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             =
             #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}
        type FieldIsArray "sampleLocationsInfo"
               VkPipelineSampleLocationsStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

instance CanReadField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSampleLocationsInfo

        {-# INLINE readField #-}
        readField = readVkSampleLocationsInfo

instance CanWriteField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSampleLocationsInfo

instance Show VkPipelineSampleLocationsStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineSampleLocationsStateCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSampleLocationsEnable = " .
                            showsPrec d (vkSampleLocationsEnable x) .
                              showString ", " .
                                showString "vkSampleLocationsInfo = " .
                                  showsPrec d (vkSampleLocationsInfo x) . showChar '}'
