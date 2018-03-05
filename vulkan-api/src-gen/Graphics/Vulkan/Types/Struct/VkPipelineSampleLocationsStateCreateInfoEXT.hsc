#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationsEnable"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsEnable}

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

instance {-# OVERLAPPING #-}
         CanReadField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "sampleLocationsInfo"
           VkPipelineSampleLocationsStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineSampleLocationsStateCreateInfoEXT, sampleLocationsInfo}

instance Show VkPipelineSampleLocationsStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineSampleLocationsStateCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "sampleLocationsEnable = " .
                            showsPrec d (getField @"sampleLocationsEnable" x) .
                              showString ", " .
                                showString "sampleLocationsInfo = " .
                                  showsPrec d (getField @"sampleLocationsInfo" x) . showChar '}'
