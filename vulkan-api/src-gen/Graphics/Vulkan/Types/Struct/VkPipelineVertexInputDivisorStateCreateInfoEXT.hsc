#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineVertexInputDivisorStateCreateInfoEXT
       (VkPipelineVertexInputDivisorStateCreateInfoEXT(..)) where
import           Foreign.Storable
                                                                                         (Storable (..))
import           GHC.Base
                                                                                         (Addr##,
                                                                                         ByteArray##,
                                                                                         byteArrayContents##,
                                                                                         plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                         (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineVertexInputStateCreateInfo
                                                                                         (VkPipelineVertexInputStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkVertexInputBindingDivisorDescriptionEXT
                                                                                         (VkVertexInputBindingDivisorDescriptionEXT)
import           System.IO.Unsafe
                                                                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineVertexInputDivisorStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*                         pNext;
--   >     uint32_t                            vertexBindingDivisorCount;
--   >     const VkVertexInputBindingDivisorDescriptionEXT*      pVertexBindingDivisors;
--   > } VkPipelineVertexInputDivisorStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPipelineVertexInputDivisorStateCreateInfoEXTVkPipelineVertexInputDivisorStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineVertexInputDivisorStateCreateInfoEXT = VkPipelineVertexInputDivisorStateCreateInfoEXT## Addr##
                                                                                                      ByteArray##

instance Eq VkPipelineVertexInputDivisorStateCreateInfoEXT where
        (VkPipelineVertexInputDivisorStateCreateInfoEXT## a _) ==
          x@(VkPipelineVertexInputDivisorStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineVertexInputDivisorStateCreateInfoEXT where
        (VkPipelineVertexInputDivisorStateCreateInfoEXT## a _) `compare`
          x@(VkPipelineVertexInputDivisorStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineVertexInputDivisorStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineVertexInputDivisorStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        unsafeAddr (VkPipelineVertexInputDivisorStateCreateInfoEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineVertexInputDivisorStateCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineVertexInputDivisorStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        type StructFields VkPipelineVertexInputDivisorStateCreateInfoEXT =
             '["sType", "pNext", "vertexBindingDivisorCount", -- ' closing tick for hsc2hs
               "pVertexBindingDivisors"]
        type CUnionType VkPipelineVertexInputDivisorStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineVertexInputDivisorStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineVertexInputDivisorStateCreateInfoEXT =
             '[VkPipelineVertexInputStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        type FieldType "sType"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             =
             #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        type FieldType "pNext"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             =
             #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "vertexBindingDivisorCount"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        type FieldType "vertexBindingDivisorCount"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = Word32
        type FieldOptional "vertexBindingDivisorCount"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "vertexBindingDivisorCount"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             =
             #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount}
        type FieldIsArray "vertexBindingDivisorCount"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount}

instance {-# OVERLAPPING #-}
         CanReadField "vertexBindingDivisorCount"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount}

instance {-# OVERLAPPING #-}
         CanWriteField "vertexBindingDivisorCount"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, vertexBindingDivisorCount}

instance {-# OVERLAPPING #-}
         HasField "pVertexBindingDivisors"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        type FieldType "pVertexBindingDivisors"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = Ptr VkVertexInputBindingDivisorDescriptionEXT
        type FieldOptional "pVertexBindingDivisors"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pVertexBindingDivisors"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             =
             #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors}
        type FieldIsArray "pVertexBindingDivisors"
               VkPipelineVertexInputDivisorStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors}

instance {-# OVERLAPPING #-}
         CanReadField "pVertexBindingDivisors"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors}

instance {-# OVERLAPPING #-}
         CanWriteField "pVertexBindingDivisors"
           VkPipelineVertexInputDivisorStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineVertexInputDivisorStateCreateInfoEXT, pVertexBindingDivisors}

instance Show VkPipelineVertexInputDivisorStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineVertexInputDivisorStateCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "vertexBindingDivisorCount = " .
                            showsPrec d (getField @"vertexBindingDivisorCount" x) .
                              showString ", " .
                                showString "pVertexBindingDivisors = " .
                                  showsPrec d (getField @"pVertexBindingDivisors" x) . showChar '}'
