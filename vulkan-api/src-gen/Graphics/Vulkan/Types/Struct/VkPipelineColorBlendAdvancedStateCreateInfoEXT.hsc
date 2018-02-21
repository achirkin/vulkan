#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineColorBlendAdvancedStateCreateInfoEXT
       (VkPipelineColorBlendAdvancedStateCreateInfoEXT(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes
                                                                                   (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkBlendOverlapEXT
                                                                                   (VkBlendOverlapEXT)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineColorBlendStateCreateInfo
                                                                                   (VkPipelineColorBlendStateCreateInfo)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineColorBlendAdvancedStateCreateInfoEXT {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkBool32               srcPremultiplied;
--   >     VkBool32               dstPremultiplied;
--   >     VkBlendOverlapEXT      blendOverlap;
--   > } VkPipelineColorBlendAdvancedStateCreateInfoEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineColorBlendAdvancedStateCreateInfoEXT.html VkPipelineColorBlendAdvancedStateCreateInfoEXT registry at www.khronos.org>
data VkPipelineColorBlendAdvancedStateCreateInfoEXT = VkPipelineColorBlendAdvancedStateCreateInfoEXT## Addr##
                                                                                                      ByteArray##

instance Eq VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a _) ==
          x@(VkPipelineColorBlendAdvancedStateCreateInfoEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a _) `compare`
          x@(VkPipelineColorBlendAdvancedStateCreateInfoEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        sizeOf ~_
          = #{size VkPipelineColorBlendAdvancedStateCreateInfoEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineColorBlendAdvancedStateCreateInfoEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        unsafeAddr (VkPipelineColorBlendAdvancedStateCreateInfoEXT## a _)
          = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPipelineColorBlendAdvancedStateCreateInfoEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineColorBlendAdvancedStateCreateInfoEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type StructFields VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             '["sType", "pNext", "srcPremultiplied", "dstPremultiplied", -- ' closing tick for hsc2hs
               "blendOverlap"]
        type CUnionType VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             '[VkPipelineColorBlendStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkSType VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        type VkSTypeMType VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}
        type FieldIsArray "sType"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

instance CanReadField "sType"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        type VkPNextMType VkPipelineColorBlendAdvancedStateCreateInfoEXT =
             Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}
        type FieldIsArray "pNext"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

instance CanReadField "pNext"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkSrcPremultiplied
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type VkSrcPremultipliedMType
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBool32

        {-# NOINLINE vkSrcPremultiplied #-}
        vkSrcPremultiplied x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied})

        {-# INLINE vkSrcPremultipliedByteOffset #-}
        vkSrcPremultipliedByteOffset ~_
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

        {-# INLINE readVkSrcPremultiplied #-}
        readVkSrcPremultiplied p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

        {-# INLINE writeVkSrcPremultiplied #-}
        writeVkSrcPremultiplied p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

instance {-# OVERLAPPING #-}
         HasField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBool32
        type FieldOptional "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}
        type FieldIsArray "srcPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

instance CanReadField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkSrcPremultiplied

        {-# INLINE readField #-}
        readField = readVkSrcPremultiplied

instance CanWriteField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkSrcPremultiplied

instance {-# OVERLAPPING #-}
         HasVkDstPremultiplied
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type VkDstPremultipliedMType
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBool32

        {-# NOINLINE vkDstPremultiplied #-}
        vkDstPremultiplied x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied})

        {-# INLINE vkDstPremultipliedByteOffset #-}
        vkDstPremultipliedByteOffset ~_
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

        {-# INLINE readVkDstPremultiplied #-}
        readVkDstPremultiplied p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

        {-# INLINE writeVkDstPremultiplied #-}
        writeVkDstPremultiplied p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

instance {-# OVERLAPPING #-}
         HasField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBool32
        type FieldOptional "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}
        type FieldIsArray "dstPremultiplied"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

instance CanReadField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkDstPremultiplied

        {-# INLINE readField #-}
        readField = readVkDstPremultiplied

instance CanWriteField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkDstPremultiplied

instance {-# OVERLAPPING #-}
         HasVkBlendOverlap VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type VkBlendOverlapMType
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBlendOverlapEXT

        {-# NOINLINE vkBlendOverlap #-}
        vkBlendOverlap x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap})

        {-# INLINE vkBlendOverlapByteOffset #-}
        vkBlendOverlapByteOffset ~_
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

        {-# INLINE readVkBlendOverlap #-}
        readVkBlendOverlap p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

        {-# INLINE writeVkBlendOverlap #-}
        writeVkBlendOverlap p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

instance {-# OVERLAPPING #-}
         HasField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        type FieldType "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = VkBlendOverlapEXT
        type FieldOptional "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             =
             #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}
        type FieldIsArray "blendOverlap"
               VkPipelineColorBlendAdvancedStateCreateInfoEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

instance CanReadField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE getField #-}
        getField = vkBlendOverlap

        {-# INLINE readField #-}
        readField = readVkBlendOverlap

instance CanWriteField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField = writeVkBlendOverlap

instance Show VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineColorBlendAdvancedStateCreateInfoEXT {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkSrcPremultiplied = " .
                            showsPrec d (vkSrcPremultiplied x) .
                              showString ", " .
                                showString "vkDstPremultiplied = " .
                                  showsPrec d (vkDstPremultiplied x) .
                                    showString ", " .
                                      showString "vkBlendOverlap = " .
                                        showsPrec d (vkBlendOverlap x) . showChar '}'
