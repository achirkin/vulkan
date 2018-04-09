#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineColorBlendAdvancedStateCreateInfoEXT
       (VkPipelineColorBlendAdvancedStateCreateInfoEXT(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Base
                                                                                   (Addr##,
                                                                                   ByteArray##,
                                                                                   byteArrayContents##,
                                                                                   plusAddr##)
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPipelineColorBlendAdvancedStateCreateInfoEXTVkPipelineColorBlendAdvancedStateCreateInfoEXT registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

instance {-# OVERLAPPING #-}
         CanWriteField "srcPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, srcPremultiplied}

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

instance {-# OVERLAPPING #-}
         CanReadField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

instance {-# OVERLAPPING #-}
         CanWriteField "dstPremultiplied"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, dstPremultiplied}

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

instance {-# OVERLAPPING #-}
         CanReadField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

instance {-# OVERLAPPING #-}
         CanWriteField "blendOverlap"
           VkPipelineColorBlendAdvancedStateCreateInfoEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAdvancedStateCreateInfoEXT, blendOverlap}

instance Show VkPipelineColorBlendAdvancedStateCreateInfoEXT where
        showsPrec d x
          = showString "VkPipelineColorBlendAdvancedStateCreateInfoEXT {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "srcPremultiplied = " .
                            showsPrec d (getField @"srcPremultiplied" x) .
                              showString ", " .
                                showString "dstPremultiplied = " .
                                  showsPrec d (getField @"dstPremultiplied" x) .
                                    showString ", " .
                                      showString "blendOverlap = " .
                                        showsPrec d (getField @"blendOverlap" x) . showChar '}'
