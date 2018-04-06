#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineViewportWScalingStateCreateInfoNV
       (VkPipelineViewportWScalingStateCreateInfoNV(..)) where
import           Foreign.Storable                                               (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                                (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType                     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo (VkPipelineViewportStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkViewportWScalingNV              (VkViewportWScalingNV)
import           System.IO.Unsafe                                               (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineViewportWScalingStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkBool32               viewportWScalingEnable;
--   >     uint32_t               viewportCount;
--   >     const VkViewportWScalingNV*      pViewportWScalings;
--   > } VkPipelineViewportWScalingStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPipelineViewportWScalingStateCreateInfoNV.html VkPipelineViewportWScalingStateCreateInfoNV registry at www.khronos.org>
data VkPipelineViewportWScalingStateCreateInfoNV = VkPipelineViewportWScalingStateCreateInfoNV## Addr##
                                                                                                ByteArray##

instance Eq VkPipelineViewportWScalingStateCreateInfoNV where
        (VkPipelineViewportWScalingStateCreateInfoNV## a _) ==
          x@(VkPipelineViewportWScalingStateCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineViewportWScalingStateCreateInfoNV where
        (VkPipelineViewportWScalingStateCreateInfoNV## a _) `compare`
          x@(VkPipelineViewportWScalingStateCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineViewportWScalingStateCreateInfoNV where
        sizeOf ~_
          = #{size VkPipelineViewportWScalingStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineViewportWScalingStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        unsafeAddr (VkPipelineViewportWScalingStateCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineViewportWScalingStateCreateInfoNV## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineViewportWScalingStateCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineViewportWScalingStateCreateInfoNV
         where
        type StructFields VkPipelineViewportWScalingStateCreateInfoNV =
             '["sType", "pNext", "viewportWScalingEnable", "viewportCount", -- ' closing tick for hsc2hs
               "pViewportWScalings"]
        type CUnionType VkPipelineViewportWScalingStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineViewportWScalingStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineViewportWScalingStateCreateInfoNV =
             '[VkPipelineViewportStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineViewportWScalingStateCreateInfoNV where
        type FieldType "sType" VkPipelineViewportWScalingStateCreateInfoNV
             = VkStructureType
        type FieldOptional "sType"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}
        type FieldIsArray "sType"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineViewportWScalingStateCreateInfoNV where
        type FieldType "pNext" VkPipelineViewportWScalingStateCreateInfoNV
             = Ptr Void
        type FieldOptional "pNext"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}
        type FieldIsArray "pNext"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "viewportWScalingEnable"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type FieldType "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             = VkBool32
        type FieldOptional "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}
        type FieldIsArray "viewportWScalingEnable"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

instance {-# OVERLAPPING #-}
         CanReadField "viewportWScalingEnable"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "viewportWScalingEnable"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportWScalingEnable}

instance {-# OVERLAPPING #-}
         HasField "viewportCount"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type FieldType "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             = Word32
        type FieldOptional "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}
        type FieldIsArray "viewportCount"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         CanReadField "viewportCount"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         CanWriteField "viewportCount"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         HasField "pViewportWScalings"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        type FieldType "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             = Ptr VkViewportWScalingNV
        type FieldOptional "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             =
             #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}
        type FieldIsArray "pViewportWScalings"
               VkPipelineViewportWScalingStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

instance {-# OVERLAPPING #-}
         CanReadField "pViewportWScalings"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewportWScalings"
           VkPipelineViewportWScalingStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportWScalingStateCreateInfoNV, pViewportWScalings}

instance Show VkPipelineViewportWScalingStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineViewportWScalingStateCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "viewportWScalingEnable = " .
                            showsPrec d (getField @"viewportWScalingEnable" x) .
                              showString ", " .
                                showString "viewportCount = " .
                                  showsPrec d (getField @"viewportCount" x) .
                                    showString ", " .
                                      showString "pViewportWScalings = " .
                                        showsPrec d (getField @"pViewportWScalings" x) .
                                          showChar '}'
