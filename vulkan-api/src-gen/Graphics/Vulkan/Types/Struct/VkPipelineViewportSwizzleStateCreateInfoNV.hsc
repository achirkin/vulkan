#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineViewportSwizzleStateCreateInfoNV
       (VkPipelineViewportSwizzleStateCreateInfoNV(..)) where
import           Foreign.Storable                                               (Storable (..))
import           GHC.Base                                                       (Addr##,
                                                                                 ByteArray##,
                                                                                 byteArrayContents##,
                                                                                 plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                                 (VkPipelineViewportSwizzleStateCreateFlagsNV)
import           Graphics.Vulkan.Types.Enum.VkStructureType                     (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineViewportStateCreateInfo (VkPipelineViewportStateCreateInfo)
import           Graphics.Vulkan.Types.Struct.VkViewportSwizzleNV               (VkViewportSwizzleNV)
import           System.IO.Unsafe                                               (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineViewportSwizzleStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineViewportSwizzleStateCreateFlagsNV    flags;
--   >     uint32_t               viewportCount;
--   >     const VkViewportSwizzleNV*      pViewportSwizzles;
--   > } VkPipelineViewportSwizzleStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineViewportSwizzleStateCreateInfoNV VkPipelineViewportSwizzleStateCreateInfoNV registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, sType}

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

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pNext}

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

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, flags}

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

instance {-# OVERLAPPING #-}
         CanReadField "viewportCount"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

instance {-# OVERLAPPING #-}
         CanWriteField "viewportCount"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, viewportCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "pViewportSwizzles"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

instance {-# OVERLAPPING #-}
         CanWriteField "pViewportSwizzles"
           VkPipelineViewportSwizzleStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineViewportSwizzleStateCreateInfoNV, pViewportSwizzles}

instance Show VkPipelineViewportSwizzleStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineViewportSwizzleStateCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "viewportCount = " .
                                  showsPrec d (getField @"viewportCount" x) .
                                    showString ", " .
                                      showString "pViewportSwizzles = " .
                                        showsPrec d (getField @"pViewportSwizzles" x) . showChar '}'
