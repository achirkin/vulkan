#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineCoverageToColorStateCreateInfoNV
       (VkPipelineCoverageToColorStateCreateInfoNV(..)) where
import           Foreign.Storable
                                                                                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes
                                                                                    (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks
                                                                                    (VkPipelineCoverageToColorStateCreateFlagsNV)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPipelineMultisampleStateCreateInfo
                                                                                    (VkPipelineMultisampleStateCreateInfo)
import           System.IO.Unsafe
                                                                                    (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineCoverageToColorStateCreateInfoNV {
--   >     VkStructureType sType;
--   >     const void*                                                                      pNext;
--   >     VkPipelineCoverageToColorStateCreateFlagsNV                    flags;
--   >     VkBool32                         coverageToColorEnable;
--   >     uint32_t         coverageToColorLocation;
--   > } VkPipelineCoverageToColorStateCreateInfoNV;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPipelineCoverageToColorStateCreateInfoNV.html VkPipelineCoverageToColorStateCreateInfoNV registry at www.khronos.org>
data VkPipelineCoverageToColorStateCreateInfoNV = VkPipelineCoverageToColorStateCreateInfoNV## Addr##
                                                                                              ByteArray##

instance Eq VkPipelineCoverageToColorStateCreateInfoNV where
        (VkPipelineCoverageToColorStateCreateInfoNV## a _) ==
          x@(VkPipelineCoverageToColorStateCreateInfoNV## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineCoverageToColorStateCreateInfoNV where
        (VkPipelineCoverageToColorStateCreateInfoNV## a _) `compare`
          x@(VkPipelineCoverageToColorStateCreateInfoNV## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineCoverageToColorStateCreateInfoNV where
        sizeOf ~_
          = #{size VkPipelineCoverageToColorStateCreateInfoNV}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineCoverageToColorStateCreateInfoNV}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        unsafeAddr (VkPipelineCoverageToColorStateCreateInfoNV## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineCoverageToColorStateCreateInfoNV## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineCoverageToColorStateCreateInfoNV##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineCoverageToColorStateCreateInfoNV
         where
        type StructFields VkPipelineCoverageToColorStateCreateInfoNV =
             '["sType", "pNext", "flags", "coverageToColorEnable", -- ' closing tick for hsc2hs
               "coverageToColorLocation"]
        type CUnionType VkPipelineCoverageToColorStateCreateInfoNV = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineCoverageToColorStateCreateInfoNV =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineCoverageToColorStateCreateInfoNV =
             '[VkPipelineMultisampleStateCreateInfo] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineCoverageToColorStateCreateInfoNV where
        type FieldType "sType" VkPipelineCoverageToColorStateCreateInfoNV =
             VkStructureType
        type FieldOptional "sType"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineCoverageToColorStateCreateInfoNV
             =
             #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}
        type FieldIsArray "sType"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineCoverageToColorStateCreateInfoNV where
        type FieldType "pNext" VkPipelineCoverageToColorStateCreateInfoNV =
             Ptr Void
        type FieldOptional "pNext"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineCoverageToColorStateCreateInfoNV
             =
             #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}
        type FieldIsArray "pNext"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineCoverageToColorStateCreateInfoNV where
        type FieldType "flags" VkPipelineCoverageToColorStateCreateInfoNV =
             VkPipelineCoverageToColorStateCreateFlagsNV
        type FieldOptional "flags"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineCoverageToColorStateCreateInfoNV
             =
             #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}
        type FieldIsArray "flags"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, flags}

instance {-# OVERLAPPING #-}
         HasField "coverageToColorEnable"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        type FieldType "coverageToColorEnable"
               VkPipelineCoverageToColorStateCreateInfoNV
             = VkBool32
        type FieldOptional "coverageToColorEnable"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "coverageToColorEnable"
               VkPipelineCoverageToColorStateCreateInfoNV
             =
             #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}
        type FieldIsArray "coverageToColorEnable"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}

instance {-# OVERLAPPING #-}
         CanReadField "coverageToColorEnable"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "coverageToColorEnable"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorEnable}

instance {-# OVERLAPPING #-}
         HasField "coverageToColorLocation"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        type FieldType "coverageToColorLocation"
               VkPipelineCoverageToColorStateCreateInfoNV
             = Word32
        type FieldOptional "coverageToColorLocation"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "coverageToColorLocation"
               VkPipelineCoverageToColorStateCreateInfoNV
             =
             #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}
        type FieldIsArray "coverageToColorLocation"
               VkPipelineCoverageToColorStateCreateInfoNV
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}

instance {-# OVERLAPPING #-}
         CanReadField "coverageToColorLocation"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}

instance {-# OVERLAPPING #-}
         CanWriteField "coverageToColorLocation"
           VkPipelineCoverageToColorStateCreateInfoNV
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineCoverageToColorStateCreateInfoNV, coverageToColorLocation}

instance Show VkPipelineCoverageToColorStateCreateInfoNV where
        showsPrec d x
          = showString "VkPipelineCoverageToColorStateCreateInfoNV {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "coverageToColorEnable = " .
                                  showsPrec d (getField @"coverageToColorEnable" x) .
                                    showString ", " .
                                      showString "coverageToColorLocation = " .
                                        showsPrec d (getField @"coverageToColorLocation" x) .
                                          showChar '}'
