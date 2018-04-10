#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
       (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Base                                                 (Addr##,
                                                                           ByteArray##,
                                                                           byteArrayContents##,
                                                                           plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT {
--   >     VkStructureType sType;
--   >     void*                            pNext;
--   >     uint32_t                         advancedBlendMaxColorAttachments;
--   >     VkBool32                         advancedBlendIndependentBlend;
--   >     VkBool32                         advancedBlendNonPremultipliedSrcColor;
--   >     VkBool32                         advancedBlendNonPremultipliedDstColor;
--   >     VkBool32                         advancedBlendCorrelatedOverlap;
--   >     VkBool32                         advancedBlendAllOperations;
--   > } VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT registry at www.khronos.org>
data VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT = VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## Addr##
                                                                                                                ByteArray##

instance Eq VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## a _) ==
          x@(VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## a _)
          `compare`
          x@(VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        sizeOf ~_
          = #{size VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        unsafeAddr
          (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray
          (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type StructFields
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             '["sType", "pNext", "advancedBlendMaxColorAttachments", -- ' closing tick for hsc2hs
               "advancedBlendIndependentBlend",
               "advancedBlendNonPremultipliedSrcColor",
               "advancedBlendNonPremultipliedDstColor",
               "advancedBlendCorrelatedOverlap", "advancedBlendAllOperations"]
        type CUnionType VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'True -- ' closing tick for hsc2hs
        type StructExtends
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "sType"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkStructureType
        type FieldOptional "sType"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType}
        type FieldIsArray "sType"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "pNext"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = Ptr Void
        type FieldOptional "pNext"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext}
        type FieldIsArray "pNext"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, pNext}

instance {-# OVERLAPPING #-}
         HasField "advancedBlendMaxColorAttachments"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendMaxColorAttachments"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = Word32
        type FieldOptional "advancedBlendMaxColorAttachments"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendMaxColorAttachments"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments}
        type FieldIsArray "advancedBlendMaxColorAttachments"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments}

instance {-# OVERLAPPING #-}
         CanReadField "advancedBlendMaxColorAttachments"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments}

instance {-# OVERLAPPING #-}
         CanWriteField "advancedBlendMaxColorAttachments"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendMaxColorAttachments}

instance {-# OVERLAPPING #-}
         HasField "advancedBlendIndependentBlend"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendIndependentBlend"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32
        type FieldOptional "advancedBlendIndependentBlend"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendIndependentBlend"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend}
        type FieldIsArray "advancedBlendIndependentBlend"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend}

instance {-# OVERLAPPING #-}
         CanReadField "advancedBlendIndependentBlend"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend}

instance {-# OVERLAPPING #-}
         CanWriteField "advancedBlendIndependentBlend"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendIndependentBlend}

instance {-# OVERLAPPING #-}
         HasField "advancedBlendNonPremultipliedSrcColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendNonPremultipliedSrcColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32
        type FieldOptional "advancedBlendNonPremultipliedSrcColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendNonPremultipliedSrcColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor}
        type FieldIsArray "advancedBlendNonPremultipliedSrcColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor}

instance {-# OVERLAPPING #-}
         CanReadField "advancedBlendNonPremultipliedSrcColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor}

instance {-# OVERLAPPING #-}
         CanWriteField "advancedBlendNonPremultipliedSrcColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedSrcColor}

instance {-# OVERLAPPING #-}
         HasField "advancedBlendNonPremultipliedDstColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendNonPremultipliedDstColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32
        type FieldOptional "advancedBlendNonPremultipliedDstColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendNonPremultipliedDstColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor}
        type FieldIsArray "advancedBlendNonPremultipliedDstColor"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor}

instance {-# OVERLAPPING #-}
         CanReadField "advancedBlendNonPremultipliedDstColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor}

instance {-# OVERLAPPING #-}
         CanWriteField "advancedBlendNonPremultipliedDstColor"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendNonPremultipliedDstColor}

instance {-# OVERLAPPING #-}
         HasField "advancedBlendCorrelatedOverlap"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendCorrelatedOverlap"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32
        type FieldOptional "advancedBlendCorrelatedOverlap"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendCorrelatedOverlap"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap}
        type FieldIsArray "advancedBlendCorrelatedOverlap"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap}

instance {-# OVERLAPPING #-}
         CanReadField "advancedBlendCorrelatedOverlap"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap}

instance {-# OVERLAPPING #-}
         CanWriteField "advancedBlendCorrelatedOverlap"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendCorrelatedOverlap}

instance {-# OVERLAPPING #-}
         HasField "advancedBlendAllOperations"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        type FieldType "advancedBlendAllOperations"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = VkBool32
        type FieldOptional "advancedBlendAllOperations"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "advancedBlendAllOperations"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             =
             #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations}
        type FieldIsArray "advancedBlendAllOperations"
               VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations}

instance {-# OVERLAPPING #-}
         CanReadField "advancedBlendAllOperations"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations}

instance {-# OVERLAPPING #-}
         CanWriteField "advancedBlendAllOperations"
           VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT, advancedBlendAllOperations}

instance Show VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
         where
        showsPrec d x
          = showString
              "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT {"
              .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "advancedBlendMaxColorAttachments = " .
                            showsPrec d (getField @"advancedBlendMaxColorAttachments" x) .
                              showString ", " .
                                showString "advancedBlendIndependentBlend = " .
                                  showsPrec d (getField @"advancedBlendIndependentBlend" x) .
                                    showString ", " .
                                      showString "advancedBlendNonPremultipliedSrcColor = " .
                                        showsPrec d
                                          (getField @"advancedBlendNonPremultipliedSrcColor" x)
                                          .
                                          showString ", " .
                                            showString "advancedBlendNonPremultipliedDstColor = " .
                                              showsPrec d
                                                (getField @"advancedBlendNonPremultipliedDstColor"
                                                   x)
                                                .
                                                showString ", " .
                                                  showString "advancedBlendCorrelatedOverlap = " .
                                                    showsPrec d
                                                      (getField @"advancedBlendCorrelatedOverlap" x)
                                                      .
                                                      showString ", " .
                                                        showString "advancedBlendAllOperations = " .
                                                          showsPrec d
                                                            (getField @"advancedBlendAllOperations"
                                                               x)
                                                            . showChar '}'
