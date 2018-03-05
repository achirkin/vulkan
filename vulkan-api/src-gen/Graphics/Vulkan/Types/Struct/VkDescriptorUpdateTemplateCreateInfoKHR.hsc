#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateCreateInfoKHR
       (VkDescriptorUpdateTemplateCreateInfoKHR(..)) where
import           Foreign.Storable                                                (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                                  (VkDescriptorUpdateTemplateCreateFlagsKHR)
import           Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateTypeKHR    (VkDescriptorUpdateTemplateTypeKHR)
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint                  (VkPipelineBindPoint)
import           Graphics.Vulkan.Types.Enum.VkStructureType                      (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                   (VkDescriptorSetLayout,
                                                                                  VkPipelineLayout)
import           Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntryKHR (VkDescriptorUpdateTemplateEntryKHR)
import           System.IO.Unsafe                                                (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorUpdateTemplateCreateInfoKHR {
--   >     VkStructureType sType;
--   >     void*                                   pNext;
--   >     VkDescriptorUpdateTemplateCreateFlagsKHR    flags;
--   >     uint32_t                 descriptorUpdateEntryCount;
--   >     const VkDescriptorUpdateTemplateEntryKHR* pDescriptorUpdateEntries;
--   >     VkDescriptorUpdateTemplateTypeKHR templateType;
--   >     VkDescriptorSetLayout descriptorSetLayout;
--   >     VkPipelineBindPoint pipelineBindPoint;
--   >     VkPipelineLayoutpipelineLayout;
--   >     uint32_t set;
--   > } VkDescriptorUpdateTemplateCreateInfoKHR;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDescriptorUpdateTemplateCreateInfoKHR.html VkDescriptorUpdateTemplateCreateInfoKHR registry at www.khronos.org>
data VkDescriptorUpdateTemplateCreateInfoKHR = VkDescriptorUpdateTemplateCreateInfoKHR## Addr##
                                                                                        ByteArray##

instance Eq VkDescriptorUpdateTemplateCreateInfoKHR where
        (VkDescriptorUpdateTemplateCreateInfoKHR## a _) ==
          x@(VkDescriptorUpdateTemplateCreateInfoKHR## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorUpdateTemplateCreateInfoKHR where
        (VkDescriptorUpdateTemplateCreateInfoKHR## a _) `compare`
          x@(VkDescriptorUpdateTemplateCreateInfoKHR## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorUpdateTemplateCreateInfoKHR where
        sizeOf ~_
          = #{size VkDescriptorUpdateTemplateCreateInfoKHR}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorUpdateTemplateCreateInfoKHR}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorUpdateTemplateCreateInfoKHR
         where
        unsafeAddr (VkDescriptorUpdateTemplateCreateInfoKHR## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorUpdateTemplateCreateInfoKHR## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorUpdateTemplateCreateInfoKHR##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type StructFields VkDescriptorUpdateTemplateCreateInfoKHR =
             '["sType", "pNext", "flags", "descriptorUpdateEntryCount", -- ' closing tick for hsc2hs
               "pDescriptorUpdateEntries", "templateType", "descriptorSetLayout",
               "pipelineBindPoint", "pipelineLayout", "set"]
        type CUnionType VkDescriptorUpdateTemplateCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorUpdateTemplateCreateInfoKHR = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorUpdateTemplateCreateInfoKHR = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "sType" VkDescriptorUpdateTemplateCreateInfoKHR =
             VkStructureType
        type FieldOptional "sType" VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}
        type FieldIsArray "sType" VkDescriptorUpdateTemplateCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorUpdateTemplateCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorUpdateTemplateCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "pNext" VkDescriptorUpdateTemplateCreateInfoKHR =
             Ptr Void
        type FieldOptional "pNext" VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}
        type FieldIsArray "pNext" VkDescriptorUpdateTemplateCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorUpdateTemplateCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorUpdateTemplateCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "flags" VkDescriptorUpdateTemplateCreateInfoKHR =
             VkDescriptorUpdateTemplateCreateFlagsKHR
        type FieldOptional "flags" VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}
        type FieldIsArray "flags" VkDescriptorUpdateTemplateCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDescriptorUpdateTemplateCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDescriptorUpdateTemplateCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, flags}

instance {-# OVERLAPPING #-}
         HasField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = Word32
        type FieldOptional "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}
        type FieldIsArray "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         HasField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = Ptr VkDescriptorUpdateTemplateEntryKHR
        type FieldOptional "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}
        type FieldIsArray "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         CanReadField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         CanWriteField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         HasField "templateType" VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkDescriptorUpdateTemplateTypeKHR
        type FieldOptional "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}
        type FieldIsArray "templateType"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

instance {-# OVERLAPPING #-}
         CanReadField "templateType" VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

instance {-# OVERLAPPING #-}
         CanWriteField "templateType"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, templateType}

instance {-# OVERLAPPING #-}
         HasField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkDescriptorSetLayout
        type FieldOptional "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}
        type FieldIsArray "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         HasField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkPipelineBindPoint
        type FieldOptional "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}
        type FieldIsArray "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         HasField "pipelineLayout" VkDescriptorUpdateTemplateCreateInfoKHR
         where
        type FieldType "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = VkPipelineLayout
        type FieldOptional "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}
        type FieldIsArray "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfoKHR
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineLayout"
           VkDescriptorUpdateTemplateCreateInfoKHR
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, pipelineLayout}

instance {-# OVERLAPPING #-}
         HasField "set" VkDescriptorUpdateTemplateCreateInfoKHR where
        type FieldType "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             Word32
        type FieldOptional "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}
        type FieldIsArray "set" VkDescriptorUpdateTemplateCreateInfoKHR =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

instance {-# OVERLAPPING #-}
         CanReadField "set" VkDescriptorUpdateTemplateCreateInfoKHR where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

instance {-# OVERLAPPING #-}
         CanWriteField "set" VkDescriptorUpdateTemplateCreateInfoKHR where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfoKHR, set}

instance Show VkDescriptorUpdateTemplateCreateInfoKHR where
        showsPrec d x
          = showString "VkDescriptorUpdateTemplateCreateInfoKHR {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "descriptorUpdateEntryCount = " .
                                  showsPrec d (getField @"descriptorUpdateEntryCount" x) .
                                    showString ", " .
                                      showString "pDescriptorUpdateEntries = " .
                                        showsPrec d (getField @"pDescriptorUpdateEntries" x) .
                                          showString ", " .
                                            showString "templateType = " .
                                              showsPrec d (getField @"templateType" x) .
                                                showString ", " .
                                                  showString "descriptorSetLayout = " .
                                                    showsPrec d (getField @"descriptorSetLayout" x)
                                                      .
                                                      showString ", " .
                                                        showString "pipelineBindPoint = " .
                                                          showsPrec d
                                                            (getField @"pipelineBindPoint" x)
                                                            .
                                                            showString ", " .
                                                              showString "pipelineLayout = " .
                                                                showsPrec d
                                                                  (getField @"pipelineLayout" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString "set = " .
                                                                      showsPrec d
                                                                        (getField @"set" x)
                                                                        . showChar '}'
