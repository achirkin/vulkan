#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateCreateInfo
       (VkDescriptorUpdateTemplateCreateInfo(..)) where
import           Foreign.Storable                                             (Storable (..))
import           GHC.Base                                                     (Addr##,
                                                                               ByteArray##,
                                                                               byteArrayContents##,
                                                                               plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                               (VkDescriptorUpdateTemplateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkDescriptorUpdateTemplateType    (VkDescriptorUpdateTemplateType)
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint               (VkPipelineBindPoint)
import           Graphics.Vulkan.Types.Enum.VkStructureType                   (VkStructureType)
import           Graphics.Vulkan.Types.Handles                                (VkDescriptorSetLayout,
                                                                               VkPipelineLayout)
import           Graphics.Vulkan.Types.Struct.VkDescriptorUpdateTemplateEntry (VkDescriptorUpdateTemplateEntry)
import           System.IO.Unsafe                                             (unsafeDupablePerformIO)

-- | > typedef struct VkDescriptorUpdateTemplateCreateInfo {
--   >     VkStructureType sType;
--   >     void*                                   pNext;
--   >     VkDescriptorUpdateTemplateCreateFlags    flags;
--   >     uint32_t                 descriptorUpdateEntryCount;
--   >     const VkDescriptorUpdateTemplateEntry* pDescriptorUpdateEntries;
--   >     VkDescriptorUpdateTemplateType templateType;
--   >     VkDescriptorSetLayout descriptorSetLayout;
--   >     VkPipelineBindPoint pipelineBindPoint;
--   >     VkPipelineLayoutpipelineLayout;
--   >     uint32_t set;
--   > } VkDescriptorUpdateTemplateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDescriptorUpdateTemplateCreateInfo VkDescriptorUpdateTemplateCreateInfo registry at www.khronos.org>
data VkDescriptorUpdateTemplateCreateInfo = VkDescriptorUpdateTemplateCreateInfo## Addr##
                                                                                  ByteArray##

instance Eq VkDescriptorUpdateTemplateCreateInfo where
        (VkDescriptorUpdateTemplateCreateInfo## a _) ==
          x@(VkDescriptorUpdateTemplateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDescriptorUpdateTemplateCreateInfo where
        (VkDescriptorUpdateTemplateCreateInfo## a _) `compare`
          x@(VkDescriptorUpdateTemplateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDescriptorUpdateTemplateCreateInfo where
        sizeOf ~_
          = #{size VkDescriptorUpdateTemplateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDescriptorUpdateTemplateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDescriptorUpdateTemplateCreateInfo
         where
        unsafeAddr (VkDescriptorUpdateTemplateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDescriptorUpdateTemplateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDescriptorUpdateTemplateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDescriptorUpdateTemplateCreateInfo where
        type StructFields VkDescriptorUpdateTemplateCreateInfo =
             '["sType", "pNext", "flags", "descriptorUpdateEntryCount", -- ' closing tick for hsc2hs
               "pDescriptorUpdateEntries", "templateType", "descriptorSetLayout",
               "pipelineBindPoint", "pipelineLayout", "set"]
        type CUnionType VkDescriptorUpdateTemplateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDescriptorUpdateTemplateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDescriptorUpdateTemplateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkDescriptorUpdateTemplateCreateInfo where
        type FieldType "sType" VkDescriptorUpdateTemplateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkDescriptorUpdateTemplateCreateInfo =
             #{offset VkDescriptorUpdateTemplateCreateInfo, sType}
        type FieldIsArray "sType" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkDescriptorUpdateTemplateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkDescriptorUpdateTemplateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkDescriptorUpdateTemplateCreateInfo where
        type FieldType "pNext" VkDescriptorUpdateTemplateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkDescriptorUpdateTemplateCreateInfo =
             #{offset VkDescriptorUpdateTemplateCreateInfo, pNext}
        type FieldIsArray "pNext" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkDescriptorUpdateTemplateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkDescriptorUpdateTemplateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkDescriptorUpdateTemplateCreateInfo where
        type FieldType "flags" VkDescriptorUpdateTemplateCreateInfo =
             VkDescriptorUpdateTemplateCreateFlags
        type FieldOptional "flags" VkDescriptorUpdateTemplateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkDescriptorUpdateTemplateCreateInfo =
             #{offset VkDescriptorUpdateTemplateCreateInfo, flags}
        type FieldIsArray "flags" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkDescriptorUpdateTemplateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkDescriptorUpdateTemplateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfo
         where
        type FieldType "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfo
             = Word32
        type FieldOptional "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount}
        type FieldIsArray "descriptorUpdateEntryCount"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorUpdateEntryCount"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorUpdateEntryCount}

instance {-# OVERLAPPING #-}
         HasField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfo
         where
        type FieldType "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfo
             = Ptr VkDescriptorUpdateTemplateEntry
        type FieldOptional "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries}
        type FieldIsArray "pDescriptorUpdateEntries"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         CanReadField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         CanWriteField "pDescriptorUpdateEntries"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pDescriptorUpdateEntries}

instance {-# OVERLAPPING #-}
         HasField "templateType" VkDescriptorUpdateTemplateCreateInfo where
        type FieldType "templateType" VkDescriptorUpdateTemplateCreateInfo
             = VkDescriptorUpdateTemplateType
        type FieldOptional "templateType"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "templateType"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, templateType}
        type FieldIsArray "templateType"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, templateType}

instance {-# OVERLAPPING #-}
         CanReadField "templateType" VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, templateType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, templateType}

instance {-# OVERLAPPING #-}
         CanWriteField "templateType" VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, templateType}

instance {-# OVERLAPPING #-}
         HasField "descriptorSetLayout" VkDescriptorUpdateTemplateCreateInfo
         where
        type FieldType "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = VkDescriptorSetLayout
        type FieldOptional "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout}
        type FieldIsArray "descriptorSetLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         CanReadField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "descriptorSetLayout"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, descriptorSetLayout}

instance {-# OVERLAPPING #-}
         HasField "pipelineBindPoint" VkDescriptorUpdateTemplateCreateInfo
         where
        type FieldType "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfo
             = VkPipelineBindPoint
        type FieldOptional "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint}
        type FieldIsArray "pipelineBindPoint"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineBindPoint"
           VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         HasField "pipelineLayout" VkDescriptorUpdateTemplateCreateInfo
         where
        type FieldType "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = VkPipelineLayout
        type FieldOptional "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfo
             =
             #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout}
        type FieldIsArray "pipelineLayout"
               VkDescriptorUpdateTemplateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineLayout" VkDescriptorUpdateTemplateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineLayout" VkDescriptorUpdateTemplateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, pipelineLayout}

instance {-# OVERLAPPING #-}
         HasField "set" VkDescriptorUpdateTemplateCreateInfo where
        type FieldType "set" VkDescriptorUpdateTemplateCreateInfo = Word32
        type FieldOptional "set" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "set" VkDescriptorUpdateTemplateCreateInfo =
             #{offset VkDescriptorUpdateTemplateCreateInfo, set}
        type FieldIsArray "set" VkDescriptorUpdateTemplateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDescriptorUpdateTemplateCreateInfo, set}

instance {-# OVERLAPPING #-}
         CanReadField "set" VkDescriptorUpdateTemplateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDescriptorUpdateTemplateCreateInfo, set})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, set}

instance {-# OVERLAPPING #-}
         CanWriteField "set" VkDescriptorUpdateTemplateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDescriptorUpdateTemplateCreateInfo, set}

instance Show VkDescriptorUpdateTemplateCreateInfo where
        showsPrec d x
          = showString "VkDescriptorUpdateTemplateCreateInfo {" .
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
