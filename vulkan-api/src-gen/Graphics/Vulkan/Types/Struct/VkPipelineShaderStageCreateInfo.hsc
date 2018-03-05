#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineShaderStageCreateInfo
       (VkPipelineShaderStageCreateInfo(..)) where
import           Foreign.Storable                                  (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Bitmasks                    (VkPipelineShaderStageCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags     (VkShaderStageFlagBits)
import           Graphics.Vulkan.Types.Enum.VkStructureType        (VkStructureType)
import           Graphics.Vulkan.Types.Handles                     (VkShaderModule)
import           Graphics.Vulkan.Types.Struct.VkSpecializationInfo (VkSpecializationInfo)
import           System.IO.Unsafe                                  (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineShaderStageCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineShaderStageCreateFlags    flags;
--   >     VkShaderStageFlagBits  stage;
--   >     VkShaderModule         module;
--   >     const char*            pName;
--   >     const VkSpecializationInfo* pSpecializationInfo;
--   > } VkPipelineShaderStageCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineShaderStageCreateInfo.html VkPipelineShaderStageCreateInfo registry at www.khronos.org>
data VkPipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo## Addr##
                                                                        ByteArray##

instance Eq VkPipelineShaderStageCreateInfo where
        (VkPipelineShaderStageCreateInfo## a _) ==
          x@(VkPipelineShaderStageCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineShaderStageCreateInfo where
        (VkPipelineShaderStageCreateInfo## a _) `compare`
          x@(VkPipelineShaderStageCreateInfo## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineShaderStageCreateInfo where
        sizeOf ~_ = #{size VkPipelineShaderStageCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineShaderStageCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineShaderStageCreateInfo where
        unsafeAddr (VkPipelineShaderStageCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineShaderStageCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineShaderStageCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineShaderStageCreateInfo where
        type StructFields VkPipelineShaderStageCreateInfo =
             '["sType", "pNext", "flags", "stage", "module", "pName", -- ' closing tick for hsc2hs
               "pSpecializationInfo"]
        type CUnionType VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineShaderStageCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineShaderStageCreateInfo where
        type FieldType "sType" VkPipelineShaderStageCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineShaderStageCreateInfo where
        type FieldType "pNext" VkPipelineShaderStageCreateInfo = Ptr Void
        type FieldOptional "pNext" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineShaderStageCreateInfo where
        type FieldType "flags" VkPipelineShaderStageCreateInfo =
             VkPipelineShaderStageCreateFlags
        type FieldOptional "flags" VkPipelineShaderStageCreateInfo = 'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "stage" VkPipelineShaderStageCreateInfo where
        type FieldType "stage" VkPipelineShaderStageCreateInfo =
             VkShaderStageFlagBits
        type FieldOptional "stage" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stage" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, stage}
        type FieldIsArray "stage" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, stage}

instance {-# OVERLAPPING #-}
         CanReadField "stage" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, stage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, stage}

instance {-# OVERLAPPING #-}
         CanWriteField "stage" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, stage}

instance {-# OVERLAPPING #-}
         HasField "module" VkPipelineShaderStageCreateInfo where
        type FieldType "module" VkPipelineShaderStageCreateInfo =
             VkShaderModule
        type FieldOptional "module" VkPipelineShaderStageCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "module" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, module}
        type FieldIsArray "module" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, module}

instance {-# OVERLAPPING #-}
         CanReadField "module" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, module})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, module}

instance {-# OVERLAPPING #-}
         CanWriteField "module" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, module}

instance {-# OVERLAPPING #-}
         HasField "pName" VkPipelineShaderStageCreateInfo where
        type FieldType "pName" VkPipelineShaderStageCreateInfo = CString
        type FieldOptional "pName" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pName" VkPipelineShaderStageCreateInfo =
             #{offset VkPipelineShaderStageCreateInfo, pName}
        type FieldIsArray "pName" VkPipelineShaderStageCreateInfo = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, pName}

instance {-# OVERLAPPING #-}
         CanReadField "pName" VkPipelineShaderStageCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, pName})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, pName}

instance {-# OVERLAPPING #-}
         CanWriteField "pName" VkPipelineShaderStageCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, pName}

instance {-# OVERLAPPING #-}
         HasField "pSpecializationInfo" VkPipelineShaderStageCreateInfo
         where
        type FieldType "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             = Ptr VkSpecializationInfo
        type FieldOptional "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             =
             #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}
        type FieldIsArray "pSpecializationInfo"
               VkPipelineShaderStageCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}

instance {-# OVERLAPPING #-}
         CanReadField "pSpecializationInfo" VkPipelineShaderStageCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}

instance {-# OVERLAPPING #-}
         CanWriteField "pSpecializationInfo" VkPipelineShaderStageCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineShaderStageCreateInfo, pSpecializationInfo}

instance Show VkPipelineShaderStageCreateInfo where
        showsPrec d x
          = showString "VkPipelineShaderStageCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "stage = " .
                                  showsPrec d (getField @"stage" x) .
                                    showString ", " .
                                      showString "module = " .
                                        showsPrec d (getField @"module" x) .
                                          showString ", " .
                                            showString "pName = " .
                                              showsPrec d (getField @"pName" x) .
                                                showString ", " .
                                                  showString "pSpecializationInfo = " .
                                                    showsPrec d (getField @"pSpecializationInfo" x)
                                                      . showChar '}'
