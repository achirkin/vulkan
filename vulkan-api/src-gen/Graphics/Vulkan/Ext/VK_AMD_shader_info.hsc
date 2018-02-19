#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE Strict                   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ViewPatterns             #-}
module Graphics.Vulkan.Ext.VK_AMD_shader_info
       (-- * Vulkan extension: @VK_AMD_shader_info@
        -- |
        --
        -- supported: @vulkan@
        --
        -- contact: @Jaakko Konttinen @jaakko@
        --
        -- author: @AMD@
        --
        -- type: @device@
        --
        -- Extension number: @43@
        VkShaderResourceUsageAMD(..), VkShaderStatisticsInfoAMD(..),
        vkGetShaderInfoAMD, VK_AMD_SHADER_INFO_SPEC_VERSION,
        pattern VK_AMD_SHADER_INFO_SPEC_VERSION,
        VK_AMD_SHADER_INFO_EXTENSION_NAME,
        pattern VK_AMD_SHADER_INFO_EXTENSION_NAME)
       where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
import           GHC.Ptr                          (Ptr (..))
import           GHC.TypeLits                     (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Common
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.StructMembers
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkShaderResourceUsageAMD {
--   >     uint32_t numUsedVgprs;
--   >     uint32_t numUsedSgprs;
--   >     uint32_t ldsSizePerLocalWorkGroup;
--   >     size_t ldsUsageSizeInBytes;
--   >     size_t scratchMemUsageInBytes;
--   > } VkShaderResourceUsageAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkShaderResourceUsageAMD.html VkShaderResourceUsageAMD registry at www.khronos.org>
data VkShaderResourceUsageAMD = VkShaderResourceUsageAMD## Addr##
                                                          ByteArray##

instance Eq VkShaderResourceUsageAMD where
        (VkShaderResourceUsageAMD## a _) ==
          x@(VkShaderResourceUsageAMD## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkShaderResourceUsageAMD where
        (VkShaderResourceUsageAMD## a _) `compare`
          x@(VkShaderResourceUsageAMD## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkShaderResourceUsageAMD where
        sizeOf ~_ = #{size VkShaderResourceUsageAMD}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkShaderResourceUsageAMD}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkShaderResourceUsageAMD where
        unsafeAddr (VkShaderResourceUsageAMD## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkShaderResourceUsageAMD## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkShaderResourceUsageAMD## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkShaderResourceUsageAMD where
        type StructFields VkShaderResourceUsageAMD =
             '["numUsedVgprs", "numUsedSgprs", "ldsSizePerLocalWorkGroup", -- ' closing tick for hsc2hs
               "ldsUsageSizeInBytes", "scratchMemUsageInBytes"]
        type CUnionType VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkShaderResourceUsageAMD = 'True -- ' closing tick for hsc2hs
        type StructExtends VkShaderResourceUsageAMD = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkNumUsedVgprs VkShaderResourceUsageAMD where
        type VkNumUsedVgprsMType VkShaderResourceUsageAMD = Word32

        {-# NOINLINE vkNumUsedVgprs #-}
        vkNumUsedVgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, numUsedVgprs})

        {-# INLINE vkNumUsedVgprsByteOffset #-}
        vkNumUsedVgprsByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, numUsedVgprs}

        {-# INLINE readVkNumUsedVgprs #-}
        readVkNumUsedVgprs p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, numUsedVgprs}

        {-# INLINE writeVkNumUsedVgprs #-}
        writeVkNumUsedVgprs p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, numUsedVgprs}

instance {-# OVERLAPPING #-}
         HasField "numUsedVgprs" VkShaderResourceUsageAMD where
        type FieldType "numUsedVgprs" VkShaderResourceUsageAMD = Word32
        type FieldOptional "numUsedVgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs
        type FieldOffset "numUsedVgprs" VkShaderResourceUsageAMD =
             #{offset VkShaderResourceUsageAMD, numUsedVgprs}
        type FieldIsArray "numUsedVgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, numUsedVgprs}

instance CanReadField "numUsedVgprs" VkShaderResourceUsageAMD where
        {-# INLINE getField #-}
        getField = vkNumUsedVgprs

        {-# INLINE readField #-}
        readField = readVkNumUsedVgprs

instance {-# OVERLAPPING #-}
         HasVkNumUsedSgprs VkShaderResourceUsageAMD where
        type VkNumUsedSgprsMType VkShaderResourceUsageAMD = Word32

        {-# NOINLINE vkNumUsedSgprs #-}
        vkNumUsedSgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, numUsedSgprs})

        {-# INLINE vkNumUsedSgprsByteOffset #-}
        vkNumUsedSgprsByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, numUsedSgprs}

        {-# INLINE readVkNumUsedSgprs #-}
        readVkNumUsedSgprs p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, numUsedSgprs}

        {-# INLINE writeVkNumUsedSgprs #-}
        writeVkNumUsedSgprs p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, numUsedSgprs}

instance {-# OVERLAPPING #-}
         HasField "numUsedSgprs" VkShaderResourceUsageAMD where
        type FieldType "numUsedSgprs" VkShaderResourceUsageAMD = Word32
        type FieldOptional "numUsedSgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs
        type FieldOffset "numUsedSgprs" VkShaderResourceUsageAMD =
             #{offset VkShaderResourceUsageAMD, numUsedSgprs}
        type FieldIsArray "numUsedSgprs" VkShaderResourceUsageAMD = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, numUsedSgprs}

instance CanReadField "numUsedSgprs" VkShaderResourceUsageAMD where
        {-# INLINE getField #-}
        getField = vkNumUsedSgprs

        {-# INLINE readField #-}
        readField = readVkNumUsedSgprs

instance {-# OVERLAPPING #-}
         HasVkLdsSizePerLocalWorkGroup VkShaderResourceUsageAMD where
        type VkLdsSizePerLocalWorkGroupMType VkShaderResourceUsageAMD =
             Word32

        {-# NOINLINE vkLdsSizePerLocalWorkGroup #-}
        vkLdsSizePerLocalWorkGroup x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup})

        {-# INLINE vkLdsSizePerLocalWorkGroupByteOffset #-}
        vkLdsSizePerLocalWorkGroupByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

        {-# INLINE readVkLdsSizePerLocalWorkGroup #-}
        readVkLdsSizePerLocalWorkGroup p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

        {-# INLINE writeVkLdsSizePerLocalWorkGroup #-}
        writeVkLdsSizePerLocalWorkGroup p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

instance {-# OVERLAPPING #-}
         HasField "ldsSizePerLocalWorkGroup" VkShaderResourceUsageAMD where
        type FieldType "ldsSizePerLocalWorkGroup" VkShaderResourceUsageAMD
             = Word32
        type FieldOptional "ldsSizePerLocalWorkGroup"
               VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "ldsSizePerLocalWorkGroup"
               VkShaderResourceUsageAMD
             =
             #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}
        type FieldIsArray "ldsSizePerLocalWorkGroup"
               VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, ldsSizePerLocalWorkGroup}

instance CanReadField "ldsSizePerLocalWorkGroup"
           VkShaderResourceUsageAMD
         where
        {-# INLINE getField #-}
        getField = vkLdsSizePerLocalWorkGroup

        {-# INLINE readField #-}
        readField = readVkLdsSizePerLocalWorkGroup

instance {-# OVERLAPPING #-}
         HasVkLdsUsageSizeInBytes VkShaderResourceUsageAMD where
        type VkLdsUsageSizeInBytesMType VkShaderResourceUsageAMD = CSize

        {-# NOINLINE vkLdsUsageSizeInBytes #-}
        vkLdsUsageSizeInBytes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes})

        {-# INLINE vkLdsUsageSizeInBytesByteOffset #-}
        vkLdsUsageSizeInBytesByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

        {-# INLINE readVkLdsUsageSizeInBytes #-}
        readVkLdsUsageSizeInBytes p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

        {-# INLINE writeVkLdsUsageSizeInBytes #-}
        writeVkLdsUsageSizeInBytes p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

instance {-# OVERLAPPING #-}
         HasField "ldsUsageSizeInBytes" VkShaderResourceUsageAMD where
        type FieldType "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             CSize
        type FieldOptional "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}
        type FieldIsArray "ldsUsageSizeInBytes" VkShaderResourceUsageAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, ldsUsageSizeInBytes}

instance CanReadField "ldsUsageSizeInBytes"
           VkShaderResourceUsageAMD
         where
        {-# INLINE getField #-}
        getField = vkLdsUsageSizeInBytes

        {-# INLINE readField #-}
        readField = readVkLdsUsageSizeInBytes

instance {-# OVERLAPPING #-}
         HasVkScratchMemUsageInBytes VkShaderResourceUsageAMD where
        type VkScratchMemUsageInBytesMType VkShaderResourceUsageAMD = CSize

        {-# NOINLINE vkScratchMemUsageInBytes #-}
        vkScratchMemUsageInBytes x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes})

        {-# INLINE vkScratchMemUsageInBytesByteOffset #-}
        vkScratchMemUsageInBytesByteOffset ~_
          = #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

        {-# INLINE readVkScratchMemUsageInBytes #-}
        readVkScratchMemUsageInBytes p
          = peekByteOff p #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

        {-# INLINE writeVkScratchMemUsageInBytes #-}
        writeVkScratchMemUsageInBytes p
          = pokeByteOff p #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

instance {-# OVERLAPPING #-}
         HasField "scratchMemUsageInBytes" VkShaderResourceUsageAMD where
        type FieldType "scratchMemUsageInBytes" VkShaderResourceUsageAMD =
             CSize
        type FieldOptional "scratchMemUsageInBytes"
               VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "scratchMemUsageInBytes" VkShaderResourceUsageAMD
             =
             #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}
        type FieldIsArray "scratchMemUsageInBytes" VkShaderResourceUsageAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderResourceUsageAMD, scratchMemUsageInBytes}

instance CanReadField "scratchMemUsageInBytes"
           VkShaderResourceUsageAMD
         where
        {-# INLINE getField #-}
        getField = vkScratchMemUsageInBytes

        {-# INLINE readField #-}
        readField = readVkScratchMemUsageInBytes

instance Show VkShaderResourceUsageAMD where
        showsPrec d x
          = showString "VkShaderResourceUsageAMD {" .
              showString "vkNumUsedVgprs = " .
                showsPrec d (vkNumUsedVgprs x) .
                  showString ", " .
                    showString "vkNumUsedSgprs = " .
                      showsPrec d (vkNumUsedSgprs x) .
                        showString ", " .
                          showString "vkLdsSizePerLocalWorkGroup = " .
                            showsPrec d (vkLdsSizePerLocalWorkGroup x) .
                              showString ", " .
                                showString "vkLdsUsageSizeInBytes = " .
                                  showsPrec d (vkLdsUsageSizeInBytes x) .
                                    showString ", " .
                                      showString "vkScratchMemUsageInBytes = " .
                                        showsPrec d (vkScratchMemUsageInBytes x) . showChar '}'

-- | > typedef struct VkShaderStatisticsInfoAMD {
--   >     VkShaderStageFlags shaderStageMask;
--   >     VkShaderResourceUsageAMD resourceUsage;
--   >     uint32_t numPhysicalVgprs;
--   >     uint32_t numPhysicalSgprs;
--   >     uint32_t numAvailableVgprs;
--   >     uint32_t numAvailableSgprs;
--   >     uint32_t computeWorkGroupSize[3];
--   > } VkShaderStatisticsInfoAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkShaderStatisticsInfoAMD.html VkShaderStatisticsInfoAMD registry at www.khronos.org>
data VkShaderStatisticsInfoAMD = VkShaderStatisticsInfoAMD## Addr##
                                                            ByteArray##

instance Eq VkShaderStatisticsInfoAMD where
        (VkShaderStatisticsInfoAMD## a _) ==
          x@(VkShaderStatisticsInfoAMD## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkShaderStatisticsInfoAMD where
        (VkShaderStatisticsInfoAMD## a _) `compare`
          x@(VkShaderStatisticsInfoAMD## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkShaderStatisticsInfoAMD where
        sizeOf ~_ = #{size VkShaderStatisticsInfoAMD}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkShaderStatisticsInfoAMD}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkShaderStatisticsInfoAMD where
        unsafeAddr (VkShaderStatisticsInfoAMD## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkShaderStatisticsInfoAMD## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkShaderStatisticsInfoAMD## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkShaderStatisticsInfoAMD where
        type StructFields VkShaderStatisticsInfoAMD =
             '["shaderStageMask", "resourceUsage", "numPhysicalVgprs", -- ' closing tick for hsc2hs
               "numPhysicalSgprs", "numAvailableVgprs", "numAvailableSgprs",
               "computeWorkGroupSize"]
        type CUnionType VkShaderStatisticsInfoAMD = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkShaderStatisticsInfoAMD = 'True -- ' closing tick for hsc2hs
        type StructExtends VkShaderStatisticsInfoAMD = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkShaderStageMask VkShaderStatisticsInfoAMD where
        type VkShaderStageMaskMType VkShaderStatisticsInfoAMD =
             VkShaderStageFlags

        {-# NOINLINE vkShaderStageMask #-}
        vkShaderStageMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, shaderStageMask})

        {-# INLINE vkShaderStageMaskByteOffset #-}
        vkShaderStageMaskByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

        {-# INLINE readVkShaderStageMask #-}
        readVkShaderStageMask p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

        {-# INLINE writeVkShaderStageMask #-}
        writeVkShaderStageMask p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

instance {-# OVERLAPPING #-}
         HasField "shaderStageMask" VkShaderStatisticsInfoAMD where
        type FieldType "shaderStageMask" VkShaderStatisticsInfoAMD =
             VkShaderStageFlags
        type FieldOptional "shaderStageMask" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderStageMask" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, shaderStageMask}
        type FieldIsArray "shaderStageMask" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

instance CanReadField "shaderStageMask" VkShaderStatisticsInfoAMD
         where
        {-# INLINE getField #-}
        getField = vkShaderStageMask

        {-# INLINE readField #-}
        readField = readVkShaderStageMask

instance {-# OVERLAPPING #-}
         HasVkResourceUsage VkShaderStatisticsInfoAMD where
        type VkResourceUsageMType VkShaderStatisticsInfoAMD =
             VkShaderResourceUsageAMD

        {-# NOINLINE vkResourceUsage #-}
        vkResourceUsage x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, resourceUsage})

        {-# INLINE vkResourceUsageByteOffset #-}
        vkResourceUsageByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, resourceUsage}

        {-# INLINE readVkResourceUsage #-}
        readVkResourceUsage p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, resourceUsage}

        {-# INLINE writeVkResourceUsage #-}
        writeVkResourceUsage p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, resourceUsage}

instance {-# OVERLAPPING #-}
         HasField "resourceUsage" VkShaderStatisticsInfoAMD where
        type FieldType "resourceUsage" VkShaderStatisticsInfoAMD =
             VkShaderResourceUsageAMD
        type FieldOptional "resourceUsage" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "resourceUsage" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, resourceUsage}
        type FieldIsArray "resourceUsage" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, resourceUsage}

instance CanReadField "resourceUsage" VkShaderStatisticsInfoAMD
         where
        {-# INLINE getField #-}
        getField = vkResourceUsage

        {-# INLINE readField #-}
        readField = readVkResourceUsage

instance {-# OVERLAPPING #-}
         HasVkNumPhysicalVgprs VkShaderStatisticsInfoAMD where
        type VkNumPhysicalVgprsMType VkShaderStatisticsInfoAMD = Word32

        {-# NOINLINE vkNumPhysicalVgprs #-}
        vkNumPhysicalVgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs})

        {-# INLINE vkNumPhysicalVgprsByteOffset #-}
        vkNumPhysicalVgprsByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

        {-# INLINE readVkNumPhysicalVgprs #-}
        readVkNumPhysicalVgprs p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

        {-# INLINE writeVkNumPhysicalVgprs #-}
        writeVkNumPhysicalVgprs p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

instance {-# OVERLAPPING #-}
         HasField "numPhysicalVgprs" VkShaderStatisticsInfoAMD where
        type FieldType "numPhysicalVgprs" VkShaderStatisticsInfoAMD =
             Word32
        type FieldOptional "numPhysicalVgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "numPhysicalVgprs" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}
        type FieldIsArray "numPhysicalVgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

instance CanReadField "numPhysicalVgprs" VkShaderStatisticsInfoAMD
         where
        {-# INLINE getField #-}
        getField = vkNumPhysicalVgprs

        {-# INLINE readField #-}
        readField = readVkNumPhysicalVgprs

instance {-# OVERLAPPING #-}
         HasVkNumPhysicalSgprs VkShaderStatisticsInfoAMD where
        type VkNumPhysicalSgprsMType VkShaderStatisticsInfoAMD = Word32

        {-# NOINLINE vkNumPhysicalSgprs #-}
        vkNumPhysicalSgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs})

        {-# INLINE vkNumPhysicalSgprsByteOffset #-}
        vkNumPhysicalSgprsByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

        {-# INLINE readVkNumPhysicalSgprs #-}
        readVkNumPhysicalSgprs p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

        {-# INLINE writeVkNumPhysicalSgprs #-}
        writeVkNumPhysicalSgprs p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

instance {-# OVERLAPPING #-}
         HasField "numPhysicalSgprs" VkShaderStatisticsInfoAMD where
        type FieldType "numPhysicalSgprs" VkShaderStatisticsInfoAMD =
             Word32
        type FieldOptional "numPhysicalSgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "numPhysicalSgprs" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}
        type FieldIsArray "numPhysicalSgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

instance CanReadField "numPhysicalSgprs" VkShaderStatisticsInfoAMD
         where
        {-# INLINE getField #-}
        getField = vkNumPhysicalSgprs

        {-# INLINE readField #-}
        readField = readVkNumPhysicalSgprs

instance {-# OVERLAPPING #-}
         HasVkNumAvailableVgprs VkShaderStatisticsInfoAMD where
        type VkNumAvailableVgprsMType VkShaderStatisticsInfoAMD = Word32

        {-# NOINLINE vkNumAvailableVgprs #-}
        vkNumAvailableVgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs})

        {-# INLINE vkNumAvailableVgprsByteOffset #-}
        vkNumAvailableVgprsByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

        {-# INLINE readVkNumAvailableVgprs #-}
        readVkNumAvailableVgprs p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

        {-# INLINE writeVkNumAvailableVgprs #-}
        writeVkNumAvailableVgprs p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

instance {-# OVERLAPPING #-}
         HasField "numAvailableVgprs" VkShaderStatisticsInfoAMD where
        type FieldType "numAvailableVgprs" VkShaderStatisticsInfoAMD =
             Word32
        type FieldOptional "numAvailableVgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "numAvailableVgprs" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}
        type FieldIsArray "numAvailableVgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

instance CanReadField "numAvailableVgprs" VkShaderStatisticsInfoAMD
         where
        {-# INLINE getField #-}
        getField = vkNumAvailableVgprs

        {-# INLINE readField #-}
        readField = readVkNumAvailableVgprs

instance {-# OVERLAPPING #-}
         HasVkNumAvailableSgprs VkShaderStatisticsInfoAMD where
        type VkNumAvailableSgprsMType VkShaderStatisticsInfoAMD = Word32

        {-# NOINLINE vkNumAvailableSgprs #-}
        vkNumAvailableSgprs x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs})

        {-# INLINE vkNumAvailableSgprsByteOffset #-}
        vkNumAvailableSgprsByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

        {-# INLINE readVkNumAvailableSgprs #-}
        readVkNumAvailableSgprs p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

        {-# INLINE writeVkNumAvailableSgprs #-}
        writeVkNumAvailableSgprs p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

instance {-# OVERLAPPING #-}
         HasField "numAvailableSgprs" VkShaderStatisticsInfoAMD where
        type FieldType "numAvailableSgprs" VkShaderStatisticsInfoAMD =
             Word32
        type FieldOptional "numAvailableSgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "numAvailableSgprs" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}
        type FieldIsArray "numAvailableSgprs" VkShaderStatisticsInfoAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

instance CanReadField "numAvailableSgprs" VkShaderStatisticsInfoAMD
         where
        {-# INLINE getField #-}
        getField = vkNumAvailableSgprs

        {-# INLINE readField #-}
        readField = readVkNumAvailableSgprs

instance {-# OVERLAPPING #-}
         HasVkComputeWorkGroupSizeArray VkShaderStatisticsInfoAMD where
        type VkComputeWorkGroupSizeArrayMType VkShaderStatisticsInfoAMD =
             Word32

        {-# NOINLINE vkComputeWorkGroupSizeArray #-}
        vkComputeWorkGroupSizeArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: Word32) +
                    #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}))

        {-# INLINE vkComputeWorkGroupSizeArrayByteOffset #-}
        vkComputeWorkGroupSizeArrayByteOffset ~_
          = #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}

        {-# INLINE readVkComputeWorkGroupSizeArray #-}
        readVkComputeWorkGroupSizeArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize})

        {-# INLINE writeVkComputeWorkGroupSizeArray #-}
        writeVkComputeWorkGroupSizeArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: Word32) +
                 #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize})

instance {-# OVERLAPPING #-}
         HasField "computeWorkGroupSize" VkShaderStatisticsInfoAMD where
        type FieldType "computeWorkGroupSize" VkShaderStatisticsInfoAMD =
             Word32
        type FieldOptional "computeWorkGroupSize" VkShaderStatisticsInfoAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "computeWorkGroupSize" VkShaderStatisticsInfoAMD =
             #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}
        type FieldIsArray "computeWorkGroupSize" VkShaderStatisticsInfoAMD
             = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}

instance (KnownNat idx,
          IndexInBounds "computeWorkGroupSize" idx
            VkShaderStatisticsInfoAMD) =>
         CanReadFieldArray "computeWorkGroupSize" idx
           VkShaderStatisticsInfoAMD
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "computeWorkGroupSize" 0
                         VkShaderStatisticsInfoAMD
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "computeWorkGroupSize" 1
                         VkShaderStatisticsInfoAMD
                       #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "computeWorkGroupSize" 2
                         VkShaderStatisticsInfoAMD
                       #-}
        type FieldArrayLength "computeWorkGroupSize"
               VkShaderStatisticsInfoAMD
             = 3

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = 3

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkComputeWorkGroupSizeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkComputeWorkGroupSizeArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance Show VkShaderStatisticsInfoAMD where
        showsPrec d x
          = showString "VkShaderStatisticsInfoAMD {" .
              showString "vkShaderStageMask = " .
                showsPrec d (vkShaderStageMask x) .
                  showString ", " .
                    showString "vkResourceUsage = " .
                      showsPrec d (vkResourceUsage x) .
                        showString ", " .
                          showString "vkNumPhysicalVgprs = " .
                            showsPrec d (vkNumPhysicalVgprs x) .
                              showString ", " .
                                showString "vkNumPhysicalSgprs = " .
                                  showsPrec d (vkNumPhysicalSgprs x) .
                                    showString ", " .
                                      showString "vkNumAvailableVgprs = " .
                                        showsPrec d (vkNumAvailableVgprs x) .
                                          showString ", " .
                                            showString "vkNumAvailableSgprs = " .
                                              showsPrec d (vkNumAvailableSgprs x) .
                                                showString ", " .
                                                  showString "vkComputeWorkGroupSizeArray = [" .
                                                    showsPrec d
                                                      (map (vkComputeWorkGroupSizeArray x) [1 .. 3])
                                                      . showChar ']' . showChar '}'

-- | Success codes: 'VK_SUCCESS', 'VK_INCOMPLETE'.
--
--   Error codes: 'VK_ERROR_FEATURE_NOT_PRESENT', 'VK_ERROR_OUT_OF_HOST_MEMORY'.
--
--   > VkResult vkGetShaderInfoAMD
--   >     ( VkDevice device
--   >     , VkPipeline pipeline
--   >     , VkShaderStageFlagBits shaderStage
--   >     , VkShaderInfoTypeAMD infoType
--   >     , size_t* pInfoSize
--   >     , void* pInfo
--   >     )
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/vkGetShaderInfoAMD.html vkGetShaderInfoAMD registry at www.khronos.org>
foreign import ccall unsafe "vkGetShaderInfoAMD" vkGetShaderInfoAMD
               ::
               VkDevice -- ^ device
                        ->
                 VkPipeline -- ^ pipeline
                            ->
                   VkShaderStageFlagBits -- ^ shaderStage
                                         ->
                     VkShaderInfoTypeAMD -- ^ infoType
                                         -> Ptr CSize -- ^ pInfoSize
                                                      -> Ptr Void -- ^ pInfo
                                                                  -> IO VkResult

pattern VK_AMD_SHADER_INFO_SPEC_VERSION :: (Num a, Eq a) => a

pattern VK_AMD_SHADER_INFO_SPEC_VERSION = 1

type VK_AMD_SHADER_INFO_SPEC_VERSION = 1

pattern VK_AMD_SHADER_INFO_EXTENSION_NAME :: CString

pattern VK_AMD_SHADER_INFO_EXTENSION_NAME <-
        (is_VK_AMD_SHADER_INFO_EXTENSION_NAME -> True)
  where VK_AMD_SHADER_INFO_EXTENSION_NAME
          = _VK_AMD_SHADER_INFO_EXTENSION_NAME

{-# INLINE _VK_AMD_SHADER_INFO_EXTENSION_NAME #-}

_VK_AMD_SHADER_INFO_EXTENSION_NAME :: CString
_VK_AMD_SHADER_INFO_EXTENSION_NAME = Ptr "VK_AMD_shader_info\NUL"##

{-# INLINE is_VK_AMD_SHADER_INFO_EXTENSION_NAME #-}

is_VK_AMD_SHADER_INFO_EXTENSION_NAME :: CString -> Bool
is_VK_AMD_SHADER_INFO_EXTENSION_NAME
  = eqCStrings _VK_AMD_SHADER_INFO_EXTENSION_NAME

type VK_AMD_SHADER_INFO_EXTENSION_NAME = "VK_AMD_shader_info"
