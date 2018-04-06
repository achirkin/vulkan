#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceShaderCorePropertiesAMD
       (VkPhysicalDeviceShaderCorePropertiesAMD(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceShaderCorePropertiesAMD {
--   >     VkStructureType sType;
--   >     void*    pNext;
--   >     uint32_t shaderEngineCount;
--   >     uint32_t shaderArraysPerEngineCount;
--   >     uint32_t computeUnitsPerShaderArray;
--   >     uint32_t simdPerComputeUnit;
--   >     uint32_t wavefrontsPerSimd;
--   >     uint32_t wavefrontSize;
--   >     uint32_t sgprsPerSimd;
--   >     uint32_t minSgprAllocation;
--   >     uint32_t maxSgprAllocation;
--   >     uint32_t sgprAllocationGranularity;
--   >     uint32_t vgprsPerSimd;
--   >     uint32_t minVgprAllocation;
--   >     uint32_t maxVgprAllocation;
--   >     uint32_t vgprAllocationGranularity;
--   > } VkPhysicalDeviceShaderCorePropertiesAMD;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceShaderCorePropertiesAMD.html VkPhysicalDeviceShaderCorePropertiesAMD registry at www.khronos.org>
data VkPhysicalDeviceShaderCorePropertiesAMD = VkPhysicalDeviceShaderCorePropertiesAMD## Addr##
                                                                                        ByteArray##

instance Eq VkPhysicalDeviceShaderCorePropertiesAMD where
        (VkPhysicalDeviceShaderCorePropertiesAMD## a _) ==
          x@(VkPhysicalDeviceShaderCorePropertiesAMD## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceShaderCorePropertiesAMD where
        (VkPhysicalDeviceShaderCorePropertiesAMD## a _) `compare`
          x@(VkPhysicalDeviceShaderCorePropertiesAMD## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceShaderCorePropertiesAMD where
        sizeOf ~_
          = #{size VkPhysicalDeviceShaderCorePropertiesAMD}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceShaderCorePropertiesAMD}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceShaderCorePropertiesAMD
         where
        unsafeAddr (VkPhysicalDeviceShaderCorePropertiesAMD## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceShaderCorePropertiesAMD## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceShaderCorePropertiesAMD##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type StructFields VkPhysicalDeviceShaderCorePropertiesAMD =
             '["sType", "pNext", "shaderEngineCount", -- ' closing tick for hsc2hs
               "shaderArraysPerEngineCount", "computeUnitsPerShaderArray",
               "simdPerComputeUnit", "wavefrontsPerSimd", "wavefrontSize",
               "sgprsPerSimd", "minSgprAllocation", "maxSgprAllocation",
               "sgprAllocationGranularity", "vgprsPerSimd", "minVgprAllocation",
               "maxVgprAllocation", "vgprAllocationGranularity"]
        type CUnionType VkPhysicalDeviceShaderCorePropertiesAMD = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceShaderCorePropertiesAMD = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceShaderCorePropertiesAMD =
             '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceShaderCorePropertiesAMD where
        type FieldType "sType" VkPhysicalDeviceShaderCorePropertiesAMD =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceShaderCorePropertiesAMD =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sType}
        type FieldIsArray "sType" VkPhysicalDeviceShaderCorePropertiesAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceShaderCorePropertiesAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceShaderCorePropertiesAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceShaderCorePropertiesAMD where
        type FieldType "pNext" VkPhysicalDeviceShaderCorePropertiesAMD =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceShaderCorePropertiesAMD =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceShaderCorePropertiesAMD =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceShaderCorePropertiesAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceShaderCorePropertiesAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, pNext}

instance {-# OVERLAPPING #-}
         HasField "shaderEngineCount"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "shaderEngineCount"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "shaderEngineCount"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderEngineCount"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderEngineCount}
        type FieldIsArray "shaderEngineCount"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderEngineCount}

instance {-# OVERLAPPING #-}
         CanReadField "shaderEngineCount"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderEngineCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderEngineCount}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderEngineCount"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderEngineCount}

instance {-# OVERLAPPING #-}
         HasField "shaderArraysPerEngineCount"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "shaderArraysPerEngineCount"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "shaderArraysPerEngineCount"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "shaderArraysPerEngineCount"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderArraysPerEngineCount}
        type FieldIsArray "shaderArraysPerEngineCount"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderArraysPerEngineCount}

instance {-# OVERLAPPING #-}
         CanReadField "shaderArraysPerEngineCount"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderArraysPerEngineCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderArraysPerEngineCount}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderArraysPerEngineCount"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, shaderArraysPerEngineCount}

instance {-# OVERLAPPING #-}
         HasField "computeUnitsPerShaderArray"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "computeUnitsPerShaderArray"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "computeUnitsPerShaderArray"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "computeUnitsPerShaderArray"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, computeUnitsPerShaderArray}
        type FieldIsArray "computeUnitsPerShaderArray"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, computeUnitsPerShaderArray}

instance {-# OVERLAPPING #-}
         CanReadField "computeUnitsPerShaderArray"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, computeUnitsPerShaderArray})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, computeUnitsPerShaderArray}

instance {-# OVERLAPPING #-}
         CanWriteField "computeUnitsPerShaderArray"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, computeUnitsPerShaderArray}

instance {-# OVERLAPPING #-}
         HasField "simdPerComputeUnit"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "simdPerComputeUnit"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "simdPerComputeUnit"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "simdPerComputeUnit"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, simdPerComputeUnit}
        type FieldIsArray "simdPerComputeUnit"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, simdPerComputeUnit}

instance {-# OVERLAPPING #-}
         CanReadField "simdPerComputeUnit"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, simdPerComputeUnit})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, simdPerComputeUnit}

instance {-# OVERLAPPING #-}
         CanWriteField "simdPerComputeUnit"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, simdPerComputeUnit}

instance {-# OVERLAPPING #-}
         HasField "wavefrontsPerSimd"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "wavefrontsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "wavefrontsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "wavefrontsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontsPerSimd}
        type FieldIsArray "wavefrontsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontsPerSimd}

instance {-# OVERLAPPING #-}
         CanReadField "wavefrontsPerSimd"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontsPerSimd})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontsPerSimd}

instance {-# OVERLAPPING #-}
         CanWriteField "wavefrontsPerSimd"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontsPerSimd}

instance {-# OVERLAPPING #-}
         HasField "wavefrontSize" VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "wavefrontSize"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "wavefrontSize"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "wavefrontSize"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontSize}
        type FieldIsArray "wavefrontSize"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontSize}

instance {-# OVERLAPPING #-}
         CanReadField "wavefrontSize"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontSize}

instance {-# OVERLAPPING #-}
         CanWriteField "wavefrontSize"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, wavefrontSize}

instance {-# OVERLAPPING #-}
         HasField "sgprsPerSimd" VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "sgprsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "sgprsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sgprsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprsPerSimd}
        type FieldIsArray "sgprsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprsPerSimd}

instance {-# OVERLAPPING #-}
         CanReadField "sgprsPerSimd" VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprsPerSimd})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprsPerSimd}

instance {-# OVERLAPPING #-}
         CanWriteField "sgprsPerSimd"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprsPerSimd}

instance {-# OVERLAPPING #-}
         HasField "minSgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "minSgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "minSgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minSgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minSgprAllocation}
        type FieldIsArray "minSgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minSgprAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "minSgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minSgprAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minSgprAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "minSgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minSgprAllocation}

instance {-# OVERLAPPING #-}
         HasField "maxSgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "maxSgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "maxSgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxSgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxSgprAllocation}
        type FieldIsArray "maxSgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxSgprAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "maxSgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxSgprAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxSgprAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "maxSgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxSgprAllocation}

instance {-# OVERLAPPING #-}
         HasField "sgprAllocationGranularity"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "sgprAllocationGranularity"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "sgprAllocationGranularity"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "sgprAllocationGranularity"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprAllocationGranularity}
        type FieldIsArray "sgprAllocationGranularity"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprAllocationGranularity}

instance {-# OVERLAPPING #-}
         CanReadField "sgprAllocationGranularity"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprAllocationGranularity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprAllocationGranularity}

instance {-# OVERLAPPING #-}
         CanWriteField "sgprAllocationGranularity"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, sgprAllocationGranularity}

instance {-# OVERLAPPING #-}
         HasField "vgprsPerSimd" VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "vgprsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "vgprsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "vgprsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprsPerSimd}
        type FieldIsArray "vgprsPerSimd"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprsPerSimd}

instance {-# OVERLAPPING #-}
         CanReadField "vgprsPerSimd" VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprsPerSimd})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprsPerSimd}

instance {-# OVERLAPPING #-}
         CanWriteField "vgprsPerSimd"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprsPerSimd}

instance {-# OVERLAPPING #-}
         HasField "minVgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "minVgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "minVgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minVgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minVgprAllocation}
        type FieldIsArray "minVgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minVgprAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "minVgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minVgprAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minVgprAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "minVgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, minVgprAllocation}

instance {-# OVERLAPPING #-}
         HasField "maxVgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "maxVgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "maxVgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxVgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxVgprAllocation}
        type FieldIsArray "maxVgprAllocation"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxVgprAllocation}

instance {-# OVERLAPPING #-}
         CanReadField "maxVgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxVgprAllocation})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxVgprAllocation}

instance {-# OVERLAPPING #-}
         CanWriteField "maxVgprAllocation"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, maxVgprAllocation}

instance {-# OVERLAPPING #-}
         HasField "vgprAllocationGranularity"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        type FieldType "vgprAllocationGranularity"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = Word32
        type FieldOptional "vgprAllocationGranularity"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "vgprAllocationGranularity"
               VkPhysicalDeviceShaderCorePropertiesAMD
             =
             #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprAllocationGranularity}
        type FieldIsArray "vgprAllocationGranularity"
               VkPhysicalDeviceShaderCorePropertiesAMD
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprAllocationGranularity}

instance {-# OVERLAPPING #-}
         CanReadField "vgprAllocationGranularity"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprAllocationGranularity})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprAllocationGranularity}

instance {-# OVERLAPPING #-}
         CanWriteField "vgprAllocationGranularity"
           VkPhysicalDeviceShaderCorePropertiesAMD
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceShaderCorePropertiesAMD, vgprAllocationGranularity}

instance Show VkPhysicalDeviceShaderCorePropertiesAMD where
        showsPrec d x
          = showString "VkPhysicalDeviceShaderCorePropertiesAMD {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "shaderEngineCount = " .
                            showsPrec d (getField @"shaderEngineCount" x) .
                              showString ", " .
                                showString "shaderArraysPerEngineCount = " .
                                  showsPrec d (getField @"shaderArraysPerEngineCount" x) .
                                    showString ", " .
                                      showString "computeUnitsPerShaderArray = " .
                                        showsPrec d (getField @"computeUnitsPerShaderArray" x) .
                                          showString ", " .
                                            showString "simdPerComputeUnit = " .
                                              showsPrec d (getField @"simdPerComputeUnit" x) .
                                                showString ", " .
                                                  showString "wavefrontsPerSimd = " .
                                                    showsPrec d (getField @"wavefrontsPerSimd" x) .
                                                      showString ", " .
                                                        showString "wavefrontSize = " .
                                                          showsPrec d (getField @"wavefrontSize" x)
                                                            .
                                                            showString ", " .
                                                              showString "sgprsPerSimd = " .
                                                                showsPrec d
                                                                  (getField @"sgprsPerSimd" x)
                                                                  .
                                                                  showString ", " .
                                                                    showString
                                                                      "minSgprAllocation = "
                                                                      .
                                                                      showsPrec d
                                                                        (getField
                                                                           @"minSgprAllocation"
                                                                           x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "maxSgprAllocation = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"maxSgprAllocation"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "sgprAllocationGranularity = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"sgprAllocationGranularity"
                                                                                       x)
                                                                                    .
                                                                                    showString ", "
                                                                                      .
                                                                                      showString
                                                                                        "vgprsPerSimd = "
                                                                                        .
                                                                                        showsPrec d
                                                                                          (getField
                                                                                             @"vgprsPerSimd"
                                                                                             x)
                                                                                          .
                                                                                          showString
                                                                                            ", "
                                                                                            .
                                                                                            showString
                                                                                              "minVgprAllocation = "
                                                                                              .
                                                                                              showsPrec
                                                                                                d
                                                                                                (getField
                                                                                                   @"minVgprAllocation"
                                                                                                   x)
                                                                                                .
                                                                                                showString
                                                                                                  ", "
                                                                                                  .
                                                                                                  showString
                                                                                                    "maxVgprAllocation = "
                                                                                                    .
                                                                                                    showsPrec
                                                                                                      d
                                                                                                      (getField
                                                                                                         @"maxVgprAllocation"
                                                                                                         x)
                                                                                                      .
                                                                                                      showString
                                                                                                        ", "
                                                                                                        .
                                                                                                        showString
                                                                                                          "vgprAllocationGranularity = "
                                                                                                          .
                                                                                                          showsPrec
                                                                                                            d
                                                                                                            (getField
                                                                                                               @"vgprAllocationGranularity"
                                                                                                               x)
                                                                                                            .
                                                                                                            showChar
                                                                                                              '}'
