#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Graphics.Vulkan.Types.Struct.VkShaderStatisticsInfoAMD
       (VkShaderStatisticsInfoAMD(..)) where
import           Foreign.Storable                                      (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                                          (KnownNat,
                                                                        natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags         (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Struct.VkShaderResourceUsageAMD (VkShaderResourceUsageAMD)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                      (unsafeDupablePerformIO)

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
