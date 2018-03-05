#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
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

instance {-# OVERLAPPING #-}
         CanReadField "shaderStageMask" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, shaderStageMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

instance {-# OVERLAPPING #-}
         CanWriteField "shaderStageMask" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, shaderStageMask}

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

instance {-# OVERLAPPING #-}
         CanReadField "resourceUsage" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, resourceUsage})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, resourceUsage}

instance {-# OVERLAPPING #-}
         CanWriteField "resourceUsage" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, resourceUsage}

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

instance {-# OVERLAPPING #-}
         CanReadField "numPhysicalVgprs" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numPhysicalVgprs" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalVgprs}

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

instance {-# OVERLAPPING #-}
         CanReadField "numPhysicalSgprs" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numPhysicalSgprs" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numPhysicalSgprs}

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

instance {-# OVERLAPPING #-}
         CanReadField "numAvailableVgprs" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numAvailableVgprs" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableVgprs}

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

instance {-# OVERLAPPING #-}
         CanReadField "numAvailableSgprs" VkShaderStatisticsInfoAMD where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

instance {-# OVERLAPPING #-}
         CanWriteField "numAvailableSgprs" VkShaderStatisticsInfoAMD where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkShaderStatisticsInfoAMD, numAvailableSgprs}

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}
                      +
                      sizeOf (undefined :: Word32) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "computeWorkGroupSize" idx
            VkShaderStatisticsInfoAMD) =>
         CanWriteFieldArray "computeWorkGroupSize" idx
           VkShaderStatisticsInfoAMD
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "computeWorkGroupSize" 0
                         VkShaderStatisticsInfoAMD
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "computeWorkGroupSize" 1
                         VkShaderStatisticsInfoAMD
                       #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "computeWorkGroupSize" 2
                         VkShaderStatisticsInfoAMD
                       #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkShaderStatisticsInfoAMD, computeWorkGroupSize}
                 +
                 sizeOf (undefined :: Word32) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance Show VkShaderStatisticsInfoAMD where
        showsPrec d x
          = showString "VkShaderStatisticsInfoAMD {" .
              showString "shaderStageMask = " .
                showsPrec d (getField @"shaderStageMask" x) .
                  showString ", " .
                    showString "resourceUsage = " .
                      showsPrec d (getField @"resourceUsage" x) .
                        showString ", " .
                          showString "numPhysicalVgprs = " .
                            showsPrec d (getField @"numPhysicalVgprs" x) .
                              showString ", " .
                                showString "numPhysicalSgprs = " .
                                  showsPrec d (getField @"numPhysicalSgprs" x) .
                                    showString ", " .
                                      showString "numAvailableVgprs = " .
                                        showsPrec d (getField @"numAvailableVgprs" x) .
                                          showString ", " .
                                            showString "numAvailableSgprs = " .
                                              showsPrec d (getField @"numAvailableSgprs" x) .
                                                showString ", " .
                                                  (showString "computeWorkGroupSize = [" .
                                                     showsPrec d
                                                       (let s = sizeOf
                                                                  (undefined ::
                                                                     FieldType
                                                                       "computeWorkGroupSize"
                                                                       VkShaderStatisticsInfoAMD)
                                                            o = fieldOffset @"computeWorkGroupSize"
                                                                  @VkShaderStatisticsInfoAMD
                                                            f i
                                                              = peekByteOff (unsafePtr x) i ::
                                                                  IO
                                                                    (FieldType
                                                                       "computeWorkGroupSize"
                                                                       VkShaderStatisticsInfoAMD)
                                                          in
                                                          unsafeDupablePerformIO . mapM f $
                                                            map (\ i -> o + i * s) [0 .. 3 - 1])
                                                       . showChar ']')
                                                    . showChar '}'
