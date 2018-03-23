#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPhysicalDeviceSubgroupProperties
       (VkPhysicalDeviceSubgroupProperties(..)) where
import           Foreign.Storable                                         (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                          (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkShaderStageFlags            (VkShaderStageFlags)
import           Graphics.Vulkan.Types.Enum.VkStructureType               (VkStructureType)
import           Graphics.Vulkan.Types.Enum.VkSubgroupFeatureFlags        (VkSubgroupFeatureFlags)
import           Graphics.Vulkan.Types.Struct.VkPhysicalDeviceProperties2 (VkPhysicalDeviceProperties2)
import           System.IO.Unsafe                                         (unsafeDupablePerformIO)

-- | > typedef struct VkPhysicalDeviceSubgroupProperties {
--   >     VkStructureType sType;
--   >     void*                   pNext;
--   >     uint32_t                      subgroupSize;
--   >     VkShaderStageFlags            supportedStages;
--   >     VkSubgroupFeatureFlags        supportedOperations;
--   >     VkBool32 quadOperationsInAllStages;
--   > } VkPhysicalDeviceSubgroupProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceSubgroupProperties.html VkPhysicalDeviceSubgroupProperties registry at www.khronos.org>
data VkPhysicalDeviceSubgroupProperties = VkPhysicalDeviceSubgroupProperties## Addr##
                                                                              ByteArray##

instance Eq VkPhysicalDeviceSubgroupProperties where
        (VkPhysicalDeviceSubgroupProperties## a _) ==
          x@(VkPhysicalDeviceSubgroupProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPhysicalDeviceSubgroupProperties where
        (VkPhysicalDeviceSubgroupProperties## a _) `compare`
          x@(VkPhysicalDeviceSubgroupProperties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPhysicalDeviceSubgroupProperties where
        sizeOf ~_ = #{size VkPhysicalDeviceSubgroupProperties}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPhysicalDeviceSubgroupProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPhysicalDeviceSubgroupProperties where
        unsafeAddr (VkPhysicalDeviceSubgroupProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPhysicalDeviceSubgroupProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPhysicalDeviceSubgroupProperties##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPhysicalDeviceSubgroupProperties where
        type StructFields VkPhysicalDeviceSubgroupProperties =
             '["sType", "pNext", "subgroupSize", "supportedStages", -- ' closing tick for hsc2hs
               "supportedOperations", "quadOperationsInAllStages"]
        type CUnionType VkPhysicalDeviceSubgroupProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPhysicalDeviceSubgroupProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkPhysicalDeviceSubgroupProperties =
             '[VkPhysicalDeviceProperties2] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPhysicalDeviceSubgroupProperties where
        type FieldType "sType" VkPhysicalDeviceSubgroupProperties =
             VkStructureType
        type FieldOptional "sType" VkPhysicalDeviceSubgroupProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPhysicalDeviceSubgroupProperties =
             #{offset VkPhysicalDeviceSubgroupProperties, sType}
        type FieldIsArray "sType" VkPhysicalDeviceSubgroupProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSubgroupProperties, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPhysicalDeviceSubgroupProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSubgroupProperties, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSubgroupProperties, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPhysicalDeviceSubgroupProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSubgroupProperties, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPhysicalDeviceSubgroupProperties where
        type FieldType "pNext" VkPhysicalDeviceSubgroupProperties =
             Ptr Void
        type FieldOptional "pNext" VkPhysicalDeviceSubgroupProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPhysicalDeviceSubgroupProperties =
             #{offset VkPhysicalDeviceSubgroupProperties, pNext}
        type FieldIsArray "pNext" VkPhysicalDeviceSubgroupProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSubgroupProperties, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPhysicalDeviceSubgroupProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSubgroupProperties, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSubgroupProperties, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPhysicalDeviceSubgroupProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSubgroupProperties, pNext}

instance {-# OVERLAPPING #-}
         HasField "subgroupSize" VkPhysicalDeviceSubgroupProperties where
        type FieldType "subgroupSize" VkPhysicalDeviceSubgroupProperties =
             Word32
        type FieldOptional "subgroupSize"
               VkPhysicalDeviceSubgroupProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "subgroupSize" VkPhysicalDeviceSubgroupProperties
             =
             #{offset VkPhysicalDeviceSubgroupProperties, subgroupSize}
        type FieldIsArray "subgroupSize" VkPhysicalDeviceSubgroupProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSubgroupProperties, subgroupSize}

instance {-# OVERLAPPING #-}
         CanReadField "subgroupSize" VkPhysicalDeviceSubgroupProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSubgroupProperties, subgroupSize})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSubgroupProperties, subgroupSize}

instance {-# OVERLAPPING #-}
         CanWriteField "subgroupSize" VkPhysicalDeviceSubgroupProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSubgroupProperties, subgroupSize}

instance {-# OVERLAPPING #-}
         HasField "supportedStages" VkPhysicalDeviceSubgroupProperties where
        type FieldType "supportedStages" VkPhysicalDeviceSubgroupProperties
             = VkShaderStageFlags
        type FieldOptional "supportedStages"
               VkPhysicalDeviceSubgroupProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "supportedStages"
               VkPhysicalDeviceSubgroupProperties
             =
             #{offset VkPhysicalDeviceSubgroupProperties, supportedStages}
        type FieldIsArray "supportedStages"
               VkPhysicalDeviceSubgroupProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSubgroupProperties, supportedStages}

instance {-# OVERLAPPING #-}
         CanReadField "supportedStages" VkPhysicalDeviceSubgroupProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSubgroupProperties, supportedStages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSubgroupProperties, supportedStages}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedStages" VkPhysicalDeviceSubgroupProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSubgroupProperties, supportedStages}

instance {-# OVERLAPPING #-}
         HasField "supportedOperations" VkPhysicalDeviceSubgroupProperties
         where
        type FieldType "supportedOperations"
               VkPhysicalDeviceSubgroupProperties
             = VkSubgroupFeatureFlags
        type FieldOptional "supportedOperations"
               VkPhysicalDeviceSubgroupProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "supportedOperations"
               VkPhysicalDeviceSubgroupProperties
             =
             #{offset VkPhysicalDeviceSubgroupProperties, supportedOperations}
        type FieldIsArray "supportedOperations"
               VkPhysicalDeviceSubgroupProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSubgroupProperties, supportedOperations}

instance {-# OVERLAPPING #-}
         CanReadField "supportedOperations"
           VkPhysicalDeviceSubgroupProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSubgroupProperties, supportedOperations})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSubgroupProperties, supportedOperations}

instance {-# OVERLAPPING #-}
         CanWriteField "supportedOperations"
           VkPhysicalDeviceSubgroupProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSubgroupProperties, supportedOperations}

instance {-# OVERLAPPING #-}
         HasField "quadOperationsInAllStages"
           VkPhysicalDeviceSubgroupProperties
         where
        type FieldType "quadOperationsInAllStages"
               VkPhysicalDeviceSubgroupProperties
             = VkBool32
        type FieldOptional "quadOperationsInAllStages"
               VkPhysicalDeviceSubgroupProperties
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "quadOperationsInAllStages"
               VkPhysicalDeviceSubgroupProperties
             =
             #{offset VkPhysicalDeviceSubgroupProperties, quadOperationsInAllStages}
        type FieldIsArray "quadOperationsInAllStages"
               VkPhysicalDeviceSubgroupProperties
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPhysicalDeviceSubgroupProperties, quadOperationsInAllStages}

instance {-# OVERLAPPING #-}
         CanReadField "quadOperationsInAllStages"
           VkPhysicalDeviceSubgroupProperties
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPhysicalDeviceSubgroupProperties, quadOperationsInAllStages})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPhysicalDeviceSubgroupProperties, quadOperationsInAllStages}

instance {-# OVERLAPPING #-}
         CanWriteField "quadOperationsInAllStages"
           VkPhysicalDeviceSubgroupProperties
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPhysicalDeviceSubgroupProperties, quadOperationsInAllStages}

instance Show VkPhysicalDeviceSubgroupProperties where
        showsPrec d x
          = showString "VkPhysicalDeviceSubgroupProperties {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "subgroupSize = " .
                            showsPrec d (getField @"subgroupSize" x) .
                              showString ", " .
                                showString "supportedStages = " .
                                  showsPrec d (getField @"supportedStages" x) .
                                    showString ", " .
                                      showString "supportedOperations = " .
                                        showsPrec d (getField @"supportedOperations" x) .
                                          showString ", " .
                                            showString "quadOperationsInAllStages = " .
                                              showsPrec d (getField @"quadOperationsInAllStages" x)
                                                . showChar '}'
