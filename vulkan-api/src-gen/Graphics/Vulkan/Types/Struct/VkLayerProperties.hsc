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
module Graphics.Vulkan.Types.Struct.VkLayerProperties
       (VkLayerProperties(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                        (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants           (VK_MAX_DESCRIPTION_SIZE, pattern VK_MAX_DESCRIPTION_SIZE,
                                                      VK_MAX_EXTENSION_NAME_SIZE,
                                                      pattern VK_MAX_EXTENSION_NAME_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkLayerProperties {
--   >     char            layerName[VK_MAX_EXTENSION_NAME_SIZE];
--   >     uint32_t        specVersion;
--   >     uint32_t        implementationVersion;
--   >     char            description[VK_MAX_DESCRIPTION_SIZE];
--   > } VkLayerProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkLayerProperties.html VkLayerProperties registry at www.khronos.org>
data VkLayerProperties = VkLayerProperties## Addr## ByteArray##

instance Eq VkLayerProperties where
        (VkLayerProperties## a _) == x@(VkLayerProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkLayerProperties where
        (VkLayerProperties## a _) `compare` x@(VkLayerProperties## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkLayerProperties where
        sizeOf ~_ = #{size VkLayerProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkLayerProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkLayerProperties where
        unsafeAddr (VkLayerProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkLayerProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkLayerProperties## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkLayerProperties where
        type StructFields VkLayerProperties =
             '["layerName", "specVersion", "implementationVersion", -- ' closing tick for hsc2hs
               "description"]
        type CUnionType VkLayerProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkLayerProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkLayerProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkLayerNameArray VkLayerProperties
         where
        type VkLayerNameArrayMType VkLayerProperties = CChar

        {-# NOINLINE vkLayerNameArray #-}
        vkLayerNameArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: CChar) +
                    #{offset VkLayerProperties, layerName}))

        {-# INLINE vkLayerNameArrayByteOffset #-}
        vkLayerNameArrayByteOffset ~_
          = #{offset VkLayerProperties, layerName}

        {-# INLINE readVkLayerNameArray #-}
        readVkLayerNameArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: CChar) +
                 #{offset VkLayerProperties, layerName})

        {-# INLINE writeVkLayerNameArray #-}
        writeVkLayerNameArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: CChar) +
                 #{offset VkLayerProperties, layerName})

instance {-# OVERLAPPING #-} HasField "layerName" VkLayerProperties
         where
        type FieldType "layerName" VkLayerProperties = CChar
        type FieldOptional "layerName" VkLayerProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "layerName" VkLayerProperties =
             #{offset VkLayerProperties, layerName}
        type FieldIsArray "layerName" VkLayerProperties = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkLayerProperties, layerName}

instance (KnownNat idx,
          IndexInBounds "layerName" idx VkLayerProperties) =>
         CanReadFieldArray "layerName" idx VkLayerProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "layerName" 0 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "layerName" 1 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "layerName" 2 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "layerName" 3 VkLayerProperties #-}
        type FieldArrayLength "layerName" VkLayerProperties =
             VK_MAX_EXTENSION_NAME_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_EXTENSION_NAME_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkLayerNameArray x (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkLayerNameArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSpecVersion VkLayerProperties
         where
        type VkSpecVersionMType VkLayerProperties = Word32

        {-# NOINLINE vkSpecVersion #-}
        vkSpecVersion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkLayerProperties, specVersion})

        {-# INLINE vkSpecVersionByteOffset #-}
        vkSpecVersionByteOffset ~_
          = #{offset VkLayerProperties, specVersion}

        {-# INLINE readVkSpecVersion #-}
        readVkSpecVersion p
          = peekByteOff p #{offset VkLayerProperties, specVersion}

        {-# INLINE writeVkSpecVersion #-}
        writeVkSpecVersion p
          = pokeByteOff p #{offset VkLayerProperties, specVersion}

instance {-# OVERLAPPING #-}
         HasField "specVersion" VkLayerProperties where
        type FieldType "specVersion" VkLayerProperties = Word32
        type FieldOptional "specVersion" VkLayerProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "specVersion" VkLayerProperties =
             #{offset VkLayerProperties, specVersion}
        type FieldIsArray "specVersion" VkLayerProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkLayerProperties, specVersion}

instance CanReadField "specVersion" VkLayerProperties where
        {-# INLINE getField #-}
        getField = vkSpecVersion

        {-# INLINE readField #-}
        readField = readVkSpecVersion

instance {-# OVERLAPPING #-}
         HasVkImplementationVersion VkLayerProperties where
        type VkImplementationVersionMType VkLayerProperties = Word32

        {-# NOINLINE vkImplementationVersion #-}
        vkImplementationVersion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkLayerProperties, implementationVersion})

        {-# INLINE vkImplementationVersionByteOffset #-}
        vkImplementationVersionByteOffset ~_
          = #{offset VkLayerProperties, implementationVersion}

        {-# INLINE readVkImplementationVersion #-}
        readVkImplementationVersion p
          = peekByteOff p #{offset VkLayerProperties, implementationVersion}

        {-# INLINE writeVkImplementationVersion #-}
        writeVkImplementationVersion p
          = pokeByteOff p #{offset VkLayerProperties, implementationVersion}

instance {-# OVERLAPPING #-}
         HasField "implementationVersion" VkLayerProperties where
        type FieldType "implementationVersion" VkLayerProperties = Word32
        type FieldOptional "implementationVersion" VkLayerProperties =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "implementationVersion" VkLayerProperties =
             #{offset VkLayerProperties, implementationVersion}
        type FieldIsArray "implementationVersion" VkLayerProperties =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkLayerProperties, implementationVersion}

instance CanReadField "implementationVersion" VkLayerProperties
         where
        {-# INLINE getField #-}
        getField = vkImplementationVersion

        {-# INLINE readField #-}
        readField = readVkImplementationVersion

instance {-# OVERLAPPING #-}
         HasVkDescriptionArray VkLayerProperties where
        type VkDescriptionArrayMType VkLayerProperties = CChar

        {-# NOINLINE vkDescriptionArray #-}
        vkDescriptionArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: CChar) +
                    #{offset VkLayerProperties, description}))

        {-# INLINE vkDescriptionArrayByteOffset #-}
        vkDescriptionArrayByteOffset ~_
          = #{offset VkLayerProperties, description}

        {-# INLINE readVkDescriptionArray #-}
        readVkDescriptionArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: CChar) +
                 #{offset VkLayerProperties, description})

        {-# INLINE writeVkDescriptionArray #-}
        writeVkDescriptionArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: CChar) +
                 #{offset VkLayerProperties, description})

instance {-# OVERLAPPING #-}
         HasField "description" VkLayerProperties where
        type FieldType "description" VkLayerProperties = CChar
        type FieldOptional "description" VkLayerProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "description" VkLayerProperties =
             #{offset VkLayerProperties, description}
        type FieldIsArray "description" VkLayerProperties = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkLayerProperties, description}

instance (KnownNat idx,
          IndexInBounds "description" idx VkLayerProperties) =>
         CanReadFieldArray "description" idx VkLayerProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "description" 0 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "description" 1 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "description" 2 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "description" 3 VkLayerProperties #-}
        type FieldArrayLength "description" VkLayerProperties =
             VK_MAX_DESCRIPTION_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_DESCRIPTION_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray x
          = vkDescriptionArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkDescriptionArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance Show VkLayerProperties where
        showsPrec d x
          = showString "VkLayerProperties {" .
              showString "vkLayerNameArray = [" .
                showsPrec d
                  (map (vkLayerNameArray x) [1 .. VK_MAX_EXTENSION_NAME_SIZE])
                  .
                  showChar ']' .
                    showString ", " .
                      showString "vkSpecVersion = " .
                        showsPrec d (vkSpecVersion x) .
                          showString ", " .
                            showString "vkImplementationVersion = " .
                              showsPrec d (vkImplementationVersion x) .
                                showString ", " .
                                  showString "vkDescriptionArray = [" .
                                    showsPrec d
                                      (map (vkDescriptionArray x) [1 .. VK_MAX_DESCRIPTION_SIZE])
                                      . showChar ']' . showChar '}'
