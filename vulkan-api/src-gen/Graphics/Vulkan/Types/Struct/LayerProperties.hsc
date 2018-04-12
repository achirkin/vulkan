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
module Graphics.Vulkan.Types.Struct.LayerProperties
       (VkLayerProperties(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##, Proxy##,
                                                   byteArrayContents##,
                                                   plusAddr##, proxy##)
import           GHC.TypeLits                     (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants        (VK_MAX_DESCRIPTION_SIZE, pattern VK_MAX_DESCRIPTION_SIZE,
                                                   VK_MAX_EXTENSION_NAME_SIZE,
                                                   pattern VK_MAX_EXTENSION_NAME_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkLayerProperties {
--   >     char            layerName[VK_MAX_EXTENSION_NAME_SIZE];
--   >     uint32_t        specVersion;
--   >     uint32_t        implementationVersion;
--   >     char            description[VK_MAX_DESCRIPTION_SIZE];
--   > } VkLayerProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkLayerProperties VkLayerProperties registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "layerName" idx VkLayerProperties) =>
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkLayerProperties, layerName} +
                      sizeOf (undefined :: CChar) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkLayerProperties, layerName} +
                 sizeOf (undefined :: CChar) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx, IndexInBounds "layerName" idx VkLayerProperties) =>
         CanWriteFieldArray "layerName" idx VkLayerProperties
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "layerName" 0 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "layerName" 1 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "layerName" 2 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "layerName" 3 VkLayerProperties #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkLayerProperties, layerName} +
                 sizeOf (undefined :: CChar) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

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

instance {-# OVERLAPPING #-}
         CanReadField "specVersion" VkLayerProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkLayerProperties, specVersion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkLayerProperties, specVersion}

instance {-# OVERLAPPING #-}
         CanWriteField "specVersion" VkLayerProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkLayerProperties, specVersion}

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

instance {-# OVERLAPPING #-}
         CanReadField "implementationVersion" VkLayerProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkLayerProperties, implementationVersion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkLayerProperties, implementationVersion}

instance {-# OVERLAPPING #-}
         CanWriteField "implementationVersion" VkLayerProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkLayerProperties, implementationVersion}

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

instance {-# OVERLAPPING #-}
         (KnownNat idx,
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
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkLayerProperties, description} +
                      sizeOf (undefined :: CChar) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkLayerProperties, description} +
                 sizeOf (undefined :: CChar) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "description" idx VkLayerProperties) =>
         CanWriteFieldArray "description" idx VkLayerProperties
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "description" 0 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "description" 1 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "description" 2 VkLayerProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "description" 3 VkLayerProperties #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkLayerProperties, description} +
                 sizeOf (undefined :: CChar) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance Show VkLayerProperties where
        showsPrec d x
          = showString "VkLayerProperties {" .
              (showString "layerName = [" .
                 showsPrec d
                   (let s = sizeOf
                              (undefined :: FieldType "layerName" VkLayerProperties)
                        o = fieldOffset @"layerName" @VkLayerProperties
                        f i
                          = peekByteOff (unsafePtr x) i ::
                              IO (FieldType "layerName" VkLayerProperties)
                      in
                      unsafeDupablePerformIO . mapM f $
                        map (\ i -> o + i * s) [0 .. VK_MAX_EXTENSION_NAME_SIZE - 1])
                   . showChar ']')
                .
                showString ", " .
                  showString "specVersion = " .
                    showsPrec d (getField @"specVersion" x) .
                      showString ", " .
                        showString "implementationVersion = " .
                          showsPrec d (getField @"implementationVersion" x) .
                            showString ", " .
                              (showString "description = [" .
                                 showsPrec d
                                   (let s = sizeOf
                                              (undefined ::
                                                 FieldType "description" VkLayerProperties)
                                        o = fieldOffset @"description" @VkLayerProperties
                                        f i
                                          = peekByteOff (unsafePtr x) i ::
                                              IO (FieldType "description" VkLayerProperties)
                                      in
                                      unsafeDupablePerformIO . mapM f $
                                        map (\ i -> o + i * s) [0 .. VK_MAX_DESCRIPTION_SIZE - 1])
                                   . showChar ']')
                                . showChar '}'
