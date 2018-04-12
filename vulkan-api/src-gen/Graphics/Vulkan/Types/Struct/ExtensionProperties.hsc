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
module Graphics.Vulkan.Types.Struct.ExtensionProperties
       (VkExtensionProperties(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##, Proxy##,
                                                   byteArrayContents##,
                                                   plusAddr##, proxy##)
import           GHC.TypeLits                     (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants        (VK_MAX_EXTENSION_NAME_SIZE, pattern VK_MAX_EXTENSION_NAME_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExtensionProperties {
--   >     char            extensionName[VK_MAX_EXTENSION_NAME_SIZE];
--   >     uint32_t        specVersion;
--   > } VkExtensionProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExtensionProperties VkExtensionProperties registry at www.khronos.org>
data VkExtensionProperties = VkExtensionProperties## Addr##
                                                    ByteArray##

instance Eq VkExtensionProperties where
        (VkExtensionProperties## a _) == x@(VkExtensionProperties## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkExtensionProperties where
        (VkExtensionProperties## a _) `compare`
          x@(VkExtensionProperties## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkExtensionProperties where
        sizeOf ~_ = #{size VkExtensionProperties}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkExtensionProperties}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkExtensionProperties where
        unsafeAddr (VkExtensionProperties## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkExtensionProperties## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkExtensionProperties## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkExtensionProperties where
        type StructFields VkExtensionProperties =
             '["extensionName", "specVersion"] -- ' closing tick for hsc2hs
        type CUnionType VkExtensionProperties = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkExtensionProperties = 'True -- ' closing tick for hsc2hs
        type StructExtends VkExtensionProperties = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "extensionName" VkExtensionProperties where
        type FieldType "extensionName" VkExtensionProperties = CChar
        type FieldOptional "extensionName" VkExtensionProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "extensionName" VkExtensionProperties =
             #{offset VkExtensionProperties, extensionName}
        type FieldIsArray "extensionName" VkExtensionProperties = 'True -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExtensionProperties, extensionName}

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "extensionName" idx VkExtensionProperties) =>
         CanReadFieldArray "extensionName" idx VkExtensionProperties
         where
        {-# SPECIALISE instance
                       CanReadFieldArray "extensionName" 0 VkExtensionProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "extensionName" 1 VkExtensionProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "extensionName" 2 VkExtensionProperties #-}

        {-# SPECIALISE instance
                       CanReadFieldArray "extensionName" 3 VkExtensionProperties #-}
        type FieldArrayLength "extensionName" VkExtensionProperties =
             VK_MAX_EXTENSION_NAME_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_EXTENSION_NAME_SIZE

        {-# INLINE getFieldArray #-}
        getFieldArray = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkExtensionProperties, extensionName} +
                      sizeOf (undefined :: CChar) *
                        fromInteger (natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray p
          = peekByteOff p
              (#{offset VkExtensionProperties, extensionName} +
                 sizeOf (undefined :: CChar) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         (KnownNat idx,
          IndexInBounds "extensionName" idx VkExtensionProperties) =>
         CanWriteFieldArray "extensionName" idx VkExtensionProperties
         where
        {-# SPECIALISE instance
                       CanWriteFieldArray "extensionName" 0 VkExtensionProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "extensionName" 1 VkExtensionProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "extensionName" 2 VkExtensionProperties #-}

        {-# SPECIALISE instance
                       CanWriteFieldArray "extensionName" 3 VkExtensionProperties #-}

        {-# INLINE writeFieldArray #-}
        writeFieldArray p
          = pokeByteOff p
              (#{offset VkExtensionProperties, extensionName} +
                 sizeOf (undefined :: CChar) *
                   fromInteger (natVal' (proxy## :: Proxy## idx))) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "specVersion" VkExtensionProperties where
        type FieldType "specVersion" VkExtensionProperties = Word32
        type FieldOptional "specVersion" VkExtensionProperties = 'False -- ' closing tick for hsc2hs
        type FieldOffset "specVersion" VkExtensionProperties =
             #{offset VkExtensionProperties, specVersion}
        type FieldIsArray "specVersion" VkExtensionProperties = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkExtensionProperties, specVersion}

instance {-# OVERLAPPING #-}
         CanReadField "specVersion" VkExtensionProperties where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExtensionProperties, specVersion})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkExtensionProperties, specVersion}

instance {-# OVERLAPPING #-}
         CanWriteField "specVersion" VkExtensionProperties where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkExtensionProperties, specVersion}

instance Show VkExtensionProperties where
        showsPrec d x
          = showString "VkExtensionProperties {" .
              (showString "extensionName = [" .
                 showsPrec d
                   (let s = sizeOf
                              (undefined :: FieldType "extensionName" VkExtensionProperties)
                        o = fieldOffset @"extensionName" @VkExtensionProperties
                        f i
                          = peekByteOff (unsafePtr x) i ::
                              IO (FieldType "extensionName" VkExtensionProperties)
                      in
                      unsafeDupablePerformIO . mapM f $
                        map (\ i -> o + i * s) [0 .. VK_MAX_EXTENSION_NAME_SIZE - 1])
                   . showChar ']')
                .
                showString ", " .
                  showString "specVersion = " .
                    showsPrec d (getField @"specVersion" x) . showChar '}'
