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
module Graphics.Vulkan.Types.Struct.VkExtensionProperties
       (VkExtensionProperties(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           GHC.TypeLits                        (KnownNat, natVal') -- ' closing tick for hsc2hs
import           Graphics.Vulkan.Constants           (VK_MAX_EXTENSION_NAME_SIZE,
                                                      pattern VK_MAX_EXTENSION_NAME_SIZE)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkExtensionProperties {
--   >     char            extensionName[VK_MAX_EXTENSION_NAME_SIZE];
--   >     uint32_t        specVersion;
--   > } VkExtensionProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkExtensionProperties.html VkExtensionProperties registry at www.khronos.org>
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
         HasVkExtensionNameArray VkExtensionProperties where
        type VkExtensionNameArrayMType VkExtensionProperties = CChar

        {-# NOINLINE vkExtensionNameArray #-}
        vkExtensionNameArray x idx
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x)
                 (idx * sizeOf (undefined :: CChar) +
                    #{offset VkExtensionProperties, extensionName}))

        {-# INLINE vkExtensionNameArrayByteOffset #-}
        vkExtensionNameArrayByteOffset ~_
          = #{offset VkExtensionProperties, extensionName}

        {-# INLINE readVkExtensionNameArray #-}
        readVkExtensionNameArray p idx
          = peekByteOff p
              (idx * sizeOf (undefined :: CChar) +
                 #{offset VkExtensionProperties, extensionName})

        {-# INLINE writeVkExtensionNameArray #-}
        writeVkExtensionNameArray p idx
          = pokeByteOff p
              (idx * sizeOf (undefined :: CChar) +
                 #{offset VkExtensionProperties, extensionName})

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

instance (KnownNat idx,
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
        getFieldArray x
          = vkExtensionNameArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

        {-# INLINE readFieldArray #-}
        readFieldArray x
          = readVkExtensionNameArray x
              (fromInteger $ natVal' (proxy## :: Proxy## idx)) -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkSpecVersion VkExtensionProperties
         where
        type VkSpecVersionMType VkExtensionProperties = Word32

        {-# NOINLINE vkSpecVersion #-}
        vkSpecVersion x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkExtensionProperties, specVersion})

        {-# INLINE vkSpecVersionByteOffset #-}
        vkSpecVersionByteOffset ~_
          = #{offset VkExtensionProperties, specVersion}

        {-# INLINE readVkSpecVersion #-}
        readVkSpecVersion p
          = peekByteOff p #{offset VkExtensionProperties, specVersion}

        {-# INLINE writeVkSpecVersion #-}
        writeVkSpecVersion p
          = pokeByteOff p #{offset VkExtensionProperties, specVersion}

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

instance CanReadField "specVersion" VkExtensionProperties where
        {-# INLINE getField #-}
        getField = vkSpecVersion

        {-# INLINE readField #-}
        readField = readVkSpecVersion

instance Show VkExtensionProperties where
        showsPrec d x
          = showString "VkExtensionProperties {" .
              showString "vkExtensionNameArray = [" .
                showsPrec d
                  (map (vkExtensionNameArray x) [1 .. VK_MAX_EXTENSION_NAME_SIZE])
                  .
                  showChar ']' .
                    showString ", " .
                      showString "vkSpecVersion = " .
                        showsPrec d (vkSpecVersion x) . showChar '}'
