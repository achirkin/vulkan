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
       (VkExtensionProperties, VkExtensionProperties') where -- ' closing tick for hsc2hs
import Foreign.Storable                 (Storable (..))
import Graphics.Vulkan.Constants        (VK_MAX_EXTENSION_NAME_SIZE,
                                         pattern VK_MAX_EXTENSION_NAME_SIZE)
import Graphics.Vulkan.Marshal
import Graphics.Vulkan.Marshal.Internal
import System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkExtensionProperties {
--   >     char            extensionName[VK_MAX_EXTENSION_NAME_SIZE];
--   >     uint32_t        specVersion;
--   > } VkExtensionProperties;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkExtensionProperties VkExtensionProperties registry at www.khronos.org>
type VkExtensionProperties = VulkanStruct VkExtensionProperties' -- ' closing tick for hsc2hs

data VkExtensionProperties' -- ' closing tick for hsc2hs

instance Eq VkExtensionProperties where
        a == b = EQ == cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

        {-# INLINE (==) #-}

instance Ord VkExtensionProperties where
        compare a b = cmpBytes## (sizeOf a) (unsafeAddr a) (unsafeAddr b)

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
         CanReadFieldArray "extensionName" VkExtensionProperties where
        type FieldArrayLength "extensionName" VkExtensionProperties =
             VK_MAX_EXTENSION_NAME_SIZE

        {-# INLINE fieldArrayLength #-}
        fieldArrayLength = VK_MAX_EXTENSION_NAME_SIZE

        {-# INLINE getFieldArrayUnsafe #-}
        getFieldArrayUnsafe i = f
          where {-# NOINLINE f #-}
                f x = unsafeDupablePerformIO (peekByteOff (unsafePtr x) off)
                off
                  = #{offset VkExtensionProperties, extensionName} +
                      sizeOf (undefined :: CChar) * i

        {-# INLINE readFieldArrayUnsafe #-}
        readFieldArrayUnsafe i p
          = peekByteOff p
              (#{offset VkExtensionProperties, extensionName} +
                 sizeOf (undefined :: CChar) * i)

instance {-# OVERLAPPING #-}
         CanWriteFieldArray "extensionName" VkExtensionProperties where
        {-# INLINE writeFieldArrayUnsafe #-}
        writeFieldArrayUnsafe i p
          = pokeByteOff p
              (#{offset VkExtensionProperties, extensionName} +
                 sizeOf (undefined :: CChar) * i)

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
