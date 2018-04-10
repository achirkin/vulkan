#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkIndirectCommandsLayoutTokenNVX
       (VkIndirectCommandsLayoutTokenNVX(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Base                                                  (Addr##,
                                                                            ByteArray##,
                                                                            byteArrayContents##,
                                                                            plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkIndirectCommandsTokenTypeNVX (VkIndirectCommandsTokenTypeNVX)
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkIndirectCommandsLayoutTokenNVX {
--   >     VkIndirectCommandsTokenTypeNVX      tokenType;
--   >     uint32_t                         bindingUnit;
--   >     uint32_t                         dynamicCount;
--   >     uint32_t                         divisor;
--   > } VkIndirectCommandsLayoutTokenNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkIndirectCommandsLayoutTokenNVX VkIndirectCommandsLayoutTokenNVX registry at www.khronos.org>
data VkIndirectCommandsLayoutTokenNVX = VkIndirectCommandsLayoutTokenNVX## Addr##
                                                                          ByteArray##

instance Eq VkIndirectCommandsLayoutTokenNVX where
        (VkIndirectCommandsLayoutTokenNVX## a _) ==
          x@(VkIndirectCommandsLayoutTokenNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkIndirectCommandsLayoutTokenNVX where
        (VkIndirectCommandsLayoutTokenNVX## a _) `compare`
          x@(VkIndirectCommandsLayoutTokenNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkIndirectCommandsLayoutTokenNVX where
        sizeOf ~_ = #{size VkIndirectCommandsLayoutTokenNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkIndirectCommandsLayoutTokenNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkIndirectCommandsLayoutTokenNVX where
        unsafeAddr (VkIndirectCommandsLayoutTokenNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkIndirectCommandsLayoutTokenNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkIndirectCommandsLayoutTokenNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkIndirectCommandsLayoutTokenNVX where
        type StructFields VkIndirectCommandsLayoutTokenNVX =
             '["tokenType", "bindingUnit", "dynamicCount", "divisor"] -- ' closing tick for hsc2hs
        type CUnionType VkIndirectCommandsLayoutTokenNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkIndirectCommandsLayoutTokenNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkIndirectCommandsLayoutTokenNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "tokenType" VkIndirectCommandsLayoutTokenNVX where
        type FieldType "tokenType" VkIndirectCommandsLayoutTokenNVX =
             VkIndirectCommandsTokenTypeNVX
        type FieldOptional "tokenType" VkIndirectCommandsLayoutTokenNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "tokenType" VkIndirectCommandsLayoutTokenNVX =
             #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}
        type FieldIsArray "tokenType" VkIndirectCommandsLayoutTokenNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}

instance {-# OVERLAPPING #-}
         CanReadField "tokenType" VkIndirectCommandsLayoutTokenNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, tokenType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}

instance {-# OVERLAPPING #-}
         CanWriteField "tokenType" VkIndirectCommandsLayoutTokenNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}

instance {-# OVERLAPPING #-}
         HasField "bindingUnit" VkIndirectCommandsLayoutTokenNVX where
        type FieldType "bindingUnit" VkIndirectCommandsLayoutTokenNVX =
             Word32
        type FieldOptional "bindingUnit" VkIndirectCommandsLayoutTokenNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "bindingUnit" VkIndirectCommandsLayoutTokenNVX =
             #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}
        type FieldIsArray "bindingUnit" VkIndirectCommandsLayoutTokenNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}

instance {-# OVERLAPPING #-}
         CanReadField "bindingUnit" VkIndirectCommandsLayoutTokenNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}

instance {-# OVERLAPPING #-}
         CanWriteField "bindingUnit" VkIndirectCommandsLayoutTokenNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}

instance {-# OVERLAPPING #-}
         HasField "dynamicCount" VkIndirectCommandsLayoutTokenNVX where
        type FieldType "dynamicCount" VkIndirectCommandsLayoutTokenNVX =
             Word32
        type FieldOptional "dynamicCount" VkIndirectCommandsLayoutTokenNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dynamicCount" VkIndirectCommandsLayoutTokenNVX =
             #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}
        type FieldIsArray "dynamicCount" VkIndirectCommandsLayoutTokenNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}

instance {-# OVERLAPPING #-}
         CanReadField "dynamicCount" VkIndirectCommandsLayoutTokenNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}

instance {-# OVERLAPPING #-}
         CanWriteField "dynamicCount" VkIndirectCommandsLayoutTokenNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}

instance {-# OVERLAPPING #-}
         HasField "divisor" VkIndirectCommandsLayoutTokenNVX where
        type FieldType "divisor" VkIndirectCommandsLayoutTokenNVX = Word32
        type FieldOptional "divisor" VkIndirectCommandsLayoutTokenNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "divisor" VkIndirectCommandsLayoutTokenNVX =
             #{offset VkIndirectCommandsLayoutTokenNVX, divisor}
        type FieldIsArray "divisor" VkIndirectCommandsLayoutTokenNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsLayoutTokenNVX, divisor}

instance {-# OVERLAPPING #-}
         CanReadField "divisor" VkIndirectCommandsLayoutTokenNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, divisor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, divisor}

instance {-# OVERLAPPING #-}
         CanWriteField "divisor" VkIndirectCommandsLayoutTokenNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, divisor}

instance Show VkIndirectCommandsLayoutTokenNVX where
        showsPrec d x
          = showString "VkIndirectCommandsLayoutTokenNVX {" .
              showString "tokenType = " .
                showsPrec d (getField @"tokenType" x) .
                  showString ", " .
                    showString "bindingUnit = " .
                      showsPrec d (getField @"bindingUnit" x) .
                        showString ", " .
                          showString "dynamicCount = " .
                            showsPrec d (getField @"dynamicCount" x) .
                              showString ", " .
                                showString "divisor = " .
                                  showsPrec d (getField @"divisor" x) . showChar '}'
