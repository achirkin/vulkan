#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkIndirectCommandsLayoutTokenNVX
       (VkIndirectCommandsLayoutTokenNVX(..)) where
import           Foreign.Storable                                          (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkIndirectCommandsTokenTypeNVX (VkIndirectCommandsTokenTypeNVX)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                          (unsafeDupablePerformIO)

-- | > typedef struct VkIndirectCommandsLayoutTokenNVX {
--   >     VkIndirectCommandsTokenTypeNVX      tokenType;
--   >     uint32_t                         bindingUnit;
--   >     uint32_t                         dynamicCount;
--   >     uint32_t                         divisor;
--   > } VkIndirectCommandsLayoutTokenNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkIndirectCommandsLayoutTokenNVX.html VkIndirectCommandsLayoutTokenNVX registry at www.khronos.org>
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
         HasVkTokenType VkIndirectCommandsLayoutTokenNVX where
        type VkTokenTypeMType VkIndirectCommandsLayoutTokenNVX =
             VkIndirectCommandsTokenTypeNVX

        {-# NOINLINE vkTokenType #-}
        vkTokenType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, tokenType})

        {-# INLINE vkTokenTypeByteOffset #-}
        vkTokenTypeByteOffset ~_
          = #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}

        {-# INLINE readVkTokenType #-}
        readVkTokenType p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}

        {-# INLINE writeVkTokenType #-}
        writeVkTokenType p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, tokenType}

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

instance CanReadField "tokenType" VkIndirectCommandsLayoutTokenNVX
         where
        {-# INLINE getField #-}
        getField = vkTokenType

        {-# INLINE readField #-}
        readField = readVkTokenType

instance CanWriteField "tokenType" VkIndirectCommandsLayoutTokenNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkTokenType

instance {-# OVERLAPPING #-}
         HasVkBindingUnit VkIndirectCommandsLayoutTokenNVX where
        type VkBindingUnitMType VkIndirectCommandsLayoutTokenNVX = Word32

        {-# NOINLINE vkBindingUnit #-}
        vkBindingUnit x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit})

        {-# INLINE vkBindingUnitByteOffset #-}
        vkBindingUnitByteOffset ~_
          = #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}

        {-# INLINE readVkBindingUnit #-}
        readVkBindingUnit p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}

        {-# INLINE writeVkBindingUnit #-}
        writeVkBindingUnit p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, bindingUnit}

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

instance CanReadField "bindingUnit"
           VkIndirectCommandsLayoutTokenNVX
         where
        {-# INLINE getField #-}
        getField = vkBindingUnit

        {-# INLINE readField #-}
        readField = readVkBindingUnit

instance CanWriteField "bindingUnit"
           VkIndirectCommandsLayoutTokenNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkBindingUnit

instance {-# OVERLAPPING #-}
         HasVkDynamicCount VkIndirectCommandsLayoutTokenNVX where
        type VkDynamicCountMType VkIndirectCommandsLayoutTokenNVX = Word32

        {-# NOINLINE vkDynamicCount #-}
        vkDynamicCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount})

        {-# INLINE vkDynamicCountByteOffset #-}
        vkDynamicCountByteOffset ~_
          = #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}

        {-# INLINE readVkDynamicCount #-}
        readVkDynamicCount p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}

        {-# INLINE writeVkDynamicCount #-}
        writeVkDynamicCount p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, dynamicCount}

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

instance CanReadField "dynamicCount"
           VkIndirectCommandsLayoutTokenNVX
         where
        {-# INLINE getField #-}
        getField = vkDynamicCount

        {-# INLINE readField #-}
        readField = readVkDynamicCount

instance CanWriteField "dynamicCount"
           VkIndirectCommandsLayoutTokenNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDynamicCount

instance {-# OVERLAPPING #-}
         HasVkDivisor VkIndirectCommandsLayoutTokenNVX where
        type VkDivisorMType VkIndirectCommandsLayoutTokenNVX = Word32

        {-# NOINLINE vkDivisor #-}
        vkDivisor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutTokenNVX, divisor})

        {-# INLINE vkDivisorByteOffset #-}
        vkDivisorByteOffset ~_
          = #{offset VkIndirectCommandsLayoutTokenNVX, divisor}

        {-# INLINE readVkDivisor #-}
        readVkDivisor p
          = peekByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, divisor}

        {-# INLINE writeVkDivisor #-}
        writeVkDivisor p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutTokenNVX, divisor}

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

instance CanReadField "divisor" VkIndirectCommandsLayoutTokenNVX
         where
        {-# INLINE getField #-}
        getField = vkDivisor

        {-# INLINE readField #-}
        readField = readVkDivisor

instance CanWriteField "divisor" VkIndirectCommandsLayoutTokenNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkDivisor

instance Show VkIndirectCommandsLayoutTokenNVX where
        showsPrec d x
          = showString "VkIndirectCommandsLayoutTokenNVX {" .
              showString "vkTokenType = " .
                showsPrec d (vkTokenType x) .
                  showString ", " .
                    showString "vkBindingUnit = " .
                      showsPrec d (vkBindingUnit x) .
                        showString ", " .
                          showString "vkDynamicCount = " .
                            showsPrec d (vkDynamicCount x) .
                              showString ", " .
                                showString "vkDivisor = " .
                                  showsPrec d (vkDivisor x) . showChar '}'
