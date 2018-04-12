#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.IndirectCommands
       (VkIndirectCommandsLayoutCreateInfoNVX(..),
        VkIndirectCommandsLayoutTokenNVX(..),
        VkIndirectCommandsTokenNVX(..))
       where
import           Foreign.Storable                            (Storable (..))
import           GHC.Base                                    (Addr##, ByteArray##,
                                                              byteArrayContents##,
                                                              plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes             (VkDeviceSize)
import           Graphics.Vulkan.Types.Enum.IndirectCommands (VkIndirectCommandsLayoutUsageFlagsNVX,
                                                              VkIndirectCommandsTokenTypeNVX)
import           Graphics.Vulkan.Types.Enum.Pipeline         (VkPipelineBindPoint)
import           Graphics.Vulkan.Types.Enum.StructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Handles               (VkBuffer)
import           System.IO.Unsafe                            (unsafeDupablePerformIO)

-- | > typedef struct VkIndirectCommandsLayoutCreateInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkPipelineBindPoint                      pipelineBindPoint;
--   >     VkIndirectCommandsLayoutUsageFlagsNVX    flags;
--   >     uint32_t                                 tokenCount;
--   >     const VkIndirectCommandsLayoutTokenNVX*  pTokens;
--   > } VkIndirectCommandsLayoutCreateInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkIndirectCommandsLayoutCreateInfoNVX VkIndirectCommandsLayoutCreateInfoNVX registry at www.khronos.org>
data VkIndirectCommandsLayoutCreateInfoNVX = VkIndirectCommandsLayoutCreateInfoNVX## Addr##
                                                                                    ByteArray##

instance Eq VkIndirectCommandsLayoutCreateInfoNVX where
        (VkIndirectCommandsLayoutCreateInfoNVX## a _) ==
          x@(VkIndirectCommandsLayoutCreateInfoNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkIndirectCommandsLayoutCreateInfoNVX where
        (VkIndirectCommandsLayoutCreateInfoNVX## a _) `compare`
          x@(VkIndirectCommandsLayoutCreateInfoNVX## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkIndirectCommandsLayoutCreateInfoNVX where
        sizeOf ~_
          = #{size VkIndirectCommandsLayoutCreateInfoNVX}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkIndirectCommandsLayoutCreateInfoNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkIndirectCommandsLayoutCreateInfoNVX
         where
        unsafeAddr (VkIndirectCommandsLayoutCreateInfoNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkIndirectCommandsLayoutCreateInfoNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkIndirectCommandsLayoutCreateInfoNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkIndirectCommandsLayoutCreateInfoNVX where
        type StructFields VkIndirectCommandsLayoutCreateInfoNVX =
             '["sType", "pNext", "pipelineBindPoint", "flags", "tokenCount", -- ' closing tick for hsc2hs
               "pTokens"]
        type CUnionType VkIndirectCommandsLayoutCreateInfoNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkIndirectCommandsLayoutCreateInfoNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkIndirectCommandsLayoutCreateInfoNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkIndirectCommandsLayoutCreateInfoNVX where
        type FieldType "sType" VkIndirectCommandsLayoutCreateInfoNVX =
             VkStructureType
        type FieldOptional "sType" VkIndirectCommandsLayoutCreateInfoNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkIndirectCommandsLayoutCreateInfoNVX =
             #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}
        type FieldIsArray "sType" VkIndirectCommandsLayoutCreateInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkIndirectCommandsLayoutCreateInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkIndirectCommandsLayoutCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkIndirectCommandsLayoutCreateInfoNVX where
        type FieldType "pNext" VkIndirectCommandsLayoutCreateInfoNVX =
             Ptr Void
        type FieldOptional "pNext" VkIndirectCommandsLayoutCreateInfoNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkIndirectCommandsLayoutCreateInfoNVX =
             #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}
        type FieldIsArray "pNext" VkIndirectCommandsLayoutCreateInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkIndirectCommandsLayoutCreateInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkIndirectCommandsLayoutCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}

instance {-# OVERLAPPING #-}
         HasField "pipelineBindPoint" VkIndirectCommandsLayoutCreateInfoNVX
         where
        type FieldType "pipelineBindPoint"
               VkIndirectCommandsLayoutCreateInfoNVX
             = VkPipelineBindPoint
        type FieldOptional "pipelineBindPoint"
               VkIndirectCommandsLayoutCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pipelineBindPoint"
               VkIndirectCommandsLayoutCreateInfoNVX
             =
             #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}
        type FieldIsArray "pipelineBindPoint"
               VkIndirectCommandsLayoutCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         CanReadField "pipelineBindPoint"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         CanWriteField "pipelineBindPoint"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}

instance {-# OVERLAPPING #-}
         HasField "flags" VkIndirectCommandsLayoutCreateInfoNVX where
        type FieldType "flags" VkIndirectCommandsLayoutCreateInfoNVX =
             VkIndirectCommandsLayoutUsageFlagsNVX
        type FieldOptional "flags" VkIndirectCommandsLayoutCreateInfoNVX =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkIndirectCommandsLayoutCreateInfoNVX =
             #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}
        type FieldIsArray "flags" VkIndirectCommandsLayoutCreateInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkIndirectCommandsLayoutCreateInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkIndirectCommandsLayoutCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}

instance {-# OVERLAPPING #-}
         HasField "tokenCount" VkIndirectCommandsLayoutCreateInfoNVX where
        type FieldType "tokenCount" VkIndirectCommandsLayoutCreateInfoNVX =
             Word32
        type FieldOptional "tokenCount"
               VkIndirectCommandsLayoutCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tokenCount" VkIndirectCommandsLayoutCreateInfoNVX
             =
             #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}
        type FieldIsArray "tokenCount"
               VkIndirectCommandsLayoutCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}

instance {-# OVERLAPPING #-}
         CanReadField "tokenCount" VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}

instance {-# OVERLAPPING #-}
         CanWriteField "tokenCount" VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}

instance {-# OVERLAPPING #-}
         HasField "pTokens" VkIndirectCommandsLayoutCreateInfoNVX where
        type FieldType "pTokens" VkIndirectCommandsLayoutCreateInfoNVX =
             Ptr VkIndirectCommandsLayoutTokenNVX
        type FieldOptional "pTokens" VkIndirectCommandsLayoutCreateInfoNVX
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "pTokens" VkIndirectCommandsLayoutCreateInfoNVX =
             #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}
        type FieldIsArray "pTokens" VkIndirectCommandsLayoutCreateInfoNVX =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}

instance {-# OVERLAPPING #-}
         CanReadField "pTokens" VkIndirectCommandsLayoutCreateInfoNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}

instance {-# OVERLAPPING #-}
         CanWriteField "pTokens" VkIndirectCommandsLayoutCreateInfoNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}

instance Show VkIndirectCommandsLayoutCreateInfoNVX where
        showsPrec d x
          = showString "VkIndirectCommandsLayoutCreateInfoNVX {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "pipelineBindPoint = " .
                            showsPrec d (getField @"pipelineBindPoint" x) .
                              showString ", " .
                                showString "flags = " .
                                  showsPrec d (getField @"flags" x) .
                                    showString ", " .
                                      showString "tokenCount = " .
                                        showsPrec d (getField @"tokenCount" x) .
                                          showString ", " .
                                            showString "pTokens = " .
                                              showsPrec d (getField @"pTokens" x) . showChar '}'

-- | > typedef struct VkIndirectCommandsLayoutTokenNVX {
--   >     VkIndirectCommandsTokenTypeNVX      tokenType;
--   >     uint32_t                         bindingUnit;
--   >     uint32_t                         dynamicCount;
--   >     uint32_t                         divisor;
--   > } VkIndirectCommandsLayoutTokenNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkIndirectCommandsLayoutTokenNVX VkIndirectCommandsLayoutTokenNVX registry at www.khronos.org>
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

-- | > typedef struct VkIndirectCommandsTokenNVX {
--   >     VkIndirectCommandsTokenTypeNVX      tokenType;
--   >     VkBuffer                         buffer;
--   >     VkDeviceSize                     offset;
--   > } VkIndirectCommandsTokenNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkIndirectCommandsTokenNVX VkIndirectCommandsTokenNVX registry at www.khronos.org>
data VkIndirectCommandsTokenNVX = VkIndirectCommandsTokenNVX## Addr##
                                                              ByteArray##

instance Eq VkIndirectCommandsTokenNVX where
        (VkIndirectCommandsTokenNVX## a _) ==
          x@(VkIndirectCommandsTokenNVX## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkIndirectCommandsTokenNVX where
        (VkIndirectCommandsTokenNVX## a _) `compare`
          x@(VkIndirectCommandsTokenNVX## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkIndirectCommandsTokenNVX where
        sizeOf ~_ = #{size VkIndirectCommandsTokenNVX}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkIndirectCommandsTokenNVX}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkIndirectCommandsTokenNVX where
        unsafeAddr (VkIndirectCommandsTokenNVX## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkIndirectCommandsTokenNVX## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkIndirectCommandsTokenNVX##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkIndirectCommandsTokenNVX where
        type StructFields VkIndirectCommandsTokenNVX =
             '["tokenType", "buffer", "offset"] -- ' closing tick for hsc2hs
        type CUnionType VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs
        type StructExtends VkIndirectCommandsTokenNVX = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "tokenType" VkIndirectCommandsTokenNVX where
        type FieldType "tokenType" VkIndirectCommandsTokenNVX =
             VkIndirectCommandsTokenTypeNVX
        type FieldOptional "tokenType" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "tokenType" VkIndirectCommandsTokenNVX =
             #{offset VkIndirectCommandsTokenNVX, tokenType}
        type FieldIsArray "tokenType" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsTokenNVX, tokenType}

instance {-# OVERLAPPING #-}
         CanReadField "tokenType" VkIndirectCommandsTokenNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, tokenType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, tokenType}

instance {-# OVERLAPPING #-}
         CanWriteField "tokenType" VkIndirectCommandsTokenNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, tokenType}

instance {-# OVERLAPPING #-}
         HasField "buffer" VkIndirectCommandsTokenNVX where
        type FieldType "buffer" VkIndirectCommandsTokenNVX = VkBuffer
        type FieldOptional "buffer" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "buffer" VkIndirectCommandsTokenNVX =
             #{offset VkIndirectCommandsTokenNVX, buffer}
        type FieldIsArray "buffer" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsTokenNVX, buffer}

instance {-# OVERLAPPING #-}
         CanReadField "buffer" VkIndirectCommandsTokenNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, buffer})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, buffer}

instance {-# OVERLAPPING #-}
         CanWriteField "buffer" VkIndirectCommandsTokenNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, buffer}

instance {-# OVERLAPPING #-}
         HasField "offset" VkIndirectCommandsTokenNVX where
        type FieldType "offset" VkIndirectCommandsTokenNVX = VkDeviceSize
        type FieldOptional "offset" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs
        type FieldOffset "offset" VkIndirectCommandsTokenNVX =
             #{offset VkIndirectCommandsTokenNVX, offset}
        type FieldIsArray "offset" VkIndirectCommandsTokenNVX = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkIndirectCommandsTokenNVX, offset}

instance {-# OVERLAPPING #-}
         CanReadField "offset" VkIndirectCommandsTokenNVX where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsTokenNVX, offset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkIndirectCommandsTokenNVX, offset}

instance {-# OVERLAPPING #-}
         CanWriteField "offset" VkIndirectCommandsTokenNVX where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkIndirectCommandsTokenNVX, offset}

instance Show VkIndirectCommandsTokenNVX where
        showsPrec d x
          = showString "VkIndirectCommandsTokenNVX {" .
              showString "tokenType = " .
                showsPrec d (getField @"tokenType" x) .
                  showString ", " .
                    showString "buffer = " .
                      showsPrec d (getField @"buffer" x) .
                        showString ", " .
                          showString "offset = " .
                            showsPrec d (getField @"offset" x) . showChar '}'
