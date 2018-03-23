#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkIndirectCommandsLayoutCreateInfoNVX
       (VkIndirectCommandsLayoutCreateInfoNVX(..)) where
import           Foreign.Storable
                                                                                   (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkIndirectCommandsLayoutUsageFlagsNVX
                                                                                   (VkIndirectCommandsLayoutUsageFlagsNVX)
import           Graphics.Vulkan.Types.Enum.VkPipelineBindPoint
                                                                                   (VkPipelineBindPoint)
import           Graphics.Vulkan.Types.Enum.VkStructureType
                                                                                   (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkIndirectCommandsLayoutTokenNVX
                                                                                   (VkIndirectCommandsLayoutTokenNVX)
import           System.IO.Unsafe
                                                                                   (unsafeDupablePerformIO)

-- | > typedef struct VkIndirectCommandsLayoutCreateInfoNVX {
--   >     VkStructureType sType;
--   >     const void*                      pNext;
--   >     VkPipelineBindPoint                      pipelineBindPoint;
--   >     VkIndirectCommandsLayoutUsageFlagsNVX    flags;
--   >     uint32_t                                 tokenCount;
--   >     const VkIndirectCommandsLayoutTokenNVX*  pTokens;
--   > } VkIndirectCommandsLayoutCreateInfoNVX;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkIndirectCommandsLayoutCreateInfoNVX.html VkIndirectCommandsLayoutCreateInfoNVX registry at www.khronos.org>
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
