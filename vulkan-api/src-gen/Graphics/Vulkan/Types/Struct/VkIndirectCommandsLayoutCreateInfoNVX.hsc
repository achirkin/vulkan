#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
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
import           Graphics.Vulkan.Types.StructMembers
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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkIndirectCommandsLayoutCreateInfoNVX.html VkIndirectCommandsLayoutCreateInfoNVX registry at www.khronos.org>
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
         HasVkSType VkIndirectCommandsLayoutCreateInfoNVX where
        type VkSTypeMType VkIndirectCommandsLayoutCreateInfoNVX =
             VkStructureType

        {-# NOINLINE vkSType #-}
        vkSType x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType})

        {-# INLINE vkSTypeByteOffset #-}
        vkSTypeByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}

        {-# INLINE readVkSType #-}
        readVkSType p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}

        {-# INLINE writeVkSType #-}
        writeVkSType p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, sType}

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

instance CanReadField "sType" VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkSType

        {-# INLINE readField #-}
        readField = readVkSType

instance CanWriteField "sType"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkSType

instance {-# OVERLAPPING #-}
         HasVkPNext VkIndirectCommandsLayoutCreateInfoNVX where
        type VkPNextMType VkIndirectCommandsLayoutCreateInfoNVX = Ptr Void

        {-# NOINLINE vkPNext #-}
        vkPNext x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext})

        {-# INLINE vkPNextByteOffset #-}
        vkPNextByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}

        {-# INLINE readVkPNext #-}
        readVkPNext p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}

        {-# INLINE writeVkPNext #-}
        writeVkPNext p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pNext}

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

instance CanReadField "pNext" VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkPNext

        {-# INLINE readField #-}
        readField = readVkPNext

instance CanWriteField "pNext"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPNext

instance {-# OVERLAPPING #-}
         HasVkPipelineBindPoint VkIndirectCommandsLayoutCreateInfoNVX where
        type VkPipelineBindPointMType VkIndirectCommandsLayoutCreateInfoNVX
             = VkPipelineBindPoint

        {-# NOINLINE vkPipelineBindPoint #-}
        vkPipelineBindPoint x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint})

        {-# INLINE vkPipelineBindPointByteOffset #-}
        vkPipelineBindPointByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}

        {-# INLINE readVkPipelineBindPoint #-}
        readVkPipelineBindPoint p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}

        {-# INLINE writeVkPipelineBindPoint #-}
        writeVkPipelineBindPoint p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pipelineBindPoint}

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

instance CanReadField "pipelineBindPoint"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkPipelineBindPoint

        {-# INLINE readField #-}
        readField = readVkPipelineBindPoint

instance CanWriteField "pipelineBindPoint"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPipelineBindPoint

instance {-# OVERLAPPING #-}
         HasVkFlags VkIndirectCommandsLayoutCreateInfoNVX where
        type VkFlagsMType VkIndirectCommandsLayoutCreateInfoNVX =
             VkIndirectCommandsLayoutUsageFlagsNVX

        {-# NOINLINE vkFlags #-}
        vkFlags x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags})

        {-# INLINE vkFlagsByteOffset #-}
        vkFlagsByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}

        {-# INLINE readVkFlags #-}
        readVkFlags p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}

        {-# INLINE writeVkFlags #-}
        writeVkFlags p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, flags}

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

instance CanReadField "flags" VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkFlags

        {-# INLINE readField #-}
        readField = readVkFlags

instance CanWriteField "flags"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkFlags

instance {-# OVERLAPPING #-}
         HasVkTokenCount VkIndirectCommandsLayoutCreateInfoNVX where
        type VkTokenCountMType VkIndirectCommandsLayoutCreateInfoNVX =
             Word32

        {-# NOINLINE vkTokenCount #-}
        vkTokenCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount})

        {-# INLINE vkTokenCountByteOffset #-}
        vkTokenCountByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}

        {-# INLINE readVkTokenCount #-}
        readVkTokenCount p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}

        {-# INLINE writeVkTokenCount #-}
        writeVkTokenCount p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, tokenCount}

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

instance CanReadField "tokenCount"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkTokenCount

        {-# INLINE readField #-}
        readField = readVkTokenCount

instance CanWriteField "tokenCount"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkTokenCount

instance {-# OVERLAPPING #-}
         HasVkPTokens VkIndirectCommandsLayoutCreateInfoNVX where
        type VkPTokensMType VkIndirectCommandsLayoutCreateInfoNVX =
             Ptr VkIndirectCommandsLayoutTokenNVX

        {-# NOINLINE vkPTokens #-}
        vkPTokens x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens})

        {-# INLINE vkPTokensByteOffset #-}
        vkPTokensByteOffset ~_
          = #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}

        {-# INLINE readVkPTokens #-}
        readVkPTokens p
          = peekByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}

        {-# INLINE writeVkPTokens #-}
        writeVkPTokens p
          = pokeByteOff p #{offset VkIndirectCommandsLayoutCreateInfoNVX, pTokens}

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

instance CanReadField "pTokens"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE getField #-}
        getField = vkPTokens

        {-# INLINE readField #-}
        readField = readVkPTokens

instance CanWriteField "pTokens"
           VkIndirectCommandsLayoutCreateInfoNVX
         where
        {-# INLINE writeField #-}
        writeField = writeVkPTokens

instance Show VkIndirectCommandsLayoutCreateInfoNVX where
        showsPrec d x
          = showString "VkIndirectCommandsLayoutCreateInfoNVX {" .
              showString "vkSType = " .
                showsPrec d (vkSType x) .
                  showString ", " .
                    showString "vkPNext = " .
                      showsPrec d (vkPNext x) .
                        showString ", " .
                          showString "vkPipelineBindPoint = " .
                            showsPrec d (vkPipelineBindPoint x) .
                              showString ", " .
                                showString "vkFlags = " .
                                  showsPrec d (vkFlags x) .
                                    showString ", " .
                                      showString "vkTokenCount = " .
                                        showsPrec d (vkTokenCount x) .
                                          showString ", " .
                                            showString "vkPTokens = " .
                                              showsPrec d (vkPTokens x) . showChar '}'
