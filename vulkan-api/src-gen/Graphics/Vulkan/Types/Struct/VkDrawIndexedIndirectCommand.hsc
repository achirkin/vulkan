#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDrawIndexedIndirectCommand
       (VkDrawIndexedIndirectCommand(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkDrawIndexedIndirectCommand {
--   >     uint32_t               indexCount;
--   >     uint32_t               instanceCount;
--   >     uint32_t               firstIndex;
--   >     int32_t                vertexOffset;
--   >     uint32_t               firstInstance;
--   > } VkDrawIndexedIndirectCommand;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDrawIndexedIndirectCommand.html VkDrawIndexedIndirectCommand registry at www.khronos.org>
data VkDrawIndexedIndirectCommand = VkDrawIndexedIndirectCommand## Addr##
                                                                  ByteArray##

instance Eq VkDrawIndexedIndirectCommand where
        (VkDrawIndexedIndirectCommand## a _) ==
          x@(VkDrawIndexedIndirectCommand## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDrawIndexedIndirectCommand where
        (VkDrawIndexedIndirectCommand## a _) `compare`
          x@(VkDrawIndexedIndirectCommand## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDrawIndexedIndirectCommand where
        sizeOf ~_ = #{size VkDrawIndexedIndirectCommand}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkDrawIndexedIndirectCommand}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDrawIndexedIndirectCommand where
        unsafeAddr (VkDrawIndexedIndirectCommand## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDrawIndexedIndirectCommand## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDrawIndexedIndirectCommand##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDrawIndexedIndirectCommand where
        type StructFields VkDrawIndexedIndirectCommand =
             '["indexCount", "instanceCount", "firstIndex", "vertexOffset", -- ' closing tick for hsc2hs
               "firstInstance"]
        type CUnionType VkDrawIndexedIndirectCommand = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDrawIndexedIndirectCommand = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDrawIndexedIndirectCommand = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkIndexCount VkDrawIndexedIndirectCommand where
        type VkIndexCountMType VkDrawIndexedIndirectCommand = Word32

        {-# NOINLINE vkIndexCount #-}
        vkIndexCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndexedIndirectCommand, indexCount})

        {-# INLINE vkIndexCountByteOffset #-}
        vkIndexCountByteOffset ~_
          = #{offset VkDrawIndexedIndirectCommand, indexCount}

        {-# INLINE readVkIndexCount #-}
        readVkIndexCount p
          = peekByteOff p #{offset VkDrawIndexedIndirectCommand, indexCount}

        {-# INLINE writeVkIndexCount #-}
        writeVkIndexCount p
          = pokeByteOff p #{offset VkDrawIndexedIndirectCommand, indexCount}

instance {-# OVERLAPPING #-}
         HasField "indexCount" VkDrawIndexedIndirectCommand where
        type FieldType "indexCount" VkDrawIndexedIndirectCommand = Word32
        type FieldOptional "indexCount" VkDrawIndexedIndirectCommand =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "indexCount" VkDrawIndexedIndirectCommand =
             #{offset VkDrawIndexedIndirectCommand, indexCount}
        type FieldIsArray "indexCount" VkDrawIndexedIndirectCommand =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDrawIndexedIndirectCommand, indexCount}

instance CanReadField "indexCount" VkDrawIndexedIndirectCommand
         where
        {-# INLINE getField #-}
        getField = vkIndexCount

        {-# INLINE readField #-}
        readField = readVkIndexCount

instance CanWriteField "indexCount" VkDrawIndexedIndirectCommand
         where
        {-# INLINE writeField #-}
        writeField = writeVkIndexCount

instance {-# OVERLAPPING #-}
         HasVkInstanceCount VkDrawIndexedIndirectCommand where
        type VkInstanceCountMType VkDrawIndexedIndirectCommand = Word32

        {-# NOINLINE vkInstanceCount #-}
        vkInstanceCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndexedIndirectCommand, instanceCount})

        {-# INLINE vkInstanceCountByteOffset #-}
        vkInstanceCountByteOffset ~_
          = #{offset VkDrawIndexedIndirectCommand, instanceCount}

        {-# INLINE readVkInstanceCount #-}
        readVkInstanceCount p
          = peekByteOff p #{offset VkDrawIndexedIndirectCommand, instanceCount}

        {-# INLINE writeVkInstanceCount #-}
        writeVkInstanceCount p
          = pokeByteOff p #{offset VkDrawIndexedIndirectCommand, instanceCount}

instance {-# OVERLAPPING #-}
         HasField "instanceCount" VkDrawIndexedIndirectCommand where
        type FieldType "instanceCount" VkDrawIndexedIndirectCommand =
             Word32
        type FieldOptional "instanceCount" VkDrawIndexedIndirectCommand =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "instanceCount" VkDrawIndexedIndirectCommand =
             #{offset VkDrawIndexedIndirectCommand, instanceCount}
        type FieldIsArray "instanceCount" VkDrawIndexedIndirectCommand =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDrawIndexedIndirectCommand, instanceCount}

instance CanReadField "instanceCount" VkDrawIndexedIndirectCommand
         where
        {-# INLINE getField #-}
        getField = vkInstanceCount

        {-# INLINE readField #-}
        readField = readVkInstanceCount

instance CanWriteField "instanceCount" VkDrawIndexedIndirectCommand
         where
        {-# INLINE writeField #-}
        writeField = writeVkInstanceCount

instance {-# OVERLAPPING #-}
         HasVkFirstIndex VkDrawIndexedIndirectCommand where
        type VkFirstIndexMType VkDrawIndexedIndirectCommand = Word32

        {-# NOINLINE vkFirstIndex #-}
        vkFirstIndex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndexedIndirectCommand, firstIndex})

        {-# INLINE vkFirstIndexByteOffset #-}
        vkFirstIndexByteOffset ~_
          = #{offset VkDrawIndexedIndirectCommand, firstIndex}

        {-# INLINE readVkFirstIndex #-}
        readVkFirstIndex p
          = peekByteOff p #{offset VkDrawIndexedIndirectCommand, firstIndex}

        {-# INLINE writeVkFirstIndex #-}
        writeVkFirstIndex p
          = pokeByteOff p #{offset VkDrawIndexedIndirectCommand, firstIndex}

instance {-# OVERLAPPING #-}
         HasField "firstIndex" VkDrawIndexedIndirectCommand where
        type FieldType "firstIndex" VkDrawIndexedIndirectCommand = Word32
        type FieldOptional "firstIndex" VkDrawIndexedIndirectCommand =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "firstIndex" VkDrawIndexedIndirectCommand =
             #{offset VkDrawIndexedIndirectCommand, firstIndex}
        type FieldIsArray "firstIndex" VkDrawIndexedIndirectCommand =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDrawIndexedIndirectCommand, firstIndex}

instance CanReadField "firstIndex" VkDrawIndexedIndirectCommand
         where
        {-# INLINE getField #-}
        getField = vkFirstIndex

        {-# INLINE readField #-}
        readField = readVkFirstIndex

instance CanWriteField "firstIndex" VkDrawIndexedIndirectCommand
         where
        {-# INLINE writeField #-}
        writeField = writeVkFirstIndex

instance {-# OVERLAPPING #-}
         HasVkVertexOffset VkDrawIndexedIndirectCommand where
        type VkVertexOffsetMType VkDrawIndexedIndirectCommand = Int32

        {-# NOINLINE vkVertexOffset #-}
        vkVertexOffset x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndexedIndirectCommand, vertexOffset})

        {-# INLINE vkVertexOffsetByteOffset #-}
        vkVertexOffsetByteOffset ~_
          = #{offset VkDrawIndexedIndirectCommand, vertexOffset}

        {-# INLINE readVkVertexOffset #-}
        readVkVertexOffset p
          = peekByteOff p #{offset VkDrawIndexedIndirectCommand, vertexOffset}

        {-# INLINE writeVkVertexOffset #-}
        writeVkVertexOffset p
          = pokeByteOff p #{offset VkDrawIndexedIndirectCommand, vertexOffset}

instance {-# OVERLAPPING #-}
         HasField "vertexOffset" VkDrawIndexedIndirectCommand where
        type FieldType "vertexOffset" VkDrawIndexedIndirectCommand = Int32
        type FieldOptional "vertexOffset" VkDrawIndexedIndirectCommand =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "vertexOffset" VkDrawIndexedIndirectCommand =
             #{offset VkDrawIndexedIndirectCommand, vertexOffset}
        type FieldIsArray "vertexOffset" VkDrawIndexedIndirectCommand =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDrawIndexedIndirectCommand, vertexOffset}

instance CanReadField "vertexOffset" VkDrawIndexedIndirectCommand
         where
        {-# INLINE getField #-}
        getField = vkVertexOffset

        {-# INLINE readField #-}
        readField = readVkVertexOffset

instance CanWriteField "vertexOffset" VkDrawIndexedIndirectCommand
         where
        {-# INLINE writeField #-}
        writeField = writeVkVertexOffset

instance {-# OVERLAPPING #-}
         HasVkFirstInstance VkDrawIndexedIndirectCommand where
        type VkFirstInstanceMType VkDrawIndexedIndirectCommand = Word32

        {-# NOINLINE vkFirstInstance #-}
        vkFirstInstance x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndexedIndirectCommand, firstInstance})

        {-# INLINE vkFirstInstanceByteOffset #-}
        vkFirstInstanceByteOffset ~_
          = #{offset VkDrawIndexedIndirectCommand, firstInstance}

        {-# INLINE readVkFirstInstance #-}
        readVkFirstInstance p
          = peekByteOff p #{offset VkDrawIndexedIndirectCommand, firstInstance}

        {-# INLINE writeVkFirstInstance #-}
        writeVkFirstInstance p
          = pokeByteOff p #{offset VkDrawIndexedIndirectCommand, firstInstance}

instance {-# OVERLAPPING #-}
         HasField "firstInstance" VkDrawIndexedIndirectCommand where
        type FieldType "firstInstance" VkDrawIndexedIndirectCommand =
             Word32
        type FieldOptional "firstInstance" VkDrawIndexedIndirectCommand =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "firstInstance" VkDrawIndexedIndirectCommand =
             #{offset VkDrawIndexedIndirectCommand, firstInstance}
        type FieldIsArray "firstInstance" VkDrawIndexedIndirectCommand =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDrawIndexedIndirectCommand, firstInstance}

instance CanReadField "firstInstance" VkDrawIndexedIndirectCommand
         where
        {-# INLINE getField #-}
        getField = vkFirstInstance

        {-# INLINE readField #-}
        readField = readVkFirstInstance

instance CanWriteField "firstInstance" VkDrawIndexedIndirectCommand
         where
        {-# INLINE writeField #-}
        writeField = writeVkFirstInstance

instance Show VkDrawIndexedIndirectCommand where
        showsPrec d x
          = showString "VkDrawIndexedIndirectCommand {" .
              showString "vkIndexCount = " .
                showsPrec d (vkIndexCount x) .
                  showString ", " .
                    showString "vkInstanceCount = " .
                      showsPrec d (vkInstanceCount x) .
                        showString ", " .
                          showString "vkFirstIndex = " .
                            showsPrec d (vkFirstIndex x) .
                              showString ", " .
                                showString "vkVertexOffset = " .
                                  showsPrec d (vkVertexOffset x) .
                                    showString ", " .
                                      showString "vkFirstInstance = " .
                                        showsPrec d (vkFirstInstance x) . showChar '}'
