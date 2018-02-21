#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDrawIndirectCommand
       (VkDrawIndirectCommand(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkDrawIndirectCommand {
--   >     uint32_t               vertexCount;
--   >     uint32_t               instanceCount;
--   >     uint32_t               firstVertex;
--   >     uint32_t               firstInstance;
--   > } VkDrawIndirectCommand;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDrawIndirectCommand.html VkDrawIndirectCommand registry at www.khronos.org>
data VkDrawIndirectCommand = VkDrawIndirectCommand## Addr##
                                                    ByteArray##

instance Eq VkDrawIndirectCommand where
        (VkDrawIndirectCommand## a _) == x@(VkDrawIndirectCommand## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDrawIndirectCommand where
        (VkDrawIndirectCommand## a _) `compare`
          x@(VkDrawIndirectCommand## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDrawIndirectCommand where
        sizeOf ~_ = #{size VkDrawIndirectCommand}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDrawIndirectCommand}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDrawIndirectCommand where
        unsafeAddr (VkDrawIndirectCommand## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDrawIndirectCommand## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDrawIndirectCommand## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDrawIndirectCommand where
        type StructFields VkDrawIndirectCommand =
             '["vertexCount", "instanceCount", "firstVertex", "firstInstance"] -- ' closing tick for hsc2hs
        type CUnionType VkDrawIndirectCommand = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDrawIndirectCommand = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDrawIndirectCommand = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkVertexCount VkDrawIndirectCommand
         where
        type VkVertexCountMType VkDrawIndirectCommand = Word32

        {-# NOINLINE vkVertexCount #-}
        vkVertexCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndirectCommand, vertexCount})

        {-# INLINE vkVertexCountByteOffset #-}
        vkVertexCountByteOffset ~_
          = #{offset VkDrawIndirectCommand, vertexCount}

        {-# INLINE readVkVertexCount #-}
        readVkVertexCount p
          = peekByteOff p #{offset VkDrawIndirectCommand, vertexCount}

        {-# INLINE writeVkVertexCount #-}
        writeVkVertexCount p
          = pokeByteOff p #{offset VkDrawIndirectCommand, vertexCount}

instance {-# OVERLAPPING #-}
         HasField "vertexCount" VkDrawIndirectCommand where
        type FieldType "vertexCount" VkDrawIndirectCommand = Word32
        type FieldOptional "vertexCount" VkDrawIndirectCommand = 'False -- ' closing tick for hsc2hs
        type FieldOffset "vertexCount" VkDrawIndirectCommand =
             #{offset VkDrawIndirectCommand, vertexCount}
        type FieldIsArray "vertexCount" VkDrawIndirectCommand = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDrawIndirectCommand, vertexCount}

instance CanReadField "vertexCount" VkDrawIndirectCommand where
        {-# INLINE getField #-}
        getField = vkVertexCount

        {-# INLINE readField #-}
        readField = readVkVertexCount

instance CanWriteField "vertexCount" VkDrawIndirectCommand where
        {-# INLINE writeField #-}
        writeField = writeVkVertexCount

instance {-# OVERLAPPING #-}
         HasVkInstanceCount VkDrawIndirectCommand where
        type VkInstanceCountMType VkDrawIndirectCommand = Word32

        {-# NOINLINE vkInstanceCount #-}
        vkInstanceCount x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndirectCommand, instanceCount})

        {-# INLINE vkInstanceCountByteOffset #-}
        vkInstanceCountByteOffset ~_
          = #{offset VkDrawIndirectCommand, instanceCount}

        {-# INLINE readVkInstanceCount #-}
        readVkInstanceCount p
          = peekByteOff p #{offset VkDrawIndirectCommand, instanceCount}

        {-# INLINE writeVkInstanceCount #-}
        writeVkInstanceCount p
          = pokeByteOff p #{offset VkDrawIndirectCommand, instanceCount}

instance {-# OVERLAPPING #-}
         HasField "instanceCount" VkDrawIndirectCommand where
        type FieldType "instanceCount" VkDrawIndirectCommand = Word32
        type FieldOptional "instanceCount" VkDrawIndirectCommand = 'False -- ' closing tick for hsc2hs
        type FieldOffset "instanceCount" VkDrawIndirectCommand =
             #{offset VkDrawIndirectCommand, instanceCount}
        type FieldIsArray "instanceCount" VkDrawIndirectCommand = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDrawIndirectCommand, instanceCount}

instance CanReadField "instanceCount" VkDrawIndirectCommand where
        {-# INLINE getField #-}
        getField = vkInstanceCount

        {-# INLINE readField #-}
        readField = readVkInstanceCount

instance CanWriteField "instanceCount" VkDrawIndirectCommand where
        {-# INLINE writeField #-}
        writeField = writeVkInstanceCount

instance {-# OVERLAPPING #-} HasVkFirstVertex VkDrawIndirectCommand
         where
        type VkFirstVertexMType VkDrawIndirectCommand = Word32

        {-# NOINLINE vkFirstVertex #-}
        vkFirstVertex x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndirectCommand, firstVertex})

        {-# INLINE vkFirstVertexByteOffset #-}
        vkFirstVertexByteOffset ~_
          = #{offset VkDrawIndirectCommand, firstVertex}

        {-# INLINE readVkFirstVertex #-}
        readVkFirstVertex p
          = peekByteOff p #{offset VkDrawIndirectCommand, firstVertex}

        {-# INLINE writeVkFirstVertex #-}
        writeVkFirstVertex p
          = pokeByteOff p #{offset VkDrawIndirectCommand, firstVertex}

instance {-# OVERLAPPING #-}
         HasField "firstVertex" VkDrawIndirectCommand where
        type FieldType "firstVertex" VkDrawIndirectCommand = Word32
        type FieldOptional "firstVertex" VkDrawIndirectCommand = 'False -- ' closing tick for hsc2hs
        type FieldOffset "firstVertex" VkDrawIndirectCommand =
             #{offset VkDrawIndirectCommand, firstVertex}
        type FieldIsArray "firstVertex" VkDrawIndirectCommand = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDrawIndirectCommand, firstVertex}

instance CanReadField "firstVertex" VkDrawIndirectCommand where
        {-# INLINE getField #-}
        getField = vkFirstVertex

        {-# INLINE readField #-}
        readField = readVkFirstVertex

instance CanWriteField "firstVertex" VkDrawIndirectCommand where
        {-# INLINE writeField #-}
        writeField = writeVkFirstVertex

instance {-# OVERLAPPING #-}
         HasVkFirstInstance VkDrawIndirectCommand where
        type VkFirstInstanceMType VkDrawIndirectCommand = Word32

        {-# NOINLINE vkFirstInstance #-}
        vkFirstInstance x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndirectCommand, firstInstance})

        {-# INLINE vkFirstInstanceByteOffset #-}
        vkFirstInstanceByteOffset ~_
          = #{offset VkDrawIndirectCommand, firstInstance}

        {-# INLINE readVkFirstInstance #-}
        readVkFirstInstance p
          = peekByteOff p #{offset VkDrawIndirectCommand, firstInstance}

        {-# INLINE writeVkFirstInstance #-}
        writeVkFirstInstance p
          = pokeByteOff p #{offset VkDrawIndirectCommand, firstInstance}

instance {-# OVERLAPPING #-}
         HasField "firstInstance" VkDrawIndirectCommand where
        type FieldType "firstInstance" VkDrawIndirectCommand = Word32
        type FieldOptional "firstInstance" VkDrawIndirectCommand = 'False -- ' closing tick for hsc2hs
        type FieldOffset "firstInstance" VkDrawIndirectCommand =
             #{offset VkDrawIndirectCommand, firstInstance}
        type FieldIsArray "firstInstance" VkDrawIndirectCommand = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkDrawIndirectCommand, firstInstance}

instance CanReadField "firstInstance" VkDrawIndirectCommand where
        {-# INLINE getField #-}
        getField = vkFirstInstance

        {-# INLINE readField #-}
        readField = readVkFirstInstance

instance CanWriteField "firstInstance" VkDrawIndirectCommand where
        {-# INLINE writeField #-}
        writeField = writeVkFirstInstance

instance Show VkDrawIndirectCommand where
        showsPrec d x
          = showString "VkDrawIndirectCommand {" .
              showString "vkVertexCount = " .
                showsPrec d (vkVertexCount x) .
                  showString ", " .
                    showString "vkInstanceCount = " .
                      showsPrec d (vkInstanceCount x) .
                        showString ", " .
                          showString "vkFirstVertex = " .
                            showsPrec d (vkFirstVertex x) .
                              showString ", " .
                                showString "vkFirstInstance = " .
                                  showsPrec d (vkFirstInstance x) . showChar '}'
