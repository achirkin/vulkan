#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDrawIndirectCommand
       (VkDrawIndirectCommand(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDrawIndirectCommand {
--   >     uint32_t               vertexCount;
--   >     uint32_t               instanceCount;
--   >     uint32_t               firstVertex;
--   >     uint32_t               firstInstance;
--   > } VkDrawIndirectCommand;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDrawIndirectCommand VkDrawIndirectCommand registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "vertexCount" VkDrawIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndirectCommand, vertexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDrawIndirectCommand, vertexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "vertexCount" VkDrawIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDrawIndirectCommand, vertexCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "instanceCount" VkDrawIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndirectCommand, instanceCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDrawIndirectCommand, instanceCount}

instance {-# OVERLAPPING #-}
         CanWriteField "instanceCount" VkDrawIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDrawIndirectCommand, instanceCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "firstVertex" VkDrawIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndirectCommand, firstVertex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDrawIndirectCommand, firstVertex}

instance {-# OVERLAPPING #-}
         CanWriteField "firstVertex" VkDrawIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDrawIndirectCommand, firstVertex}

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

instance {-# OVERLAPPING #-}
         CanReadField "firstInstance" VkDrawIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndirectCommand, firstInstance})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDrawIndirectCommand, firstInstance}

instance {-# OVERLAPPING #-}
         CanWriteField "firstInstance" VkDrawIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDrawIndirectCommand, firstInstance}

instance Show VkDrawIndirectCommand where
        showsPrec d x
          = showString "VkDrawIndirectCommand {" .
              showString "vertexCount = " .
                showsPrec d (getField @"vertexCount" x) .
                  showString ", " .
                    showString "instanceCount = " .
                      showsPrec d (getField @"instanceCount" x) .
                        showString ", " .
                          showString "firstVertex = " .
                            showsPrec d (getField @"firstVertex" x) .
                              showString ", " .
                                showString "firstInstance = " .
                                  showsPrec d (getField @"firstInstance" x) . showChar '}'
