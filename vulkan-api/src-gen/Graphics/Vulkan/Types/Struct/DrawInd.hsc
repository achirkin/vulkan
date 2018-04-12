#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DrawInd
       (VkDrawIndexedIndirectCommand(..), VkDrawIndirectCommand(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDrawIndexedIndirectCommand {
--   >     uint32_t               indexCount;
--   >     uint32_t               instanceCount;
--   >     uint32_t               firstIndex;
--   >     int32_t                vertexOffset;
--   >     uint32_t               firstInstance;
--   > } VkDrawIndexedIndirectCommand;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDrawIndexedIndirectCommand VkDrawIndexedIndirectCommand registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "indexCount" VkDrawIndexedIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndexedIndirectCommand, indexCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDrawIndexedIndirectCommand, indexCount}

instance {-# OVERLAPPING #-}
         CanWriteField "indexCount" VkDrawIndexedIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDrawIndexedIndirectCommand, indexCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "instanceCount" VkDrawIndexedIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndexedIndirectCommand, instanceCount})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDrawIndexedIndirectCommand, instanceCount}

instance {-# OVERLAPPING #-}
         CanWriteField "instanceCount" VkDrawIndexedIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDrawIndexedIndirectCommand, instanceCount}

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

instance {-# OVERLAPPING #-}
         CanReadField "firstIndex" VkDrawIndexedIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndexedIndirectCommand, firstIndex})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDrawIndexedIndirectCommand, firstIndex}

instance {-# OVERLAPPING #-}
         CanWriteField "firstIndex" VkDrawIndexedIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDrawIndexedIndirectCommand, firstIndex}

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

instance {-# OVERLAPPING #-}
         CanReadField "vertexOffset" VkDrawIndexedIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndexedIndirectCommand, vertexOffset})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDrawIndexedIndirectCommand, vertexOffset}

instance {-# OVERLAPPING #-}
         CanWriteField "vertexOffset" VkDrawIndexedIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDrawIndexedIndirectCommand, vertexOffset}

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

instance {-# OVERLAPPING #-}
         CanReadField "firstInstance" VkDrawIndexedIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDrawIndexedIndirectCommand, firstInstance})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDrawIndexedIndirectCommand, firstInstance}

instance {-# OVERLAPPING #-}
         CanWriteField "firstInstance" VkDrawIndexedIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDrawIndexedIndirectCommand, firstInstance}

instance Show VkDrawIndexedIndirectCommand where
        showsPrec d x
          = showString "VkDrawIndexedIndirectCommand {" .
              showString "indexCount = " .
                showsPrec d (getField @"indexCount" x) .
                  showString ", " .
                    showString "instanceCount = " .
                      showsPrec d (getField @"instanceCount" x) .
                        showString ", " .
                          showString "firstIndex = " .
                            showsPrec d (getField @"firstIndex" x) .
                              showString ", " .
                                showString "vertexOffset = " .
                                  showsPrec d (getField @"vertexOffset" x) .
                                    showString ", " .
                                      showString "firstInstance = " .
                                        showsPrec d (getField @"firstInstance" x) . showChar '}'

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
