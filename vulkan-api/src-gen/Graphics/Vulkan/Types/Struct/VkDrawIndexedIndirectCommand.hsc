#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDrawIndexedIndirectCommand
       (VkDrawIndexedIndirectCommand(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Prim
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
