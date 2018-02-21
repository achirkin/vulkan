#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkDispatchIndirectCommand
       (VkDispatchIndirectCommand(..)) where
import           Foreign.Storable                    (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                    (unsafeDupablePerformIO)

-- | > typedef struct VkDispatchIndirectCommand {
--   >     uint32_t               x;
--   >     uint32_t               y;
--   >     uint32_t               z;
--   > } VkDispatchIndirectCommand;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkDispatchIndirectCommand.html VkDispatchIndirectCommand registry at www.khronos.org>
data VkDispatchIndirectCommand = VkDispatchIndirectCommand## Addr##
                                                            ByteArray##

instance Eq VkDispatchIndirectCommand where
        (VkDispatchIndirectCommand## a _) ==
          x@(VkDispatchIndirectCommand## b _) = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkDispatchIndirectCommand where
        (VkDispatchIndirectCommand## a _) `compare`
          x@(VkDispatchIndirectCommand## b _) = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkDispatchIndirectCommand where
        sizeOf ~_ = #{size VkDispatchIndirectCommand}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkDispatchIndirectCommand}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkDispatchIndirectCommand where
        unsafeAddr (VkDispatchIndirectCommand## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkDispatchIndirectCommand## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkDispatchIndirectCommand## (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkDispatchIndirectCommand where
        type StructFields VkDispatchIndirectCommand = '["x", "y", "z"] -- ' closing tick for hsc2hs
        type CUnionType VkDispatchIndirectCommand = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkDispatchIndirectCommand = 'False -- ' closing tick for hsc2hs
        type StructExtends VkDispatchIndirectCommand = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasVkX VkDispatchIndirectCommand where
        type VkXMType VkDispatchIndirectCommand = Word32

        {-# NOINLINE vkX #-}
        vkX x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDispatchIndirectCommand, x})

        {-# INLINE vkXByteOffset #-}
        vkXByteOffset ~_
          = #{offset VkDispatchIndirectCommand, x}

        {-# INLINE readVkX #-}
        readVkX p
          = peekByteOff p #{offset VkDispatchIndirectCommand, x}

        {-# INLINE writeVkX #-}
        writeVkX p
          = pokeByteOff p #{offset VkDispatchIndirectCommand, x}

instance {-# OVERLAPPING #-} HasField "x" VkDispatchIndirectCommand
         where
        type FieldType "x" VkDispatchIndirectCommand = Word32
        type FieldOptional "x" VkDispatchIndirectCommand = 'False -- ' closing tick for hsc2hs
        type FieldOffset "x" VkDispatchIndirectCommand =
             #{offset VkDispatchIndirectCommand, x}
        type FieldIsArray "x" VkDispatchIndirectCommand = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDispatchIndirectCommand, x}

instance CanReadField "x" VkDispatchIndirectCommand where
        {-# INLINE getField #-}
        getField = vkX

        {-# INLINE readField #-}
        readField = readVkX

instance CanWriteField "x" VkDispatchIndirectCommand where
        {-# INLINE writeField #-}
        writeField = writeVkX

instance {-# OVERLAPPING #-} HasVkY VkDispatchIndirectCommand where
        type VkYMType VkDispatchIndirectCommand = Word32

        {-# NOINLINE vkY #-}
        vkY x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDispatchIndirectCommand, y})

        {-# INLINE vkYByteOffset #-}
        vkYByteOffset ~_
          = #{offset VkDispatchIndirectCommand, y}

        {-# INLINE readVkY #-}
        readVkY p
          = peekByteOff p #{offset VkDispatchIndirectCommand, y}

        {-# INLINE writeVkY #-}
        writeVkY p
          = pokeByteOff p #{offset VkDispatchIndirectCommand, y}

instance {-# OVERLAPPING #-} HasField "y" VkDispatchIndirectCommand
         where
        type FieldType "y" VkDispatchIndirectCommand = Word32
        type FieldOptional "y" VkDispatchIndirectCommand = 'False -- ' closing tick for hsc2hs
        type FieldOffset "y" VkDispatchIndirectCommand =
             #{offset VkDispatchIndirectCommand, y}
        type FieldIsArray "y" VkDispatchIndirectCommand = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDispatchIndirectCommand, y}

instance CanReadField "y" VkDispatchIndirectCommand where
        {-# INLINE getField #-}
        getField = vkY

        {-# INLINE readField #-}
        readField = readVkY

instance CanWriteField "y" VkDispatchIndirectCommand where
        {-# INLINE writeField #-}
        writeField = writeVkY

instance {-# OVERLAPPING #-} HasVkZ VkDispatchIndirectCommand where
        type VkZMType VkDispatchIndirectCommand = Word32

        {-# NOINLINE vkZ #-}
        vkZ x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDispatchIndirectCommand, z})

        {-# INLINE vkZByteOffset #-}
        vkZByteOffset ~_
          = #{offset VkDispatchIndirectCommand, z}

        {-# INLINE readVkZ #-}
        readVkZ p
          = peekByteOff p #{offset VkDispatchIndirectCommand, z}

        {-# INLINE writeVkZ #-}
        writeVkZ p
          = pokeByteOff p #{offset VkDispatchIndirectCommand, z}

instance {-# OVERLAPPING #-} HasField "z" VkDispatchIndirectCommand
         where
        type FieldType "z" VkDispatchIndirectCommand = Word32
        type FieldOptional "z" VkDispatchIndirectCommand = 'False -- ' closing tick for hsc2hs
        type FieldOffset "z" VkDispatchIndirectCommand =
             #{offset VkDispatchIndirectCommand, z}
        type FieldIsArray "z" VkDispatchIndirectCommand = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkDispatchIndirectCommand, z}

instance CanReadField "z" VkDispatchIndirectCommand where
        {-# INLINE getField #-}
        getField = vkZ

        {-# INLINE readField #-}
        readField = readVkZ

instance CanWriteField "z" VkDispatchIndirectCommand where
        {-# INLINE writeField #-}
        writeField = writeVkZ

instance Show VkDispatchIndirectCommand where
        showsPrec d x
          = showString "VkDispatchIndirectCommand {" .
              showString "vkX = " .
                showsPrec d (vkX x) .
                  showString ", " .
                    showString "vkY = " .
                      showsPrec d (vkY x) .
                        showString ", " .
                          showString "vkZ = " . showsPrec d (vkZ x) . showChar '}'
