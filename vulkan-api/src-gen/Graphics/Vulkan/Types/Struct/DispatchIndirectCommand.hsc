#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.DispatchIndirectCommand
       (VkDispatchIndirectCommand(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkDispatchIndirectCommand {
--   >     uint32_t               x;
--   >     uint32_t               y;
--   >     uint32_t               z;
--   > } VkDispatchIndirectCommand;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkDispatchIndirectCommand VkDispatchIndirectCommand registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "x" VkDispatchIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDispatchIndirectCommand, x})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDispatchIndirectCommand, x}

instance {-# OVERLAPPING #-}
         CanWriteField "x" VkDispatchIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDispatchIndirectCommand, x}

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

instance {-# OVERLAPPING #-}
         CanReadField "y" VkDispatchIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDispatchIndirectCommand, y})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDispatchIndirectCommand, y}

instance {-# OVERLAPPING #-}
         CanWriteField "y" VkDispatchIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDispatchIndirectCommand, y}

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

instance {-# OVERLAPPING #-}
         CanReadField "z" VkDispatchIndirectCommand where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkDispatchIndirectCommand, z})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkDispatchIndirectCommand, z}

instance {-# OVERLAPPING #-}
         CanWriteField "z" VkDispatchIndirectCommand where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkDispatchIndirectCommand, z}

instance Show VkDispatchIndirectCommand where
        showsPrec d x
          = showString "VkDispatchIndirectCommand {" .
              showString "x = " .
                showsPrec d (getField @"x" x) .
                  showString ", " .
                    showString "y = " .
                      showsPrec d (getField @"y" x) .
                        showString ", " .
                          showString "z = " . showsPrec d (getField @"z" x) . showChar '}'
