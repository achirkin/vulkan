#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkVertexInputBindingDivisorDescriptionEXT
       (VkVertexInputBindingDivisorDescriptionEXT(..)) where
import           Foreign.Storable                 (Storable (..))
import           GHC.Base                         (Addr##, ByteArray##,
                                                   byteArrayContents##,
                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           System.IO.Unsafe                 (unsafeDupablePerformIO)

-- | > typedef struct VkVertexInputBindingDivisorDescriptionEXT {
--   >     uint32_t          binding;
--   >     uint32_t          divisor;
--   > } VkVertexInputBindingDivisorDescriptionEXT;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkVertexInputBindingDivisorDescriptionEXT VkVertexInputBindingDivisorDescriptionEXT registry at www.khronos.org>
data VkVertexInputBindingDivisorDescriptionEXT = VkVertexInputBindingDivisorDescriptionEXT## Addr##
                                                                                            ByteArray##

instance Eq VkVertexInputBindingDivisorDescriptionEXT where
        (VkVertexInputBindingDivisorDescriptionEXT## a _) ==
          x@(VkVertexInputBindingDivisorDescriptionEXT## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkVertexInputBindingDivisorDescriptionEXT where
        (VkVertexInputBindingDivisorDescriptionEXT## a _) `compare`
          x@(VkVertexInputBindingDivisorDescriptionEXT## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkVertexInputBindingDivisorDescriptionEXT where
        sizeOf ~_
          = #{size VkVertexInputBindingDivisorDescriptionEXT}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkVertexInputBindingDivisorDescriptionEXT}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim
           VkVertexInputBindingDivisorDescriptionEXT
         where
        unsafeAddr (VkVertexInputBindingDivisorDescriptionEXT## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkVertexInputBindingDivisorDescriptionEXT## _ b)
          = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkVertexInputBindingDivisorDescriptionEXT##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkVertexInputBindingDivisorDescriptionEXT
         where
        type StructFields VkVertexInputBindingDivisorDescriptionEXT =
             '["binding", "divisor"] -- ' closing tick for hsc2hs
        type CUnionType VkVertexInputBindingDivisorDescriptionEXT = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkVertexInputBindingDivisorDescriptionEXT =
             'False -- ' closing tick for hsc2hs
        type StructExtends VkVertexInputBindingDivisorDescriptionEXT = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "binding" VkVertexInputBindingDivisorDescriptionEXT where
        type FieldType "binding" VkVertexInputBindingDivisorDescriptionEXT
             = Word32
        type FieldOptional "binding"
               VkVertexInputBindingDivisorDescriptionEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "binding"
               VkVertexInputBindingDivisorDescriptionEXT
             =
             #{offset VkVertexInputBindingDivisorDescriptionEXT, binding}
        type FieldIsArray "binding"
               VkVertexInputBindingDivisorDescriptionEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputBindingDivisorDescriptionEXT, binding}

instance {-# OVERLAPPING #-}
         CanReadField "binding" VkVertexInputBindingDivisorDescriptionEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputBindingDivisorDescriptionEXT, binding})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputBindingDivisorDescriptionEXT, binding}

instance {-# OVERLAPPING #-}
         CanWriteField "binding" VkVertexInputBindingDivisorDescriptionEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputBindingDivisorDescriptionEXT, binding}

instance {-# OVERLAPPING #-}
         HasField "divisor" VkVertexInputBindingDivisorDescriptionEXT where
        type FieldType "divisor" VkVertexInputBindingDivisorDescriptionEXT
             = Word32
        type FieldOptional "divisor"
               VkVertexInputBindingDivisorDescriptionEXT
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "divisor"
               VkVertexInputBindingDivisorDescriptionEXT
             =
             #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor}
        type FieldIsArray "divisor"
               VkVertexInputBindingDivisorDescriptionEXT
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor}

instance {-# OVERLAPPING #-}
         CanReadField "divisor" VkVertexInputBindingDivisorDescriptionEXT
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor}

instance {-# OVERLAPPING #-}
         CanWriteField "divisor" VkVertexInputBindingDivisorDescriptionEXT
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkVertexInputBindingDivisorDescriptionEXT, divisor}

instance Show VkVertexInputBindingDivisorDescriptionEXT where
        showsPrec d x
          = showString "VkVertexInputBindingDivisorDescriptionEXT {" .
              showString "binding = " .
                showsPrec d (getField @"binding" x) .
                  showString ", " .
                    showString "divisor = " .
                      showsPrec d (getField @"divisor" x) . showChar '}'
