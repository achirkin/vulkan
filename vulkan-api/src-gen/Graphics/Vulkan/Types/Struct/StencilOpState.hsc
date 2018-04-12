#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.StencilOpState
       (VkStencilOpState(..)) where
import           Foreign.Storable                     (Storable (..))
import           GHC.Base                             (Addr##, ByteArray##,
                                                       byteArrayContents##,
                                                       plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.CompareOp (VkCompareOp)
import           Graphics.Vulkan.Types.Enum.Stencil   (VkStencilOp)
import           System.IO.Unsafe                     (unsafeDupablePerformIO)

-- | > typedef struct VkStencilOpState {
--   >     VkStencilOp            failOp;
--   >     VkStencilOp            passOp;
--   >     VkStencilOp            depthFailOp;
--   >     VkCompareOp            compareOp;
--   >     uint32_t               compareMask;
--   >     uint32_t               writeMask;
--   >     uint32_t               reference;
--   > } VkStencilOpState;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkStencilOpState VkStencilOpState registry at www.khronos.org>
data VkStencilOpState = VkStencilOpState## Addr## ByteArray##

instance Eq VkStencilOpState where
        (VkStencilOpState## a _) == x@(VkStencilOpState## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkStencilOpState where
        (VkStencilOpState## a _) `compare` x@(VkStencilOpState## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkStencilOpState where
        sizeOf ~_ = #{size VkStencilOpState}

        {-# INLINE sizeOf #-}
        alignment ~_ = #{alignment VkStencilOpState}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkStencilOpState where
        unsafeAddr (VkStencilOpState## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkStencilOpState## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkStencilOpState## (plusAddr## (byteArrayContents## b) off) b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkStencilOpState where
        type StructFields VkStencilOpState =
             '["failOp", "passOp", "depthFailOp", "compareOp", "compareMask", -- ' closing tick for hsc2hs
               "writeMask", "reference"]
        type CUnionType VkStencilOpState = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkStencilOpState = 'False -- ' closing tick for hsc2hs
        type StructExtends VkStencilOpState = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-} HasField "failOp" VkStencilOpState
         where
        type FieldType "failOp" VkStencilOpState = VkStencilOp
        type FieldOptional "failOp" VkStencilOpState = 'False -- ' closing tick for hsc2hs
        type FieldOffset "failOp" VkStencilOpState =
             #{offset VkStencilOpState, failOp}
        type FieldIsArray "failOp" VkStencilOpState = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkStencilOpState, failOp}

instance {-# OVERLAPPING #-} CanReadField "failOp" VkStencilOpState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, failOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkStencilOpState, failOp}

instance {-# OVERLAPPING #-}
         CanWriteField "failOp" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkStencilOpState, failOp}

instance {-# OVERLAPPING #-} HasField "passOp" VkStencilOpState
         where
        type FieldType "passOp" VkStencilOpState = VkStencilOp
        type FieldOptional "passOp" VkStencilOpState = 'False -- ' closing tick for hsc2hs
        type FieldOffset "passOp" VkStencilOpState =
             #{offset VkStencilOpState, passOp}
        type FieldIsArray "passOp" VkStencilOpState = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkStencilOpState, passOp}

instance {-# OVERLAPPING #-} CanReadField "passOp" VkStencilOpState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, passOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkStencilOpState, passOp}

instance {-# OVERLAPPING #-}
         CanWriteField "passOp" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkStencilOpState, passOp}

instance {-# OVERLAPPING #-}
         HasField "depthFailOp" VkStencilOpState where
        type FieldType "depthFailOp" VkStencilOpState = VkStencilOp
        type FieldOptional "depthFailOp" VkStencilOpState = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthFailOp" VkStencilOpState =
             #{offset VkStencilOpState, depthFailOp}
        type FieldIsArray "depthFailOp" VkStencilOpState = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkStencilOpState, depthFailOp}

instance {-# OVERLAPPING #-}
         CanReadField "depthFailOp" VkStencilOpState where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, depthFailOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkStencilOpState, depthFailOp}

instance {-# OVERLAPPING #-}
         CanWriteField "depthFailOp" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkStencilOpState, depthFailOp}

instance {-# OVERLAPPING #-} HasField "compareOp" VkStencilOpState
         where
        type FieldType "compareOp" VkStencilOpState = VkCompareOp
        type FieldOptional "compareOp" VkStencilOpState = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compareOp" VkStencilOpState =
             #{offset VkStencilOpState, compareOp}
        type FieldIsArray "compareOp" VkStencilOpState = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkStencilOpState, compareOp}

instance {-# OVERLAPPING #-}
         CanReadField "compareOp" VkStencilOpState where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, compareOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkStencilOpState, compareOp}

instance {-# OVERLAPPING #-}
         CanWriteField "compareOp" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkStencilOpState, compareOp}

instance {-# OVERLAPPING #-}
         HasField "compareMask" VkStencilOpState where
        type FieldType "compareMask" VkStencilOpState = Word32
        type FieldOptional "compareMask" VkStencilOpState = 'False -- ' closing tick for hsc2hs
        type FieldOffset "compareMask" VkStencilOpState =
             #{offset VkStencilOpState, compareMask}
        type FieldIsArray "compareMask" VkStencilOpState = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkStencilOpState, compareMask}

instance {-# OVERLAPPING #-}
         CanReadField "compareMask" VkStencilOpState where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, compareMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkStencilOpState, compareMask}

instance {-# OVERLAPPING #-}
         CanWriteField "compareMask" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkStencilOpState, compareMask}

instance {-# OVERLAPPING #-} HasField "writeMask" VkStencilOpState
         where
        type FieldType "writeMask" VkStencilOpState = Word32
        type FieldOptional "writeMask" VkStencilOpState = 'False -- ' closing tick for hsc2hs
        type FieldOffset "writeMask" VkStencilOpState =
             #{offset VkStencilOpState, writeMask}
        type FieldIsArray "writeMask" VkStencilOpState = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkStencilOpState, writeMask}

instance {-# OVERLAPPING #-}
         CanReadField "writeMask" VkStencilOpState where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, writeMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkStencilOpState, writeMask}

instance {-# OVERLAPPING #-}
         CanWriteField "writeMask" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkStencilOpState, writeMask}

instance {-# OVERLAPPING #-} HasField "reference" VkStencilOpState
         where
        type FieldType "reference" VkStencilOpState = Word32
        type FieldOptional "reference" VkStencilOpState = 'False -- ' closing tick for hsc2hs
        type FieldOffset "reference" VkStencilOpState =
             #{offset VkStencilOpState, reference}
        type FieldIsArray "reference" VkStencilOpState = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset = #{offset VkStencilOpState, reference}

instance {-# OVERLAPPING #-}
         CanReadField "reference" VkStencilOpState where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, reference})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkStencilOpState, reference}

instance {-# OVERLAPPING #-}
         CanWriteField "reference" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkStencilOpState, reference}

instance Show VkStencilOpState where
        showsPrec d x
          = showString "VkStencilOpState {" .
              showString "failOp = " .
                showsPrec d (getField @"failOp" x) .
                  showString ", " .
                    showString "passOp = " .
                      showsPrec d (getField @"passOp" x) .
                        showString ", " .
                          showString "depthFailOp = " .
                            showsPrec d (getField @"depthFailOp" x) .
                              showString ", " .
                                showString "compareOp = " .
                                  showsPrec d (getField @"compareOp" x) .
                                    showString ", " .
                                      showString "compareMask = " .
                                        showsPrec d (getField @"compareMask" x) .
                                          showString ", " .
                                            showString "writeMask = " .
                                              showsPrec d (getField @"writeMask" x) .
                                                showString ", " .
                                                  showString "reference = " .
                                                    showsPrec d (getField @"reference" x) .
                                                      showChar '}'
