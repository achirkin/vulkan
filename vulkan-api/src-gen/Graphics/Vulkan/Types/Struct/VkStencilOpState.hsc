#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkStencilOpState
       (VkStencilOpState(..)) where
import           Foreign.Storable                       (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.Enum.VkCompareOp (VkCompareOp)
import           Graphics.Vulkan.Types.Enum.VkStencilOp (VkStencilOp)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                       (unsafeDupablePerformIO)

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
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkStencilOpState.html VkStencilOpState registry at www.khronos.org>
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

instance {-# OVERLAPPING #-} HasVkFailOp VkStencilOpState where
        type VkFailOpMType VkStencilOpState = VkStencilOp

        {-# NOINLINE vkFailOp #-}
        vkFailOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, failOp})

        {-# INLINE vkFailOpByteOffset #-}
        vkFailOpByteOffset ~_
          = #{offset VkStencilOpState, failOp}

        {-# INLINE readVkFailOp #-}
        readVkFailOp p
          = peekByteOff p #{offset VkStencilOpState, failOp}

        {-# INLINE writeVkFailOp #-}
        writeVkFailOp p
          = pokeByteOff p #{offset VkStencilOpState, failOp}

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

instance CanReadField "failOp" VkStencilOpState where
        {-# INLINE getField #-}
        getField = vkFailOp

        {-# INLINE readField #-}
        readField = readVkFailOp

instance CanWriteField "failOp" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField = writeVkFailOp

instance {-# OVERLAPPING #-} HasVkPassOp VkStencilOpState where
        type VkPassOpMType VkStencilOpState = VkStencilOp

        {-# NOINLINE vkPassOp #-}
        vkPassOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, passOp})

        {-# INLINE vkPassOpByteOffset #-}
        vkPassOpByteOffset ~_
          = #{offset VkStencilOpState, passOp}

        {-# INLINE readVkPassOp #-}
        readVkPassOp p
          = peekByteOff p #{offset VkStencilOpState, passOp}

        {-# INLINE writeVkPassOp #-}
        writeVkPassOp p
          = pokeByteOff p #{offset VkStencilOpState, passOp}

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

instance CanReadField "passOp" VkStencilOpState where
        {-# INLINE getField #-}
        getField = vkPassOp

        {-# INLINE readField #-}
        readField = readVkPassOp

instance CanWriteField "passOp" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField = writeVkPassOp

instance {-# OVERLAPPING #-} HasVkDepthFailOp VkStencilOpState
         where
        type VkDepthFailOpMType VkStencilOpState = VkStencilOp

        {-# NOINLINE vkDepthFailOp #-}
        vkDepthFailOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, depthFailOp})

        {-# INLINE vkDepthFailOpByteOffset #-}
        vkDepthFailOpByteOffset ~_
          = #{offset VkStencilOpState, depthFailOp}

        {-# INLINE readVkDepthFailOp #-}
        readVkDepthFailOp p
          = peekByteOff p #{offset VkStencilOpState, depthFailOp}

        {-# INLINE writeVkDepthFailOp #-}
        writeVkDepthFailOp p
          = pokeByteOff p #{offset VkStencilOpState, depthFailOp}

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

instance CanReadField "depthFailOp" VkStencilOpState where
        {-# INLINE getField #-}
        getField = vkDepthFailOp

        {-# INLINE readField #-}
        readField = readVkDepthFailOp

instance CanWriteField "depthFailOp" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField = writeVkDepthFailOp

instance {-# OVERLAPPING #-} HasVkCompareOp VkStencilOpState where
        type VkCompareOpMType VkStencilOpState = VkCompareOp

        {-# NOINLINE vkCompareOp #-}
        vkCompareOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, compareOp})

        {-# INLINE vkCompareOpByteOffset #-}
        vkCompareOpByteOffset ~_
          = #{offset VkStencilOpState, compareOp}

        {-# INLINE readVkCompareOp #-}
        readVkCompareOp p
          = peekByteOff p #{offset VkStencilOpState, compareOp}

        {-# INLINE writeVkCompareOp #-}
        writeVkCompareOp p
          = pokeByteOff p #{offset VkStencilOpState, compareOp}

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

instance CanReadField "compareOp" VkStencilOpState where
        {-# INLINE getField #-}
        getField = vkCompareOp

        {-# INLINE readField #-}
        readField = readVkCompareOp

instance CanWriteField "compareOp" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField = writeVkCompareOp

instance {-# OVERLAPPING #-} HasVkCompareMask VkStencilOpState
         where
        type VkCompareMaskMType VkStencilOpState = Word32

        {-# NOINLINE vkCompareMask #-}
        vkCompareMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, compareMask})

        {-# INLINE vkCompareMaskByteOffset #-}
        vkCompareMaskByteOffset ~_
          = #{offset VkStencilOpState, compareMask}

        {-# INLINE readVkCompareMask #-}
        readVkCompareMask p
          = peekByteOff p #{offset VkStencilOpState, compareMask}

        {-# INLINE writeVkCompareMask #-}
        writeVkCompareMask p
          = pokeByteOff p #{offset VkStencilOpState, compareMask}

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

instance CanReadField "compareMask" VkStencilOpState where
        {-# INLINE getField #-}
        getField = vkCompareMask

        {-# INLINE readField #-}
        readField = readVkCompareMask

instance CanWriteField "compareMask" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField = writeVkCompareMask

instance {-# OVERLAPPING #-} HasVkWriteMask VkStencilOpState where
        type VkWriteMaskMType VkStencilOpState = Word32

        {-# NOINLINE vkWriteMask #-}
        vkWriteMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, writeMask})

        {-# INLINE vkWriteMaskByteOffset #-}
        vkWriteMaskByteOffset ~_
          = #{offset VkStencilOpState, writeMask}

        {-# INLINE readVkWriteMask #-}
        readVkWriteMask p
          = peekByteOff p #{offset VkStencilOpState, writeMask}

        {-# INLINE writeVkWriteMask #-}
        writeVkWriteMask p
          = pokeByteOff p #{offset VkStencilOpState, writeMask}

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

instance CanReadField "writeMask" VkStencilOpState where
        {-# INLINE getField #-}
        getField = vkWriteMask

        {-# INLINE readField #-}
        readField = readVkWriteMask

instance CanWriteField "writeMask" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField = writeVkWriteMask

instance {-# OVERLAPPING #-} HasVkReference VkStencilOpState where
        type VkReferenceMType VkStencilOpState = Word32

        {-# NOINLINE vkReference #-}
        vkReference x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkStencilOpState, reference})

        {-# INLINE vkReferenceByteOffset #-}
        vkReferenceByteOffset ~_
          = #{offset VkStencilOpState, reference}

        {-# INLINE readVkReference #-}
        readVkReference p
          = peekByteOff p #{offset VkStencilOpState, reference}

        {-# INLINE writeVkReference #-}
        writeVkReference p
          = pokeByteOff p #{offset VkStencilOpState, reference}

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

instance CanReadField "reference" VkStencilOpState where
        {-# INLINE getField #-}
        getField = vkReference

        {-# INLINE readField #-}
        readField = readVkReference

instance CanWriteField "reference" VkStencilOpState where
        {-# INLINE writeField #-}
        writeField = writeVkReference

instance Show VkStencilOpState where
        showsPrec d x
          = showString "VkStencilOpState {" .
              showString "vkFailOp = " .
                showsPrec d (vkFailOp x) .
                  showString ", " .
                    showString "vkPassOp = " .
                      showsPrec d (vkPassOp x) .
                        showString ", " .
                          showString "vkDepthFailOp = " .
                            showsPrec d (vkDepthFailOp x) .
                              showString ", " .
                                showString "vkCompareOp = " .
                                  showsPrec d (vkCompareOp x) .
                                    showString ", " .
                                      showString "vkCompareMask = " .
                                        showsPrec d (vkCompareMask x) .
                                          showString ", " .
                                            showString "vkWriteMask = " .
                                              showsPrec d (vkWriteMask x) .
                                                showString ", " .
                                                  showString "vkReference = " .
                                                    showsPrec d (vkReference x) . showChar '}'
