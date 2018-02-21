#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineColorBlendAttachmentState
       (VkPipelineColorBlendAttachmentState(..)) where
import           Foreign.Storable                                 (Storable (..))
import           GHC.Prim
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                  (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkBlendFactor         (VkBlendFactor)
import           Graphics.Vulkan.Types.Enum.VkBlendOp             (VkBlendOp)
import           Graphics.Vulkan.Types.Enum.VkColorComponentFlags (VkColorComponentFlags)
import           Graphics.Vulkan.Types.StructMembers
import           System.IO.Unsafe                                 (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineColorBlendAttachmentState {
--   >     VkBool32               blendEnable;
--   >     VkBlendFactor          srcColorBlendFactor;
--   >     VkBlendFactor          dstColorBlendFactor;
--   >     VkBlendOp              colorBlendOp;
--   >     VkBlendFactor          srcAlphaBlendFactor;
--   >     VkBlendFactor          dstAlphaBlendFactor;
--   >     VkBlendOp              alphaBlendOp;
--   >     VkColorComponentFlags  colorWriteMask;
--   > } VkPipelineColorBlendAttachmentState;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/man/html/VkPipelineColorBlendAttachmentState.html VkPipelineColorBlendAttachmentState registry at www.khronos.org>
data VkPipelineColorBlendAttachmentState = VkPipelineColorBlendAttachmentState## Addr##
                                                                                ByteArray##

instance Eq VkPipelineColorBlendAttachmentState where
        (VkPipelineColorBlendAttachmentState## a _) ==
          x@(VkPipelineColorBlendAttachmentState## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineColorBlendAttachmentState where
        (VkPipelineColorBlendAttachmentState## a _) `compare`
          x@(VkPipelineColorBlendAttachmentState## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineColorBlendAttachmentState where
        sizeOf ~_ = #{size VkPipelineColorBlendAttachmentState}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineColorBlendAttachmentState}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineColorBlendAttachmentState
         where
        unsafeAddr (VkPipelineColorBlendAttachmentState## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineColorBlendAttachmentState## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineColorBlendAttachmentState##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineColorBlendAttachmentState where
        type StructFields VkPipelineColorBlendAttachmentState =
             '["blendEnable", "srcColorBlendFactor", "dstColorBlendFactor", -- ' closing tick for hsc2hs
               "colorBlendOp", "srcAlphaBlendFactor", "dstAlphaBlendFactor",
               "alphaBlendOp", "colorWriteMask"]
        type CUnionType VkPipelineColorBlendAttachmentState = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineColorBlendAttachmentState = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineColorBlendAttachmentState = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasVkBlendEnable VkPipelineColorBlendAttachmentState where
        type VkBlendEnableMType VkPipelineColorBlendAttachmentState =
             VkBool32

        {-# NOINLINE vkBlendEnable #-}
        vkBlendEnable x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, blendEnable})

        {-# INLINE vkBlendEnableByteOffset #-}
        vkBlendEnableByteOffset ~_
          = #{offset VkPipelineColorBlendAttachmentState, blendEnable}

        {-# INLINE readVkBlendEnable #-}
        readVkBlendEnable p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, blendEnable}

        {-# INLINE writeVkBlendEnable #-}
        writeVkBlendEnable p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, blendEnable}

instance {-# OVERLAPPING #-}
         HasField "blendEnable" VkPipelineColorBlendAttachmentState where
        type FieldType "blendEnable" VkPipelineColorBlendAttachmentState =
             VkBool32
        type FieldOptional "blendEnable"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "blendEnable" VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, blendEnable}
        type FieldIsArray "blendEnable" VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, blendEnable}

instance CanReadField "blendEnable"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE getField #-}
        getField = vkBlendEnable

        {-# INLINE readField #-}
        readField = readVkBlendEnable

instance CanWriteField "blendEnable"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField = writeVkBlendEnable

instance {-# OVERLAPPING #-}
         HasVkSrcColorBlendFactor VkPipelineColorBlendAttachmentState where
        type VkSrcColorBlendFactorMType VkPipelineColorBlendAttachmentState
             = VkBlendFactor

        {-# NOINLINE vkSrcColorBlendFactor #-}
        vkSrcColorBlendFactor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor})

        {-# INLINE vkSrcColorBlendFactorByteOffset #-}
        vkSrcColorBlendFactorByteOffset ~_
          = #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}

        {-# INLINE readVkSrcColorBlendFactor #-}
        readVkSrcColorBlendFactor p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}

        {-# INLINE writeVkSrcColorBlendFactor #-}
        writeVkSrcColorBlendFactor p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}

instance {-# OVERLAPPING #-}
         HasField "srcColorBlendFactor" VkPipelineColorBlendAttachmentState
         where
        type FieldType "srcColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = VkBlendFactor
        type FieldOptional "srcColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}
        type FieldIsArray "srcColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}

instance CanReadField "srcColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE getField #-}
        getField = vkSrcColorBlendFactor

        {-# INLINE readField #-}
        readField = readVkSrcColorBlendFactor

instance CanWriteField "srcColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField = writeVkSrcColorBlendFactor

instance {-# OVERLAPPING #-}
         HasVkDstColorBlendFactor VkPipelineColorBlendAttachmentState where
        type VkDstColorBlendFactorMType VkPipelineColorBlendAttachmentState
             = VkBlendFactor

        {-# NOINLINE vkDstColorBlendFactor #-}
        vkDstColorBlendFactor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor})

        {-# INLINE vkDstColorBlendFactorByteOffset #-}
        vkDstColorBlendFactorByteOffset ~_
          = #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}

        {-# INLINE readVkDstColorBlendFactor #-}
        readVkDstColorBlendFactor p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}

        {-# INLINE writeVkDstColorBlendFactor #-}
        writeVkDstColorBlendFactor p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}

instance {-# OVERLAPPING #-}
         HasField "dstColorBlendFactor" VkPipelineColorBlendAttachmentState
         where
        type FieldType "dstColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = VkBlendFactor
        type FieldOptional "dstColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}
        type FieldIsArray "dstColorBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}

instance CanReadField "dstColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE getField #-}
        getField = vkDstColorBlendFactor

        {-# INLINE readField #-}
        readField = readVkDstColorBlendFactor

instance CanWriteField "dstColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField = writeVkDstColorBlendFactor

instance {-# OVERLAPPING #-}
         HasVkColorBlendOp VkPipelineColorBlendAttachmentState where
        type VkColorBlendOpMType VkPipelineColorBlendAttachmentState =
             VkBlendOp

        {-# NOINLINE vkColorBlendOp #-}
        vkColorBlendOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, colorBlendOp})

        {-# INLINE vkColorBlendOpByteOffset #-}
        vkColorBlendOpByteOffset ~_
          = #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}

        {-# INLINE readVkColorBlendOp #-}
        readVkColorBlendOp p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}

        {-# INLINE writeVkColorBlendOp #-}
        writeVkColorBlendOp p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}

instance {-# OVERLAPPING #-}
         HasField "colorBlendOp" VkPipelineColorBlendAttachmentState where
        type FieldType "colorBlendOp" VkPipelineColorBlendAttachmentState =
             VkBlendOp
        type FieldOptional "colorBlendOp"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "colorBlendOp" VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}
        type FieldIsArray "colorBlendOp"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}

instance CanReadField "colorBlendOp"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE getField #-}
        getField = vkColorBlendOp

        {-# INLINE readField #-}
        readField = readVkColorBlendOp

instance CanWriteField "colorBlendOp"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField = writeVkColorBlendOp

instance {-# OVERLAPPING #-}
         HasVkSrcAlphaBlendFactor VkPipelineColorBlendAttachmentState where
        type VkSrcAlphaBlendFactorMType VkPipelineColorBlendAttachmentState
             = VkBlendFactor

        {-# NOINLINE vkSrcAlphaBlendFactor #-}
        vkSrcAlphaBlendFactor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor})

        {-# INLINE vkSrcAlphaBlendFactorByteOffset #-}
        vkSrcAlphaBlendFactorByteOffset ~_
          = #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}

        {-# INLINE readVkSrcAlphaBlendFactor #-}
        readVkSrcAlphaBlendFactor p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}

        {-# INLINE writeVkSrcAlphaBlendFactor #-}
        writeVkSrcAlphaBlendFactor p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}

instance {-# OVERLAPPING #-}
         HasField "srcAlphaBlendFactor" VkPipelineColorBlendAttachmentState
         where
        type FieldType "srcAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = VkBlendFactor
        type FieldOptional "srcAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "srcAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}
        type FieldIsArray "srcAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}

instance CanReadField "srcAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE getField #-}
        getField = vkSrcAlphaBlendFactor

        {-# INLINE readField #-}
        readField = readVkSrcAlphaBlendFactor

instance CanWriteField "srcAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField = writeVkSrcAlphaBlendFactor

instance {-# OVERLAPPING #-}
         HasVkDstAlphaBlendFactor VkPipelineColorBlendAttachmentState where
        type VkDstAlphaBlendFactorMType VkPipelineColorBlendAttachmentState
             = VkBlendFactor

        {-# NOINLINE vkDstAlphaBlendFactor #-}
        vkDstAlphaBlendFactor x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor})

        {-# INLINE vkDstAlphaBlendFactorByteOffset #-}
        vkDstAlphaBlendFactorByteOffset ~_
          = #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}

        {-# INLINE readVkDstAlphaBlendFactor #-}
        readVkDstAlphaBlendFactor p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}

        {-# INLINE writeVkDstAlphaBlendFactor #-}
        writeVkDstAlphaBlendFactor p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}

instance {-# OVERLAPPING #-}
         HasField "dstAlphaBlendFactor" VkPipelineColorBlendAttachmentState
         where
        type FieldType "dstAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = VkBlendFactor
        type FieldOptional "dstAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "dstAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}
        type FieldIsArray "dstAlphaBlendFactor"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}

instance CanReadField "dstAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE getField #-}
        getField = vkDstAlphaBlendFactor

        {-# INLINE readField #-}
        readField = readVkDstAlphaBlendFactor

instance CanWriteField "dstAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField = writeVkDstAlphaBlendFactor

instance {-# OVERLAPPING #-}
         HasVkAlphaBlendOp VkPipelineColorBlendAttachmentState where
        type VkAlphaBlendOpMType VkPipelineColorBlendAttachmentState =
             VkBlendOp

        {-# NOINLINE vkAlphaBlendOp #-}
        vkAlphaBlendOp x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp})

        {-# INLINE vkAlphaBlendOpByteOffset #-}
        vkAlphaBlendOpByteOffset ~_
          = #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}

        {-# INLINE readVkAlphaBlendOp #-}
        readVkAlphaBlendOp p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}

        {-# INLINE writeVkAlphaBlendOp #-}
        writeVkAlphaBlendOp p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}

instance {-# OVERLAPPING #-}
         HasField "alphaBlendOp" VkPipelineColorBlendAttachmentState where
        type FieldType "alphaBlendOp" VkPipelineColorBlendAttachmentState =
             VkBlendOp
        type FieldOptional "alphaBlendOp"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "alphaBlendOp" VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}
        type FieldIsArray "alphaBlendOp"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}

instance CanReadField "alphaBlendOp"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE getField #-}
        getField = vkAlphaBlendOp

        {-# INLINE readField #-}
        readField = readVkAlphaBlendOp

instance CanWriteField "alphaBlendOp"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField = writeVkAlphaBlendOp

instance {-# OVERLAPPING #-}
         HasVkColorWriteMask VkPipelineColorBlendAttachmentState where
        type VkColorWriteMaskMType VkPipelineColorBlendAttachmentState =
             VkColorComponentFlags

        {-# NOINLINE vkColorWriteMask #-}
        vkColorWriteMask x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, colorWriteMask})

        {-# INLINE vkColorWriteMaskByteOffset #-}
        vkColorWriteMaskByteOffset ~_
          = #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}

        {-# INLINE readVkColorWriteMask #-}
        readVkColorWriteMask p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}

        {-# INLINE writeVkColorWriteMask #-}
        writeVkColorWriteMask p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}

instance {-# OVERLAPPING #-}
         HasField "colorWriteMask" VkPipelineColorBlendAttachmentState where
        type FieldType "colorWriteMask" VkPipelineColorBlendAttachmentState
             = VkColorComponentFlags
        type FieldOptional "colorWriteMask"
               VkPipelineColorBlendAttachmentState
             = 'True -- ' closing tick for hsc2hs
        type FieldOffset "colorWriteMask"
               VkPipelineColorBlendAttachmentState
             =
             #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}
        type FieldIsArray "colorWriteMask"
               VkPipelineColorBlendAttachmentState
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}

instance CanReadField "colorWriteMask"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE getField #-}
        getField = vkColorWriteMask

        {-# INLINE readField #-}
        readField = readVkColorWriteMask

instance CanWriteField "colorWriteMask"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField = writeVkColorWriteMask

instance Show VkPipelineColorBlendAttachmentState where
        showsPrec d x
          = showString "VkPipelineColorBlendAttachmentState {" .
              showString "vkBlendEnable = " .
                showsPrec d (vkBlendEnable x) .
                  showString ", " .
                    showString "vkSrcColorBlendFactor = " .
                      showsPrec d (vkSrcColorBlendFactor x) .
                        showString ", " .
                          showString "vkDstColorBlendFactor = " .
                            showsPrec d (vkDstColorBlendFactor x) .
                              showString ", " .
                                showString "vkColorBlendOp = " .
                                  showsPrec d (vkColorBlendOp x) .
                                    showString ", " .
                                      showString "vkSrcAlphaBlendFactor = " .
                                        showsPrec d (vkSrcAlphaBlendFactor x) .
                                          showString ", " .
                                            showString "vkDstAlphaBlendFactor = " .
                                              showsPrec d (vkDstAlphaBlendFactor x) .
                                                showString ", " .
                                                  showString "vkAlphaBlendOp = " .
                                                    showsPrec d (vkAlphaBlendOp x) .
                                                      showString ", " .
                                                        showString "vkColorWriteMask = " .
                                                          showsPrec d (vkColorWriteMask x) .
                                                            showChar '}'
