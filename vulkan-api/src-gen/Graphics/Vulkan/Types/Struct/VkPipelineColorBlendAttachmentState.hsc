#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineColorBlendAttachmentState
       (VkPipelineColorBlendAttachmentState(..)) where
import           Foreign.Storable                                 (Storable (..))
import           GHC.Base                                         (Addr##,
                                                                   ByteArray##,
                                                                   byteArrayContents##,
                                                                   plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                  (VkBool32)
import           Graphics.Vulkan.Types.Enum.VkBlendFactor         (VkBlendFactor)
import           Graphics.Vulkan.Types.Enum.VkBlendOp             (VkBlendOp)
import           Graphics.Vulkan.Types.Enum.VkColorComponentFlags (VkColorComponentFlags)
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
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPipelineColorBlendAttachmentStateVkPipelineColorBlendAttachmentState registry at www.khronos.org>
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

instance {-# OVERLAPPING #-}
         CanReadField "blendEnable" VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, blendEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, blendEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "blendEnable" VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, blendEnable}

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

instance {-# OVERLAPPING #-}
         CanReadField "srcColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "srcColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, srcColorBlendFactor}

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

instance {-# OVERLAPPING #-}
         CanReadField "dstColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "dstColorBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, dstColorBlendFactor}

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

instance {-# OVERLAPPING #-}
         CanReadField "colorBlendOp" VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, colorBlendOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}

instance {-# OVERLAPPING #-}
         CanWriteField "colorBlendOp" VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, colorBlendOp}

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

instance {-# OVERLAPPING #-}
         CanReadField "srcAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "srcAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, srcAlphaBlendFactor}

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

instance {-# OVERLAPPING #-}
         CanReadField "dstAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}

instance {-# OVERLAPPING #-}
         CanWriteField "dstAlphaBlendFactor"
           VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, dstAlphaBlendFactor}

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

instance {-# OVERLAPPING #-}
         CanReadField "alphaBlendOp" VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}

instance {-# OVERLAPPING #-}
         CanWriteField "alphaBlendOp" VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, alphaBlendOp}

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

instance {-# OVERLAPPING #-}
         CanReadField "colorWriteMask" VkPipelineColorBlendAttachmentState
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineColorBlendAttachmentState, colorWriteMask})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}

instance {-# OVERLAPPING #-}
         CanWriteField "colorWriteMask" VkPipelineColorBlendAttachmentState
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineColorBlendAttachmentState, colorWriteMask}

instance Show VkPipelineColorBlendAttachmentState where
        showsPrec d x
          = showString "VkPipelineColorBlendAttachmentState {" .
              showString "blendEnable = " .
                showsPrec d (getField @"blendEnable" x) .
                  showString ", " .
                    showString "srcColorBlendFactor = " .
                      showsPrec d (getField @"srcColorBlendFactor" x) .
                        showString ", " .
                          showString "dstColorBlendFactor = " .
                            showsPrec d (getField @"dstColorBlendFactor" x) .
                              showString ", " .
                                showString "colorBlendOp = " .
                                  showsPrec d (getField @"colorBlendOp" x) .
                                    showString ", " .
                                      showString "srcAlphaBlendFactor = " .
                                        showsPrec d (getField @"srcAlphaBlendFactor" x) .
                                          showString ", " .
                                            showString "dstAlphaBlendFactor = " .
                                              showsPrec d (getField @"dstAlphaBlendFactor" x) .
                                                showString ", " .
                                                  showString "alphaBlendOp = " .
                                                    showsPrec d (getField @"alphaBlendOp" x) .
                                                      showString ", " .
                                                        showString "colorWriteMask = " .
                                                          showsPrec d (getField @"colorWriteMask" x)
                                                            . showChar '}'
