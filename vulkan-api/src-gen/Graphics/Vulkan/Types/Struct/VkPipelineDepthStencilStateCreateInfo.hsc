#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineDepthStencilStateCreateInfo
       (VkPipelineDepthStencilStateCreateInfo(..)) where
import           Foreign.Storable                              (Storable (..))
import           GHC.Base                                      (Addr##,
                                                                ByteArray##,
                                                                byteArrayContents##,
                                                                plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes               (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks                (VkPipelineDepthStencilStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkCompareOp        (VkCompareOp)
import           Graphics.Vulkan.Types.Enum.VkStructureType    (VkStructureType)
import           Graphics.Vulkan.Types.Struct.VkStencilOpState (VkStencilOpState)
import           System.IO.Unsafe                              (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineDepthStencilStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineDepthStencilStateCreateFlags    flags;
--   >     VkBool32               depthTestEnable;
--   >     VkBool32               depthWriteEnable;
--   >     VkCompareOp            depthCompareOp;
--   >     VkBool32               depthBoundsTestEnable;
--   >     VkBool32               stencilTestEnable;
--   >     VkStencilOpState       front;
--   >     VkStencilOpState       back;
--   >     float                  minDepthBounds;
--   >     float                  maxDepthBounds;
--   > } VkPipelineDepthStencilStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html##VkPipelineDepthStencilStateCreateInfo VkPipelineDepthStencilStateCreateInfo registry at www.khronos.org>
data VkPipelineDepthStencilStateCreateInfo = VkPipelineDepthStencilStateCreateInfo## Addr##
                                                                                    ByteArray##

instance Eq VkPipelineDepthStencilStateCreateInfo where
        (VkPipelineDepthStencilStateCreateInfo## a _) ==
          x@(VkPipelineDepthStencilStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineDepthStencilStateCreateInfo where
        (VkPipelineDepthStencilStateCreateInfo## a _) `compare`
          x@(VkPipelineDepthStencilStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineDepthStencilStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineDepthStencilStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineDepthStencilStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineDepthStencilStateCreateInfo
         where
        unsafeAddr (VkPipelineDepthStencilStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineDepthStencilStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineDepthStencilStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineDepthStencilStateCreateInfo where
        type StructFields VkPipelineDepthStencilStateCreateInfo =
             '["sType", "pNext", "flags", "depthTestEnable", "depthWriteEnable", -- ' closing tick for hsc2hs
               "depthCompareOp", "depthBoundsTestEnable", "stencilTestEnable",
               "front", "back", "minDepthBounds", "maxDepthBounds"]
        type CUnionType VkPipelineDepthStencilStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineDepthStencilStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineDepthStencilStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "sType" VkPipelineDepthStencilStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineDepthStencilStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineDepthStencilStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "pNext" VkPipelineDepthStencilStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineDepthStencilStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineDepthStencilStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "flags" VkPipelineDepthStencilStateCreateInfo =
             VkPipelineDepthStencilStateCreateFlags
        type FieldOptional "flags" VkPipelineDepthStencilStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineDepthStencilStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineDepthStencilStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "depthTestEnable" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}
        type FieldIsArray "depthTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}

instance {-# OVERLAPPING #-}
         CanReadField "depthTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "depthTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthTestEnable}

instance {-# OVERLAPPING #-}
         HasField "depthWriteEnable" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}
        type FieldIsArray "depthWriteEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}

instance {-# OVERLAPPING #-}
         CanReadField "depthWriteEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "depthWriteEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthWriteEnable}

instance {-# OVERLAPPING #-}
         HasField "depthCompareOp" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             = VkCompareOp
        type FieldOptional "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}
        type FieldIsArray "depthCompareOp"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}

instance {-# OVERLAPPING #-}
         CanReadField "depthCompareOp" VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}

instance {-# OVERLAPPING #-}
         CanWriteField "depthCompareOp"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthCompareOp}

instance {-# OVERLAPPING #-}
         HasField "depthBoundsTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}
        type FieldIsArray "depthBoundsTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}

instance {-# OVERLAPPING #-}
         CanReadField "depthBoundsTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "depthBoundsTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, depthBoundsTestEnable}

instance {-# OVERLAPPING #-}
         HasField "stencilTestEnable" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = VkBool32
        type FieldOptional "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}
        type FieldIsArray "stencilTestEnable"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}

instance {-# OVERLAPPING #-}
         CanReadField "stencilTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "stencilTestEnable"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, stencilTestEnable}

instance {-# OVERLAPPING #-}
         HasField "front" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "front" VkPipelineDepthStencilStateCreateInfo =
             VkStencilOpState
        type FieldOptional "front" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "front" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, front}
        type FieldIsArray "front" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, front}

instance {-# OVERLAPPING #-}
         CanReadField "front" VkPipelineDepthStencilStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, front})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, front}

instance {-# OVERLAPPING #-}
         CanWriteField "front" VkPipelineDepthStencilStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, front}

instance {-# OVERLAPPING #-}
         HasField "back" VkPipelineDepthStencilStateCreateInfo where
        type FieldType "back" VkPipelineDepthStencilStateCreateInfo =
             VkStencilOpState
        type FieldOptional "back" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "back" VkPipelineDepthStencilStateCreateInfo =
             #{offset VkPipelineDepthStencilStateCreateInfo, back}
        type FieldIsArray "back" VkPipelineDepthStencilStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, back}

instance {-# OVERLAPPING #-}
         CanReadField "back" VkPipelineDepthStencilStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, back})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, back}

instance {-# OVERLAPPING #-}
         CanWriteField "back" VkPipelineDepthStencilStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, back}

instance {-# OVERLAPPING #-}
         HasField "minDepthBounds" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = #{type float}
        type FieldOptional "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}
        type FieldIsArray "minDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}

instance {-# OVERLAPPING #-}
         CanReadField "minDepthBounds" VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}

instance {-# OVERLAPPING #-}
         CanWriteField "minDepthBounds"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, minDepthBounds}

instance {-# OVERLAPPING #-}
         HasField "maxDepthBounds" VkPipelineDepthStencilStateCreateInfo
         where
        type FieldType "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = #{type float}
        type FieldOptional "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             =
             #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}
        type FieldIsArray "maxDepthBounds"
               VkPipelineDepthStencilStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}

instance {-# OVERLAPPING #-}
         CanReadField "maxDepthBounds" VkPipelineDepthStencilStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}

instance {-# OVERLAPPING #-}
         CanWriteField "maxDepthBounds"
           VkPipelineDepthStencilStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineDepthStencilStateCreateInfo, maxDepthBounds}

instance Show VkPipelineDepthStencilStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineDepthStencilStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "depthTestEnable = " .
                                  showsPrec d (getField @"depthTestEnable" x) .
                                    showString ", " .
                                      showString "depthWriteEnable = " .
                                        showsPrec d (getField @"depthWriteEnable" x) .
                                          showString ", " .
                                            showString "depthCompareOp = " .
                                              showsPrec d (getField @"depthCompareOp" x) .
                                                showString ", " .
                                                  showString "depthBoundsTestEnable = " .
                                                    showsPrec d
                                                      (getField @"depthBoundsTestEnable" x)
                                                      .
                                                      showString ", " .
                                                        showString "stencilTestEnable = " .
                                                          showsPrec d
                                                            (getField @"stencilTestEnable" x)
                                                            .
                                                            showString ", " .
                                                              showString "front = " .
                                                                showsPrec d (getField @"front" x) .
                                                                  showString ", " .
                                                                    showString "back = " .
                                                                      showsPrec d
                                                                        (getField @"back" x)
                                                                        .
                                                                        showString ", " .
                                                                          showString
                                                                            "minDepthBounds = "
                                                                            .
                                                                            showsPrec d
                                                                              (getField
                                                                                 @"minDepthBounds"
                                                                                 x)
                                                                              .
                                                                              showString ", " .
                                                                                showString
                                                                                  "maxDepthBounds = "
                                                                                  .
                                                                                  showsPrec d
                                                                                    (getField
                                                                                       @"maxDepthBounds"
                                                                                       x)
                                                                                    . showChar '}'
