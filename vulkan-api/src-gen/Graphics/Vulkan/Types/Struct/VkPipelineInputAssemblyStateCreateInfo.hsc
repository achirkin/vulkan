#include "vulkan/vulkan.h"

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Graphics.Vulkan.Types.Struct.VkPipelineInputAssemblyStateCreateInfo
       (VkPipelineInputAssemblyStateCreateInfo(..)) where
import           Foreign.Storable                               (Storable (..))
import           GHC.Base                                       (Addr##,
                                                                 ByteArray##,
                                                                 byteArrayContents##,
                                                                 plusAddr##)
import           Graphics.Vulkan.Marshal
import           Graphics.Vulkan.Marshal.Internal
import           Graphics.Vulkan.Types.BaseTypes                (VkBool32)
import           Graphics.Vulkan.Types.Bitmasks                 (VkPipelineInputAssemblyStateCreateFlags)
import           Graphics.Vulkan.Types.Enum.VkPrimitiveTopology (VkPrimitiveTopology)
import           Graphics.Vulkan.Types.Enum.VkStructureType     (VkStructureType)
import           System.IO.Unsafe                               (unsafeDupablePerformIO)

-- | > typedef struct VkPipelineInputAssemblyStateCreateInfo {
--   >     VkStructureType sType;
--   >     const void*            pNext;
--   >     VkPipelineInputAssemblyStateCreateFlags    flags;
--   >     VkPrimitiveTopology    topology;
--   >     VkBool32               primitiveRestartEnable;
--   > } VkPipelineInputAssemblyStateCreateInfo;
--
--   <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkPipelineInputAssemblyStateCreateInfo VkPipelineInputAssemblyStateCreateInfo registry at www.khronos.org>
data VkPipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo## Addr##
                                                                                      ByteArray##

instance Eq VkPipelineInputAssemblyStateCreateInfo where
        (VkPipelineInputAssemblyStateCreateInfo## a _) ==
          x@(VkPipelineInputAssemblyStateCreateInfo## b _)
          = EQ == cmpBytes## (sizeOf x) a b

        {-# INLINE (==) #-}

instance Ord VkPipelineInputAssemblyStateCreateInfo where
        (VkPipelineInputAssemblyStateCreateInfo## a _) `compare`
          x@(VkPipelineInputAssemblyStateCreateInfo## b _)
          = cmpBytes## (sizeOf x) a b

        {-# INLINE compare #-}

instance Storable VkPipelineInputAssemblyStateCreateInfo where
        sizeOf ~_
          = #{size VkPipelineInputAssemblyStateCreateInfo}

        {-# INLINE sizeOf #-}
        alignment ~_
          = #{alignment VkPipelineInputAssemblyStateCreateInfo}

        {-# INLINE alignment #-}
        peek = peekVkData##

        {-# INLINE peek #-}
        poke = pokeVkData##

        {-# INLINE poke #-}

instance VulkanMarshalPrim VkPipelineInputAssemblyStateCreateInfo
         where
        unsafeAddr (VkPipelineInputAssemblyStateCreateInfo## a _) = a

        {-# INLINE unsafeAddr #-}
        unsafeByteArray (VkPipelineInputAssemblyStateCreateInfo## _ b) = b

        {-# INLINE unsafeByteArray #-}
        unsafeFromByteArrayOffset off b
          = VkPipelineInputAssemblyStateCreateInfo##
              (plusAddr## (byteArrayContents## b) off)
              b

        {-# INLINE unsafeFromByteArrayOffset #-}

instance VulkanMarshal VkPipelineInputAssemblyStateCreateInfo where
        type StructFields VkPipelineInputAssemblyStateCreateInfo =
             '["sType", "pNext", "flags", "topology", "primitiveRestartEnable"] -- ' closing tick for hsc2hs
        type CUnionType VkPipelineInputAssemblyStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type ReturnedOnly VkPipelineInputAssemblyStateCreateInfo = 'False -- ' closing tick for hsc2hs
        type StructExtends VkPipelineInputAssemblyStateCreateInfo = '[] -- ' closing tick for hsc2hs

instance {-# OVERLAPPING #-}
         HasField "sType" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "sType" VkPipelineInputAssemblyStateCreateInfo =
             VkStructureType
        type FieldOptional "sType" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "sType" VkPipelineInputAssemblyStateCreateInfo =
             #{offset VkPipelineInputAssemblyStateCreateInfo, sType}
        type FieldIsArray "sType" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanReadField "sType" VkPipelineInputAssemblyStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, sType})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         CanWriteField "sType" VkPipelineInputAssemblyStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, sType}

instance {-# OVERLAPPING #-}
         HasField "pNext" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "pNext" VkPipelineInputAssemblyStateCreateInfo =
             Ptr Void
        type FieldOptional "pNext" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs
        type FieldOffset "pNext" VkPipelineInputAssemblyStateCreateInfo =
             #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}
        type FieldIsArray "pNext" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanReadField "pNext" VkPipelineInputAssemblyStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, pNext})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         CanWriteField "pNext" VkPipelineInputAssemblyStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, pNext}

instance {-# OVERLAPPING #-}
         HasField "flags" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "flags" VkPipelineInputAssemblyStateCreateInfo =
             VkPipelineInputAssemblyStateCreateFlags
        type FieldOptional "flags" VkPipelineInputAssemblyStateCreateInfo =
             'True -- ' closing tick for hsc2hs
        type FieldOffset "flags" VkPipelineInputAssemblyStateCreateInfo =
             #{offset VkPipelineInputAssemblyStateCreateInfo, flags}
        type FieldIsArray "flags" VkPipelineInputAssemblyStateCreateInfo =
             'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = True

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanReadField "flags" VkPipelineInputAssemblyStateCreateInfo where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, flags})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         CanWriteField "flags" VkPipelineInputAssemblyStateCreateInfo where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, flags}

instance {-# OVERLAPPING #-}
         HasField "topology" VkPipelineInputAssemblyStateCreateInfo where
        type FieldType "topology" VkPipelineInputAssemblyStateCreateInfo =
             VkPrimitiveTopology
        type FieldOptional "topology"
               VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "topology" VkPipelineInputAssemblyStateCreateInfo
             =
             #{offset VkPipelineInputAssemblyStateCreateInfo, topology}
        type FieldIsArray "topology" VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, topology}

instance {-# OVERLAPPING #-}
         CanReadField "topology" VkPipelineInputAssemblyStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, topology})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, topology}

instance {-# OVERLAPPING #-}
         CanWriteField "topology" VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, topology}

instance {-# OVERLAPPING #-}
         HasField "primitiveRestartEnable"
           VkPipelineInputAssemblyStateCreateInfo
         where
        type FieldType "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             = VkBool32
        type FieldOptional "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs
        type FieldOffset "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             =
             #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}
        type FieldIsArray "primitiveRestartEnable"
               VkPipelineInputAssemblyStateCreateInfo
             = 'False -- ' closing tick for hsc2hs

        {-# INLINE fieldOptional #-}
        fieldOptional = False

        {-# INLINE fieldOffset #-}
        fieldOffset
          = #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}

instance {-# OVERLAPPING #-}
         CanReadField "primitiveRestartEnable"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# NOINLINE getField #-}
        getField x
          = unsafeDupablePerformIO
              (peekByteOff (unsafePtr x) #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable})

        {-# INLINE readField #-}
        readField p
          = peekByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}

instance {-# OVERLAPPING #-}
         CanWriteField "primitiveRestartEnable"
           VkPipelineInputAssemblyStateCreateInfo
         where
        {-# INLINE writeField #-}
        writeField p
          = pokeByteOff p #{offset VkPipelineInputAssemblyStateCreateInfo, primitiveRestartEnable}

instance Show VkPipelineInputAssemblyStateCreateInfo where
        showsPrec d x
          = showString "VkPipelineInputAssemblyStateCreateInfo {" .
              showString "sType = " .
                showsPrec d (getField @"sType" x) .
                  showString ", " .
                    showString "pNext = " .
                      showsPrec d (getField @"pNext" x) .
                        showString ", " .
                          showString "flags = " .
                            showsPrec d (getField @"flags" x) .
                              showString ", " .
                                showString "topology = " .
                                  showsPrec d (getField @"topology" x) .
                                    showString ", " .
                                      showString "primitiveRestartEnable = " .
                                        showsPrec d (getField @"primitiveRestartEnable" x) .
                                          showChar '}'
